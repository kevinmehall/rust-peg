#![feature(plugin_registrar, quote, rustc_private, box_patterns)]

extern crate rustc_plugin;
extern crate syntax;

use syntax::ast;
use syntax::codemap;
use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
use syntax::parse;
use syntax::parse::token;
use syntax::fold::Folder;
use syntax::util::small_vector::SmallVector;
use rustc_plugin::Registry;
use std::io::Read;
use std::fs::File;
use std::path::Path;

use rustast::{AstBuilder, DUMMY_SP};

mod translate;
mod grammar;
mod rustast;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
            token::intern("peg"),
            syntax::ext::base::IdentTT(Box::new(expand_peg_str), None, false));

    reg.register_syntax_extension(
            token::intern("peg_file"),
            syntax::ext::base::IdentTT(Box::new(expand_peg_file), None, false));
}

fn expand_peg_str<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 's> {
    let source = match parse_arg(cx, &tts) {
        Some(source) => source,
        None => return DummyResult::any(sp),
    };

    expand_peg(cx, sp, ident, &source)
}

fn expand_peg_file<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 's> {
    let fname = match parse_arg(cx, &tts) {
        Some(fname) => fname,
        None => return DummyResult::any(sp),
    };

    let path = Path::new(&cx.codemap().span_to_filename(sp)).parent().unwrap().join(&fname);

    let mut source = String::new();
    if let Err(e) = File::open(&path).map(|mut f| f.read_to_string(&mut source)) {
        cx.span_err(sp, &e.to_string());
        return DummyResult::any(sp);
    }

    cx.codemap().new_filemap(format!("{}", path.display()), "".to_string());

    expand_peg(cx, sp, ident, &source)
}

fn expand_peg(cx: &mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, source: &str) -> Box<MacResult + 'static> {
    let grammar_def = grammar::grammar(source);

    let grammar_def = match grammar_def {
      Ok(grammar_def) => grammar_def,
      Err(msg) => {
        cx.span_err(sp, &format!("{}", msg));
        return DummyResult::any(sp)
      }
    };

    let ast = translate::compile_grammar(cx, &grammar_def);

    // #![allow(non_snake_case, unused)]
    let allow = cx.attribute(DUMMY_SP, cx.meta_list(DUMMY_SP, token::InternedString::new("allow"), vec![
        cx.meta_word(DUMMY_SP, token::InternedString::new("non_snake_case")),
        cx.meta_word(DUMMY_SP, token::InternedString::new("unused")),
    ]));

    MacEager::items(SmallVector::one(cx.item_mod(sp, sp, ident, vec![allow], ast.items.clone())))
}

fn parse_arg(cx: &mut ExtCtxt, tts: &[ast::TokenTree]) -> Option<String> {
    use syntax::print::pprust;

    let mut parser = parse::new_parser_from_tts(cx.parse_sess(), cx.cfg(),
                                                tts.to_vec());
    // The `expand_expr` method is called so that any macro calls in the
    // parsed expression are expanded.
    let arg = cx.expander().fold_expr(parser.parse_expr().unwrap_or_else(|e| panic!(e)));
    match arg.node {
        ast::ExprLit(ref spanned) => {
            match spanned.node {
                ast::LitStr(ref n, _) => {
                    if !parser.eat(&token::Eof).unwrap_or_else(|e| panic!(e)) {
                        cx.span_err(parser.span,
                                    "expected only one string literal");
                        return None
                    }
                    return Some(n.to_string())
                }
                _ => {}
            }
        }
        _ => {}
    }

    let err = format!("expected string literal but got `{}`",
                      pprust::expr_to_string(&*arg));
    cx.span_err(parser.span, &err);
    None
}
