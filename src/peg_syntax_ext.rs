#![crate_type = "dylib"]
#![feature(plugin_registrar, quote, box_syntax)]
#![allow(unstable)]

extern crate rustc;
extern crate syntax;

use syntax::ast;
use syntax::codemap;
use syntax::ext::base::{ExtCtxt, MacResult, MacItems, DummyResult};
use syntax::parse;
use syntax::parse::token;
use syntax::fold::Folder;
use rustc::plugin::Registry;
use std::io::fs::File;
use std::str;

use rustast::{AstBuilder, DUMMY_SP};

mod translate;
mod grammar;
mod rustast;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
            token::intern("peg"),
            syntax::ext::base::IdentTT(box expand_peg_str, None));

    reg.register_syntax_extension(
            token::intern("peg_file"),
            syntax::ext::base::IdentTT(box expand_peg_file, None));
}

fn expand_peg_str<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 's> {
    let source = match parse_arg(cx, tts.as_slice()) {
        Some(source) => source,
        None => return DummyResult::any(sp),
    };

    expand_peg(cx, sp, ident, source.as_slice())
}

fn expand_peg_file<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult + 's> {
    let fname = match parse_arg(cx, tts.as_slice()) {
        Some(fname) => fname,
        None => return DummyResult::any(sp),
    };

    let path = Path::new(cx.codemap().span_to_filename(sp)).dir_path().join(fname);

    let source_utf8 = match File::open(&path).read_to_end() {
      Ok(source_utf8) => source_utf8,
      Err(e) => {
        cx.span_err(sp, e.to_string().as_slice());
        return DummyResult::any(sp)
      }
    };

    let source = str::from_utf8(source_utf8.as_slice()).unwrap();

    cx.codemap().new_filemap(path.as_str().unwrap().to_string(), "".to_string());

    expand_peg(cx, sp, ident, source.as_slice())
}

fn expand_peg(cx: &mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, source: &str) -> Box<MacResult + 'static> {
    let grammar_def = grammar::grammar(source);

    let grammar_def = match grammar_def {
      Ok(grammar_def) => grammar_def,
      Err(msg) => {
        cx.span_err(sp, msg.as_slice());
        return DummyResult::any(sp)
      }
    };

    let ast = translate::compile_grammar(cx, &grammar_def);

    // #![allow(non_snake_case_functions, unused_variable)]
    let attr = cx.attribute(DUMMY_SP, cx.meta_list(DUMMY_SP, token::InternedString::new("allow"), vec!(
        cx.meta_word(DUMMY_SP, token::InternedString::new("non_snake_case")),
        cx.meta_word(DUMMY_SP, token::InternedString::new("unused")),
        cx.meta_word(DUMMY_SP, token::InternedString::new("unstable")),
    )));

    MacItems::new(Some(cx.item_mod(sp, sp, ident, vec!(attr), ast.items.clone())).into_iter())
}

fn parse_arg(cx: &mut ExtCtxt, tts: &[ast::TokenTree]) -> Option<String> {
    use syntax::print::pprust;

    let mut parser = parse::new_parser_from_tts(cx.parse_sess(), cx.cfg(),
                                                tts.to_vec());
    // The `expand_expr` method is called so that any macro calls in the
    // parsed expression are expanded.
    let arg = cx.expander().fold_expr(parser.parse_expr());
    match arg.node {
        ast::ExprLit(ref spanned) => {
            match spanned.node {
                ast::LitStr(ref n, _) => {
                    if !parser.eat(&token::Eof) {
                        cx.span_err(parser.span,
                                    "expected only one string literal");
                        return None
                    }
                    return Some(n.get().to_string())
                }
                _ => {}
            }
        }
        _ => {}
    }

    let err = format!("expected string literal but got `{}`",
                      pprust::expr_to_string(&*arg));
    cx.span_err(parser.span, err.as_slice());
    None
}
