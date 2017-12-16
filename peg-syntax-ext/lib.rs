#![feature(plugin_registrar, quote, rustc_private, box_patterns)]
extern crate rustc_plugin;
#[macro_use] pub extern crate syntax;
extern crate rustc_errors as errors;

extern crate peg;

use syntax::ast;
use syntax::codemap;
use syntax::codemap::FileName;
use syntax::ext::base::{ExtCtxt, MacResult, MacEager, DummyResult};
use syntax::tokenstream::TokenTree;
use syntax::parse;
use syntax::parse::token;
use syntax::symbol::Symbol;
use syntax::fold::Folder;
use syntax::util::small_vector::SmallVector;
use rustc_plugin::Registry;
use std::io::Read;
use std::fs::File;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
            Symbol::intern("peg"),
            syntax::ext::base::IdentTT(Box::new(expand_peg_str), None, false));

    reg.register_syntax_extension(
            Symbol::intern("peg_file"),
            syntax::ext::base::IdentTT(Box::new(expand_peg_file), None, false));
}

fn expand_peg_str<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<TokenTree>) -> Box<MacResult + 's> {
    let source = match parse_arg(cx, &tts) {
        Some(source) => source,
        None => return DummyResult::any(sp),
    };

    expand_peg(cx, sp, ident, &source)
}

fn expand_peg_file<'s>(cx: &'s mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<TokenTree>) -> Box<MacResult + 's> {
    let fname = match parse_arg(cx, &tts) {
        Some(fname) => fname,
        None => return DummyResult::any(sp),
    };

    let path = match cx.codemap().span_to_filename(sp) {
        FileName::Real(path) => path.parent().unwrap().join(&fname),
        other => {
          cx.span_err(sp, &format!("cannot resolve relative path in non-file source `{}`", other));
          return DummyResult::any(sp)
        },
    };

    let mut source = String::new();
    if let Err(e) = File::open(&path).map(|mut f| f.read_to_string(&mut source)) {
        cx.span_err(sp, &e.to_string());
        return DummyResult::any(sp);
    }

    cx.codemap().new_filemap(path.into(), "".to_string());

    expand_peg(cx, sp, ident, &source)
}

fn expand_peg(cx: &mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, source: &str) -> Box<MacResult + 'static> {
    let code = match peg::compile(&source) {
        Ok(code) => code,
        Err(e) => {
          cx.span_err(sp, &format!("{}", e));
          return DummyResult::any(sp)
        }
    };

    let mut p = parse::new_parser_from_source_str(&cx.parse_sess, FileName::Custom("peg expansion".into()), code);
    let tts = panictry!(p.parse_all_token_trees());

    let module = quote_item! { cx,
        mod $ident {
            $tts
        }
    }.unwrap();

    MacEager::items(SmallVector::one(module))
}

fn parse_arg(cx: &mut ExtCtxt, tts: &[TokenTree]) -> Option<String> {
    use syntax::print::pprust;

    let mut parser = parse::new_parser_from_tts(cx.parse_sess(), tts.to_vec());
    // The `expand_expr` method is called so that any macro calls in the
    // parsed expression are expanded.
    let arg = cx.expander().fold_expr(panictry!(parser.parse_expr()));
    match arg.node {
        ast::ExprKind::Lit(ref spanned) => {
            match spanned.node {
                ast::LitKind::Str(ref n, _) => {
                    if !parser.eat(&token::Eof) {
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
