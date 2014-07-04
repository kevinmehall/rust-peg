#![crate_type = "dylib"]
#![feature(plugin_registrar, managed_boxes, quote, globs)]

extern crate rustc;
extern crate syntax;

use syntax::ast;
use syntax::codemap;
use syntax::ext::base::{ExtCtxt, MacResult, MacItem, DummyResult};
use syntax::parse;
use syntax::parse::token;
use rustc::plugin::Registry;

use rustast::{AstBuilder, DUMMY_SP};

mod translate;
mod grammar;
mod rustast;

#[plugin_registrar]
pub fn plugin_registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
            token::intern("peg"),
            syntax::ext::base::IdentTT(box syntax::ext::base::BasicIdentMacroExpander {
                expander: expand_peg_str,
                span: None,
            }, None));
}

fn expand_peg_str(cx: &mut ExtCtxt, sp: codemap::Span, ident: ast::Ident, tts: Vec<ast::TokenTree>) -> Box<MacResult> {
    let source = match parse_arg(cx, tts.as_slice()) {
        Some(source) => source,
        None => return DummyResult::expr(sp),
    };

    let grammar_def = grammar::grammar(source.as_slice());

    let grammar_def = match grammar_def {
      Ok(grammar_def) => grammar_def,
      Err(msg) => {
        cx.span_err(sp, msg.as_slice());
        return DummyResult::expr(sp)
      }
    };

    let ast = translate::compile_grammar(cx, &grammar_def);

    // #![allow(non_snake_case_functions, unused_variable)]
    let attr = cx.attribute(DUMMY_SP, cx.meta_list(DUMMY_SP, token::InternedString::new("allow"), vec!(
        cx.meta_word(DUMMY_SP, token::InternedString::new("non_snake_case_functions")),
        cx.meta_word(DUMMY_SP, token::InternedString::new("unused_variable")),
    )));

    MacItem::new(cx.item_mod(sp, sp, ident, vec!(attr), ast.view_items.clone(), ast.items.clone()))
}

fn parse_arg(cx: &mut ExtCtxt, tts: &[ast::TokenTree]) -> Option<String> {
    use syntax::print::pprust;

    let mut parser = parse::new_parser_from_tts(cx.parse_sess(), cx.cfg(),
                                                Vec::from_slice(tts));
    // The `expand_expr` method is called so that any macro calls in the
    // parsed expression are expanded.
    let arg = cx.expand_expr(parser.parse_expr());
    match arg.node {
        ast::ExprLit(spanned) => {
            match spanned.node {
                ast::LitStr(ref n, _) => {
                    if !parser.eat(&token::EOF) {
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
                      pprust::expr_to_str(arg));
    cx.span_err(parser.span, err.as_slice());
    None
}
