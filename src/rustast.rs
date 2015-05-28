extern crate syntax;

pub use syntax::ast;
pub use syntax::ptr::P;
pub use syntax::codemap::DUMMY_SP;
pub use syntax::ext::base::ExtCtxt;
pub use syntax::ast::{Mod, Item, Expr, TokenTree};
pub use syntax::parse::token::str_to_ident;
pub use syntax::ext::build::AstBuilder;
pub use syntax::print::pprust::{expr_to_string, item_to_string};

pub fn module(items: Vec<P<Item>>) -> P<Mod> {
	P(Mod{
		inner: DUMMY_SP,
		items: items,
	})
}

pub fn parse_path(e: &str) -> ast::Path {
	let ps = syntax::parse::ParseSess::new();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_path(syntax::parse::parser::NoTypesAllowed);
	p.abort_if_errors();
	r.unwrap_or_else(|_|panic!())
}

pub fn parse_path_vec(s: &str) -> Vec<ast::Ident> {
	s.split("::").map(|i| str_to_ident(i)).collect()
}

pub fn parse_block(ctxt: &ExtCtxt, e: &str) -> P<ast::Block> {
	let mut p = syntax::parse::new_parser_from_source_str(&ctxt.parse_sess, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_block();
	p.abort_if_errors();
	r.unwrap_or_else(|e| panic!(e))
}

pub fn parse_type(e: &str) -> P<ast::Ty> {
	let ps = syntax::parse::ParseSess::new();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_ty();
	p.abort_if_errors();
	r
}
