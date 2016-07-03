pub use syntax;
pub use errors;
pub use syntax::ast;
pub use syntax::ptr::P;
pub use syntax::codemap::DUMMY_SP;
pub use syntax::ext::base::ExtCtxt;
pub use syntax::tokenstream::TokenTree;
pub use syntax::ast::{Mod, Item, Expr};
pub use syntax::parse::token::str_to_ident;
pub use syntax::ext::build::AstBuilder;
pub use syntax::print::pprust::{expr_to_string, item_to_string};

pub fn module(items: Vec<P<Item>>) -> P<Mod> {
	P(Mod{
		inner: DUMMY_SP,
		items: items,
	})
}

pub fn parse_path(ctxt: &ExtCtxt, e: &str) -> ast::Path {
	let mut p = syntax::parse::new_parser_from_source_str(&ctxt.parse_sess, Vec::new(), "<peg>".to_string(), e.to_string());
	let r = panictry!(p.parse_path(syntax::parse::parser::PathStyle::Mod));
	p.abort_if_errors();
	r
}

pub fn parse_path_vec(s: &str) -> Vec<ast::Ident> {
	s.split("::").map(|i| str_to_ident(i)).collect()
}

pub fn parse_block(ctxt: &ExtCtxt, e: &str) -> P<ast::Block> {
	let mut p = syntax::parse::new_parser_from_source_str(&ctxt.parse_sess, Vec::new(), "<peg>".to_string(), e.to_string());
	let r = panictry!(p.parse_block());
	p.abort_if_errors();
	r
}

pub fn parse_type(ctxt: &ExtCtxt, e: &str) -> P<ast::Ty> {
	let mut p = syntax::parse::new_parser_from_source_str(&ctxt.parse_sess, Vec::new(), "<peg>".to_string(), e.to_string());
	let r = panictry!(p.parse_ty());
	p.abort_if_errors();
	r
}
