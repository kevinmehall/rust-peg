extern crate syntax;

pub use syntax::ast;
pub use syntax::ptr::P;
pub use syntax::codemap::DUMMY_SP;
pub use syntax::ext::base::ExtCtxt;
pub use syntax::ast::{Mod, Item, Expr, ViewItem};
pub use syntax::parse::token::str_to_ident;
pub use syntax::ext::build::AstBuilder;
pub use syntax::print::pprust::{expr_to_string, item_to_string};
use syntax::print::pprust::to_string;

pub fn module(view_items: Vec<ViewItem>, items: Vec<P<Item>>) -> P<Mod> {
	P(Mod{
		inner: DUMMY_SP,
		view_items: view_items,
		items: items,
	})
}

pub fn parse_path(e: &str) -> ast::Path {
	let ps = syntax::parse::new_parse_sess();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_path(syntax::parse::parser::NoTypesAllowed);
	p.abort_if_errors();
	r.path
}

pub fn parse_path_vec(s: &str) -> Vec<ast::Ident> {
	s.split_str("::").map(|i| str_to_ident(i)).collect()
}

pub fn parse_block(e: &str) -> P<ast::Block> {
	let ps = syntax::parse::new_parse_sess();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_block();
	p.abort_if_errors();
	r
}

pub fn parse_type(e: &str) -> P<ast::Ty> {
	let ps = syntax::parse::new_parse_sess();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_ty(false);
	p.abort_if_errors();
	r
}

// FIXME: https://github.com/rust-lang/rust/pull/18256
pub fn view_item_to_string(i: &ast::ViewItem) -> String {
    to_string(|s| s.print_view_item(i))
}
