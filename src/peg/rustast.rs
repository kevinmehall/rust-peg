extern crate syntax;

pub use syntax::ast;
pub use syntax::ast::P;
pub use syntax::codemap::DUMMY_SP;
pub use syntax::ext::base::ExtCtxt;
pub use syntax::ast::{Mod, Item, Expr};
pub use syntax::parse::token::str_to_ident;
pub use syntax::ext::build::AstBuilder;
pub use syntax::print::pprust::{expr_to_str, item_to_str};

pub fn module(items: Vec<P<Item>>) -> P<Mod> {
	P(Mod{
		inner: DUMMY_SP,
		view_items: Vec::new(),
		items: items,
	})
}

pub fn parse_expr(e: &str) -> P<ast::Expr> {
	let ps = syntax::parse::new_parse_sess();
	let mut p = syntax::parse::new_parser_from_source_str(&ps, Vec::new(), "file".to_string(), e.to_string());
	let r = p.parse_expr();
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


struct ErrLoader;
impl syntax::ext::base::CrateLoader for ErrLoader {
	fn load_crate(&mut self, _: &syntax::ast::ViewItem) -> syntax::ext::base::MacroCrate {
		fail!("can't load_crate")
	}
}

pub fn with_fake_extctxt<T>(f: |&ExtCtxt| -> T) -> T {
	let ps = syntax::parse::new_parse_sess();
	let mut loader = ErrLoader;

	let mut cx = syntax::ext::base::ExtCtxt::new(&ps, Vec::new(), syntax::ext::expand::ExpansionConfig {
		loader: &mut loader,
		deriving_hash_type_parameter: false,
		crate_id: from_str("test").unwrap(),
	});

	cx.bt_push(syntax::codemap::ExpnInfo{
		call_site: DUMMY_SP,
		callee: syntax::codemap::NameAndSpan {
			name: "test".to_string(),
			format: syntax::codemap::MacroBang,
			span: None,
		}
	});

	f(&cx)
}
