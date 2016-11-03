use syntax;
use syntax::codemap::DUMMY_SP;
use syntax::ext::base::{ ExtCtxt, DummyResolver };

/// Create a fake ExtCtxt to perform macro quasiquotes outside of rustc plugins
pub fn with_fake_extctxt<T, F: Fn(&ExtCtxt) -> T>(f: F) -> T {
  let ps = syntax::parse::ParseSess::new();
  let mut loader = DummyResolver;

  let mut cx = syntax::ext::base::ExtCtxt::new(&ps,
    syntax::ext::expand::ExpansionConfig::default("rust-peg".to_string()),
    &mut loader,
  );

  cx.bt_push(syntax::codemap::ExpnInfo{
    call_site: DUMMY_SP,
    callee: syntax::codemap::NameAndSpan {
      format: syntax::codemap::MacroBang(syntax::parse::token::intern("")),
      span: None,
      allow_internal_unstable: false,
    }
  });

  f(&cx)
}
