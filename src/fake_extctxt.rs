use syntax;
use syntax::codemap::DUMMY_SP;
use syntax::ext::base::ExtCtxt;

/// Create a fake ExtCtxt to perform macro quasiquotes outside of rustc plugins
pub fn with_fake_extctxt<T>(f: |&ExtCtxt| -> T) -> T {
  let ps = syntax::parse::new_parse_sess();

  let mut cx = syntax::ext::base::ExtCtxt::new(&ps, Vec::new(),
    syntax::ext::expand::ExpansionConfig::default("rust-peg".to_string())
  );

  cx.bt_push(syntax::codemap::ExpnInfo{
    call_site: DUMMY_SP,
    callee: syntax::codemap::NameAndSpan {
      name: "".to_string(),
      format: syntax::codemap::MacroBang,
      span: None,
    }
  });

  f(&cx)
}
