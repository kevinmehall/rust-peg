use syntax;
use syntax::codemap::DUMMY_SP;
use syntax::ext::base::ExtCtxt;
use syntax::feature_gate::GatedCfg;

/// Create a fake ExtCtxt to perform macro quasiquotes outside of rustc plugins
pub fn with_fake_extctxt<T, F: Fn(&ExtCtxt) -> T>(f: F) -> T {
  let ps = syntax::parse::ParseSess::new();
  let mut fg_cfg = Vec::<GatedCfg>::new();

  let mut cx = syntax::ext::base::ExtCtxt::new(&ps, Vec::new(),
    syntax::ext::expand::ExpansionConfig::default("rust-peg".to_string()),
    &mut fg_cfg
  );

  cx.bt_push(syntax::codemap::ExpnInfo{
    call_site: DUMMY_SP,
    callee: syntax::codemap::NameAndSpan {
      name: "".to_string(),
      format: syntax::codemap::MacroBang,
      span: None,
      allow_internal_unstable: false,
    }
  });

  f(&cx)
}
