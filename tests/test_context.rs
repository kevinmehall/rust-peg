#![feature(plugin)]
#![plugin(peg_syntax_ext)]

use parser::parse;

peg! parser(r#"
#![context(u32)]

#[pub]
parse -> u32 = [0-9]+ { context + match_str.parse::<u32>().unwrap() }
"#);

#[test]
fn test_context() {
    assert_eq!(parse("100", 5), Ok(105));
}
