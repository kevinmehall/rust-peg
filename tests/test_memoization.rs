#![feature(plugin)]
#![plugin(peg_syntax_ext)]

peg! memo(r#"

#[cache]
rule -> &'input str
    = [a-z]+ { match_str }

#[pub]
parse
    = rule '+' rule { () }
    / rule ' ' rule { () }

"#);

#[test]
fn main() {
	assert_eq!(memo::parse("abc zzz"), Ok(()));
}
