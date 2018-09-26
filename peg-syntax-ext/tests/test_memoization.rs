extern crate peg_syntax_ext;
use peg_syntax_ext::peg;

peg!{memo r#"

#[cache]
r -> &'input str
    = s:$([a-z]+) { s }

pub parse
    = r '+' r { () }
    / r ' ' r { () }

"#}

#[test]
fn main() {
	assert_eq!(memo::parse("abc zzz"), Ok(()));
}
