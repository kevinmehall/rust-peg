extern crate peg;
use peg::peg;

peg!{memo r#"

#[cache]
r -> &'input str
    = s:$(['a'..='z']+) { s }

pub parse
    = r '+' r { () }
    / r ' ' r { () }

"#}

#[test]
fn main() {
	assert_eq!(memo::parse("abc zzz"), Ok(()));
}
