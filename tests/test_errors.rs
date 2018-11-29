extern crate peg;
use peg::peg;

peg!{parser r#"
pub parse -> usize
    = v:( "a" / "\n" )*   { v.len() }
"#}

#[test]
fn test_errors() {
    let err = parser::parse(r#"
aaaa
aaaaaa
aaaabaaaa
"#).unwrap_err();

    assert_eq!(err.location.line, 4);
    assert_eq!(err.location.column, 5);
    assert_eq!(err.location.offset, 17);
    assert_eq!(err.expected, vec!["a", "\n"].into_iter().collect());
}
