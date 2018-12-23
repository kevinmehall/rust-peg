extern crate peg;

peg::parser!{ grammar parser() for str {
    pub rule parse -> usize
        = v:( "a" / "\n" )* { v.len() }
}}

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
    assert_eq!(format!("{}", err.expected), "one of `\\n`, `a`");
}
