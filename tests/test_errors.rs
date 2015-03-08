#![feature(plugin, core, unicode, collections)]
#![plugin(peg_syntax_ext)]

use parser::parse;
use parser::ParseError;

peg! parser(r#"
#[pub]
parse -> usize
    = v:( "a" / "\n" )*   { v.len() }
"#);

#[test]
fn test_errors() {
    assert_eq!(parse(r#"
aaaa
aaaaaa
aaaabaaaa
"#), Err(ParseError {
        line: 4,
        column: 5,
        offset: 17,
        expected: vec!["a", "\n"].into_iter().collect(),
    }));

    println!("Ok");
}
