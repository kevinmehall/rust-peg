#![feature(plugin)]
#![plugin(peg_syntax_ext)]

use parser::parse;

peg! parser(r#"
#[static]
test_static -> usize = {0}

#[export]
parse -> usize
        = char+ { *test_static }

char -> ()
        = . { *test_static = *test_static + 1; () }
"#);

#[test]
fn test_static() {
    assert_eq!(parse("abcdef").unwrap(), 6);
}
