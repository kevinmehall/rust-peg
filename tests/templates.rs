extern crate peg;

peg::parser!( grammar templates() for str {
    rule parenthesized<foo> = "(" s:foo ")" { s }
    pub rule parens -> &'input str = parenthesized<$(['a'..='z']*)>

    rule double_parenthesized<x> = parenthesized<parenthesized<x>>
    pub rule double_parens -> &'input str = double_parenthesized<$(['a'..='z']*)>
});

use self::templates::*;

#[test]
fn test_templates() {
    assert_eq!(parens("(asdf)").unwrap(), "asdf");
    assert_eq!(double_parens("((asdf))").unwrap(), "asdf");
}
