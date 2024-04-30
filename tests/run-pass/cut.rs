extern crate peg;

peg::parser!( grammar test_grammar() for str {
    pub rule cut() -> Option<&'input str>
        = "a" ending:$("a") {Some(ending)}
        / "a" ~> ending:$("b") {Some(ending)}
        / "a" ending:$("c") {Some(ending)}
});

use self::test_grammar::*;

fn main() {
    assert_eq!(cut("aa"), Ok(Some("a")));
    assert_eq!(cut("ab"), Ok(Some("b")));
    assert!(cut("ac").is_err());
}