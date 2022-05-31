extern crate peg;

peg::parser!( grammar test_grammar() for str {
    pub rule options() -> Option<()>
        = "abc" v:"def"? {v}

    pub rule option_unused_result() = "a"? / "b"
});

use self::test_grammar::*;

fn main() {
    assert_eq!(options("abc").into_result(), Ok(None));
    assert_eq!(options("abcdef").into_result(), Ok(Some(())));
    assert!(options("def").into_result().is_err());
}