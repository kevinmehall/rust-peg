extern crate peg;
use peg::parser;

parser!{
    pub grammar nonexhaustive() for [u8] {
        pub nonexhaustive rule foo() = "foo"
    }
}

fn main() {
    assert_eq!(nonexhaustive::foo(b"foobar"), Ok(()));
}
