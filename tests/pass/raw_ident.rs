use peg::parser;

parser! {
    pub grammar g() for str {
        pub rule r#break() = "foo"
    }
}

#[test]
fn main() {
    assert_eq!(g::r#break("foo"), Ok(()));
}
