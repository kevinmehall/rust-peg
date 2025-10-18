peg::parser! {
    pub grammar foo_parser() for str {
        use super::types::Foo;

        pub rule foo() -> Foo
            = "foo" { Foo }
    }
}

mod types {
    #[derive(PartialEq, Debug)]
    pub struct Foo;
}

#[test]
fn main() {
    assert_eq!(foo_parser::foo("foo"), Ok(types::Foo));
}
