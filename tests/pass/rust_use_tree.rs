peg::parser!(
grammar parser() for str {
    #[allow(unused_imports)]
    use ::std::{
        borrow::{
            Borrow as _,
            Cow,
        },
        collections::*,
    };

    rule val() = ['a'..='z']+

    pub rule foo() -> HashMap<&'input str, Cow<'input, str>>
        = kvs:(k:$(val()) ":" v:$(val()) {(k, v.into())})++","
        { HashMap::from_iter(kvs) }
}
);

#[test]
fn main() {
    assert_eq!(
        parser::foo("a:b,c:d"),
        Ok(std::collections::HashMap::from_iter([
            ("a", std::borrow::Cow::from("b")),
            ("c", std::borrow::Cow::from("d")),
        ]))
    );
}
