extern crate peg;

peg::parser!(grammar arithmetic() for str {
    pub(crate) rule no_assoc() = precedence! {
        @ "+" @ {}
        --
        @ "?" {}
        --
        "-" @ {}
        --
        "x" {}
    }
});

#[test]
fn main() {
    assert!(arithmetic::no_assoc("x+x").is_ok());
    assert!(arithmetic::no_assoc("-x").is_ok());
    assert!(arithmetic::no_assoc("x?").is_ok());

    assert!(arithmetic::no_assoc("-x?").is_ok());
    assert!(arithmetic::no_assoc("x+-x").is_ok());
    assert!(arithmetic::no_assoc("-x+x").is_ok());
    assert!(arithmetic::no_assoc("-x+-x").is_ok());
    assert!(arithmetic::no_assoc("x+x?").is_ok());
    assert!(arithmetic::no_assoc("x?+x").is_ok());
    assert!(arithmetic::no_assoc("x?+x?").is_ok());
    assert!(arithmetic::no_assoc("x+-x?").is_ok());
    assert!(arithmetic::no_assoc("x?+-x?").is_ok());
    assert!(arithmetic::no_assoc("-x?+-x?").is_ok());

    assert!(arithmetic::no_assoc("x+x+x").is_err());
    assert!(arithmetic::no_assoc("x?+x+x").is_err());
    assert!(arithmetic::no_assoc("-x+x+x").is_err());
    assert!(arithmetic::no_assoc("x+-x+x").is_err());
    assert!(arithmetic::no_assoc("x+x?+x").is_err());
    assert!(arithmetic::no_assoc("x+x?+-x").is_err());
    assert!(arithmetic::no_assoc("x+x?+x?").is_err());
}
