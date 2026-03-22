extern crate peg;

peg::parser!(grammar foo() for str {
    fn asdf() {} //~ ERROR expected one of "#", "inject", "crate", "pub", "rule", "use", "}"
});

fn main() {}
