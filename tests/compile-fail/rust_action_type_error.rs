extern crate peg;

peg::parser!(grammar foo() for str {
    rule foo() -> u32 = "a" { "a" } //~ ERROR
});

fn main() {}
