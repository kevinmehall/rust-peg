extern crate peg;

peg::parser!(grammar foo() for str {
    fn asdf() {} //~ ERROR expected ":"
});

fn main() {}
