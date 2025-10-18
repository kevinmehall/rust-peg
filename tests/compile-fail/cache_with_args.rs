extern crate peg;

peg::parser!(grammar foo() for str {
    #[cache]
    rule ltarg<'a>() -> &'a str = { "" } //~ ERROR

    #[cache]
    rule rulearg(r: rule<()>) -> &'a str = { "" } //~ ERROR
});

fn main() {}
