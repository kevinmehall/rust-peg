extern crate peg;

peg::parser!(
grammar parser() for str {
    pub rule todo_rule() -> ()
    pub rule x()
    pub rule y() = "a"
}
);

fn main() {
    assert_eq!(parser::todo_rule(""), Ok(()));
    assert_eq!(parser::x(""), Ok(()));
    assert_eq!(parser::y("a"), Ok(()));
}
