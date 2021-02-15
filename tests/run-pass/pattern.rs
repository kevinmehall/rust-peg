peg::parser!( grammar test() for str {
    pub rule alphanumeric() = ['a'..='z' | 'A'..='Z' | '0'..='9']*
    pub rule inverted_pat() -> &'input str = "(" s:$([^')']*) ")" {s}
});

fn main() {
    assert!(test::alphanumeric("azAZ09").is_ok());
    assert!(test::alphanumeric("@").is_err());

    assert_eq!(test::inverted_pat("(asdf)"), Ok("asdf"));
}

