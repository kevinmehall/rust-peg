peg::parser!( grammar test() for str {
    pub rule alphanumeric() = ['a'..='z' | 'A'..='Z' | '0'..='9']*
    pub rule inverted_pat() -> &'input str = "(" s:$([^')']*) ")" {s}

    pub rule capture() -> char = ['a'..='z']
    pub rule capture2() -> (char, char) = a:['a'..='z'] b:['0'..='9'] { (a, b) }
});

fn main() {
    assert!(test::alphanumeric("azAZ09").into_result().is_ok());
    assert!(test::alphanumeric("@").into_result().is_err());

    assert_eq!(test::inverted_pat("(asdf)").into_result(), Ok("asdf"));

    assert_eq!(test::capture("x").into_result(), Ok('x'));
    assert_eq!(test::capture2("a1").into_result(), Ok(('a', '1')));
}

