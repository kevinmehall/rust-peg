extern crate peg;

peg::parser!( grammar ra() for str {
    use peg::ParseLiteral;

    rule number() -> i64
        = n:$(['0'..='9']+) { n.parse().unwrap() }

    rule commasep<T>(x: rule<T>) -> Vec<T> = v:(x() ** ",") ","? {v}
    rule bracketed<T>(x: rule<T>) -> T = "[" v:x() "]" {v}

    pub rule list() -> Vec<i64> = commasep(<number()>)
    pub rule array() -> Vec<i64> = bracketed(<commasep(<number()>)>)

    rule keyword(id: &'static str) = ##parse_string_literal(id) !['0'..='9' | 'a'..='z' | 'A'..='Z' | '_']
    rule ident() = ['a'..='z']+
    rule _ = [' ']*
    pub rule ifelse() = keyword("if") _ ident() _ keyword("then") _ ident() _ keyword("else") _ ident()
    
    pub rule repeated_a(i: usize) = ['a']*<{i}>

    rule i(literal: &'static str) = input:$([_]*<{literal.len()}>) {? if input.eq_ignore_ascii_case(literal) { Ok(()) } else { Err(literal) } }
    
    pub rule test_i() = i("foo") i("bar")

    rule recursive(r: rule<()>) = " " recursive(r) // Issue #226
});

use ra::*;

fn main() {
    assert_eq!(list("1,2,3,4"), Ok(vec![1,2,3,4]));
    assert_eq!(array("[1,1,2,3,5,]"), Ok(vec![1,1,2,3,5]));

    assert!(ifelse("if foo then x else y").is_ok());
    assert!(ifelse("iffoothenxelsey").is_err());

    assert!(repeated_a("aa", 2).is_ok());
    assert!(repeated_a("aaa", 2).is_err());
    assert!(repeated_a("aaaaa", 5).is_ok());

    assert!(test_i("fOoBaR").is_ok());
    assert!(test_i("fOoBaZ").is_err());
    assert!(test_i("fOoX").is_err());
}