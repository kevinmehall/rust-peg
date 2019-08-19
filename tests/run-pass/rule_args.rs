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
    rule _() = [' ']*
    pub rule ifelse() = keyword("if") _ ident() _ keyword("then") _ ident() _ keyword("else") _ ident()
});

use ra::*;

fn main() {
    assert_eq!(list("1,2,3,4"), Ok(vec![1,2,3,4]));
    assert_eq!(array("[1,1,2,3,5,]"), Ok(vec![1,1,2,3,5]));

    assert!(ifelse("if foo then x else y").is_ok());
    assert!(ifelse("iffoothenxelsey").is_err());
}