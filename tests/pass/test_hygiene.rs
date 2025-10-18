#![allow(dead_code)]
use ::peg as realpeg;
struct Result;
struct ParseResult;
struct Parse;
struct Input;
struct ParseState;
struct ErrorState;
struct Vec;
struct HashMap;
mod peg {}

realpeg::parser! {
    grammar p() for str {
        pub rule number() -> f64 = n:$(['0'..='9']+) { n.parse().unwrap() }

        #[cache]
        pub rule cached() = "x"

        pub rule prec() -> () = precedence!{
            "x" {}
        }
    }
}

#[test]
fn main() {
    ::core::assert_eq!(p::number("12345"), ::core::result::Result::Ok(12345.0))
}
