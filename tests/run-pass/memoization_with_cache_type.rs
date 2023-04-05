#![no_std]

use alloc::collections::BTreeMap;
use alloc::vec;

extern crate peg;
extern crate alloc;

peg::parser!{ grammar memo() for str {
    #[cache(BTreeMap)]
    rule r() -> &'input str
        = s:$(['a'..='z']+) { s }

    pub rule parse()
        = r() "+" r() { () }
        / r() " " r() { () }
}}

fn main() {
    assert_eq!(memo::parse("abc zzz"), Ok(()));
}
