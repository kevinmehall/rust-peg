#![no_std]

use alloc::collections::BTreeMap;

extern crate peg;
extern crate alloc;

peg::parser! {
    grammar lol(config: bool) for str {
        #[cache_left_rec(BTreeMap)]
        rule one() -> ()
            = one() / "foo"
    }
}

fn main() {}
