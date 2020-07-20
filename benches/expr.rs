#![feature(test, crate_visibility_modifier)]
extern crate peg;

extern crate test;

use test::Bencher;

peg::parser!(grammar parser() for str {
crate rule expr() = eq()

#[cache]
rule eq() = additive() "=" eq() / additive()
#[cache]
rule additive() = multitive() "+" additive() / multitive()
#[cache]
rule multitive() = pow() "*" multitive() / pow()
#[cache]
rule pow() = atom() "^" pow() / atom()

#[cache]
rule atom() = ['0'..='9']+ / "(" expr() ")"
});

#[bench]
fn expr(b: &mut Bencher) {
    let bench_str = "1+2+3+4*5*6^7^8^(0^1*2+1)";

    b.bytes = bench_str.len() as u64;
    b.iter(|| {
        parser::expr(bench_str).unwrap();
    });
}
