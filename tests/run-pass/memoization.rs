#[cfg(not(feature = "std"))] #[macro_use] extern crate alloc;
extern crate peg;

peg::parser!{ grammar memo() for str {
    #[cache]
    rule r() -> &'input str
        = s:$(['a'..='z']+) { s }

    pub rule parse()
        = r() "+" r() { () }
        / r() " " r() { () }
}}

fn main() {
    assert_eq!(memo::parse("abc zzz"), Ok(()));
}
