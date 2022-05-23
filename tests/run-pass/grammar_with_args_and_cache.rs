extern crate peg;

#[cfg(not(feature = "std"))] #[macro_use] extern crate alloc;

peg::parser! {
    grammar lol(config: bool) for str {
        #[cache_left_rec]
        rule one() -> ()
            = one() / "foo"
    }
}

fn main() {}
