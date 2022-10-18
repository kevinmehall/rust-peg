use std::fmt::Debug;

peg::parser!{
    grammar g() for str {
        pub rule returns_impl_trait() -> impl Debug
            = "" { Box::new(5) }
    }
}

fn main() {
    assert_eq!(format!("{:?}", g::returns_impl_trait("")), "Ok(5)");
}
