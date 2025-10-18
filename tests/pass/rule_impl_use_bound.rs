#[cfg(not(feature = "trace"))]
use core::fmt::Debug;

// `--features trace` code names the return type, so doesn't work with `impl Trait`
#[cfg(not(feature = "trace"))]
peg::parser!( grammar test() for str {
    pub rule foo() -> impl for<'a> PartialEq<&'a str> + Debug + use<'input>
        = $("foo")
    pub rule bar() -> impl for<'a> PartialEq<&'a str> + Debug + use<>
        = { "bar" }
});

#[test]
fn main() {
    #[cfg(not(feature = "trace"))]
    {
        assert_eq!(test::foo("foo").unwrap(), "foo");
        assert_eq!(test::bar("").unwrap(), "bar");
    }
}
