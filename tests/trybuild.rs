extern crate trybuild;

fn main() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/compile-fail/*.rs");
    t.pass("tests/run-pass/*.rs");
}