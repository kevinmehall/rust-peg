extern crate trybuild;

fn main() {
    let t = trybuild::TestCases::new();

    let rust_ver = option_env!("TRAVIS_RUST_VERSION");
    if rust_ver.is_none() {
        println!(
            "Note: compile-fail tests are normally only tested on the stable rust compiler in CI."
        );
    }

    if rust_ver.is_none() || rust_ver == Some("stable") {
        t.compile_fail("tests/compile-fail/*.rs");
    } else {
        println!("Skipping compile-fail tests.");
    }

    t.pass("tests/run-pass/*.rs");
}
