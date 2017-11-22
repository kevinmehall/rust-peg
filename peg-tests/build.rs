extern crate peg;

fn main() {
    peg::cargo_build("src/test_subgrammar.rustpeg");
    peg::cargo_build("src/test_subgrammar_with_args.rustpeg");
    peg::cargo_build("src/test_grammar.rustpeg");
}
