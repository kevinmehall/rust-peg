// We allow macro expansion to handle incomplete rules so rust-analyzer can complete a return type, but it should not compile
peg::parser!(grammar test() for str {
    rule test()
});

fn main() {}
