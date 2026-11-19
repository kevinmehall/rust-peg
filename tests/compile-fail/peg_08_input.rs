peg::parser!(grammar string() for str {
    rule test() = ""
});

peg::parser!(grammar slice() for [u8] {
    rule test() = ""
});

fn main() {}
