use crate::line_counter::lines;

peg::parser!( grammar line_counter() for str {
    line_count: u32;

    pub rule lines() -> u32
        = line()* { __state.line_count }

    rule line()
        = (terminated_line() / unterminated_line()) { __state.line_count += 1; }

    rule terminated_line()
        = (not_eol() one_char())* eol()

    rule unterminated_line()
        = (not_eol() one_char())+

    rule one_char() = [_]
    rule not_eol() = !eol()
    rule eol() = "\r\n" / "\n"
    rule eof() = ![_]
});

fn main() {
    assert_eq!(lines(""), Ok(0));

    assert_eq!(lines("\n"), Ok(1));
    assert_eq!(lines("abc\n"), Ok(1));
    assert_eq!(lines("abc"), Ok(1));

    assert_eq!(lines("\n\n"), Ok(2));
    assert_eq!(lines("abc\ndef\n"), Ok(2));
    assert_eq!(lines("abc\ndef"), Ok(2));
    assert_eq!(lines("\ndef"), Ok(2));
    assert_eq!(lines("\ndef\n"), Ok(2));
}
