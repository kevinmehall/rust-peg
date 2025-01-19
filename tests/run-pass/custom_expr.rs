use peg::{ RuleResult, ParseLiteral };

peg::parser!( grammar test() for str {
    rule position() -> usize = #{|input, pos| RuleResult::Matched(pos, pos)}
    pub rule test1() -> usize = ['a']* p1:position() ['b']* { p1 }

    pub rule fail() -> usize = #{|input, pos| RuleResult::Failed}

    rule literal(literal: &str) = #{|input, pos| ParseLiteral::parse_string_literal(input, pos, literal) }
    pub rule test2() = literal("foo") "_" literal("bar")
});

fn main() {
    assert_eq!(test::test1("aaaabb"), Ok(4));
    assert_eq!(test::fail("aaaabb").unwrap_err().location.offset, 0);

    assert_eq!(test::test2("foo_bar"), Ok(()));
}
