use super::{Parse, ParseElem, ParseLiteral, ParseSlice, RuleResult};

impl<'input, T> Parse for &'input [T] {
    type PositionRepr = usize;
    fn start(self) -> usize {
        0
    }

    fn is_eof(self, pos: usize) -> bool {
        pos >= self.len()
    }

    fn position_repr(self, pos: usize) -> usize {
        pos
    }
}

impl<'input, T: 'input + Copy> ParseElem for &'input [T] {
    type Element = T;

    fn parse_elem(self, pos: usize) -> RuleResult<T> {
        match self[pos..].first() {
            Some(c) => RuleResult::Matched(pos + 1, *c),
            None => RuleResult::Failed,
        }
    }
}

impl ParseLiteral for &[u8] {
    fn parse_string_literal(self, pos: usize, literal: &str) -> RuleResult<()> {
        let l = literal.len();
        if self.len() >= pos + l && &self[pos..pos + l] == literal.as_bytes() {
            RuleResult::Matched(pos + l, ())
        } else {
            RuleResult::Failed
        }
    }
}

impl<'input, T: 'input> ParseSlice for &'input [T] {
    type Slice = &'input [T];
    fn parse_slice(self, p1: usize, p2: usize) -> &'input [T] {
        &self[p1..p2]
    }
}
