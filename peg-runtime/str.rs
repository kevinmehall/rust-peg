use super::{RuleResult, Parse, ParseElem, ParseLiteral, ParseSlice};

impl<'input> Parse<'input> for str {
    type Position = usize;
    fn start(&'input self) -> usize { 0 }
}

impl<'input> ParseElem<'input> for str {
    type Element = char;

    fn parse_elem(&'input self, pos: usize) -> RuleResult<usize, char> {
        match self[pos..].chars().next() {
            Some(c) => RuleResult::Matched(pos + c.len_utf8(), c),
            None => RuleResult::Failed
        }
    }
}

impl<'input> ParseLiteral<'input> for str {
    fn parse_string_literal(&'input self, pos: usize, literal: &str) -> RuleResult<usize, ()> {
        let l = literal.len();
        if self.len() >= pos + l && &self.as_bytes()[pos..pos+l] == literal.as_bytes() {
            RuleResult::Matched(pos+l, ())
        } else {
            RuleResult::Failed
        }
    }
}

impl<'input> ParseSlice<'input> for str {
    type Slice = &'input str;
    fn parse_slice(&'input self, pos: usize, f: impl FnOnce(usize) -> RuleResult<usize, ()>) -> RuleResult<usize, &'input str> {
        match f(pos) {
            RuleResult::Matched(end_pos, ()) => RuleResult::Matched(end_pos, &self[pos..end_pos]),
            RuleResult::Failed => RuleResult::Failed,
        }
    }
}
