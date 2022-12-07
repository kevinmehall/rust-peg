//! Utilities for `str` input

use super::{Parse, ParseElem, ParseLiteral, ParseSlice, RuleResult};
use std::fmt::Display;

/// Line and column within a string
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub struct LineCol {
    /// Line (1-indexed)
    pub line: usize,

    /// Column (1-indexed)
    pub column: usize,

    /// Byte offset from start of string (0-indexed)
    pub offset: usize,
}

impl Display for LineCol {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(fmt, "{}:{}", self.line, self.column)
    }
}

impl Parse for &str {
    type PositionRepr = LineCol;
    fn start(self) -> usize {
        0
    }

    fn is_eof(self, pos: usize) -> bool {
        pos >= self.len()
    }

    fn position_repr(self, pos: usize) -> LineCol {
        let before = &self[..pos];
        let line = before.as_bytes().iter().filter(|&&c| c == b'\n').count() + 1;
        let column = before.chars().rev().take_while(|&c| c != '\n').count() + 1;
        LineCol {
            line,
            column,
            offset: pos,
        }
    }
}

impl<'input> ParseElem for &'input str {
    type Element = char;

    fn parse_elem(self, pos: usize) -> RuleResult<char> {
        match self[pos..].chars().next() {
            Some(c) => RuleResult::Matched(pos + c.len_utf8(), c),
            None => RuleResult::Failed,
        }
    }
}

impl ParseLiteral for &str {
    fn parse_string_literal(self, pos: usize, literal: &str) -> RuleResult<()> {
        let l = literal.len();
        if self.len() >= pos + l && &self.as_bytes()[pos..pos + l] == literal.as_bytes() {
            RuleResult::Matched(pos + l, ())
        } else {
            RuleResult::Failed
        }
    }
}

impl<'input> ParseSlice for &'input str {
    type Slice = &'input str;
    fn parse_slice(self, p1: usize, p2: usize) -> &'input str {
        &self[p1..p2]
    }
}
