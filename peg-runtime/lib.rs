use std::fmt::Display;

pub mod str;
pub mod slice;
pub mod error;

#[derive(Clone)]
pub enum RuleResult<T> {
    Matched(usize, T),
    Failed,
}

pub trait Parse {
    type PositionRepr: Display;
    fn start<'input>(&'input self) -> usize;
    fn position_repr<'input>(&'input self, p: usize) -> Self::PositionRepr;
}

pub trait ParseElem: Parse {
    type Element;
    fn parse_elem(&self, pos: usize) -> RuleResult<Self::Element>;
}

pub trait ParseLiteral: Parse {
    fn parse_string_literal(&self, pos: usize, literal: &str) -> RuleResult<()>;
}

pub trait ParseSlice<'input>: Parse {
    type Slice;
    fn parse_slice(&'input self, p1: usize, p2: usize) -> Self::Slice;
}

