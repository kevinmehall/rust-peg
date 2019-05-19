use std::fmt::Display;

pub mod str;
pub mod slice;
pub mod error;

#[derive(Clone)]
pub enum RuleResult<P, T> {
    Matched(P, T),
    Failed,
}

pub trait Parse {
    type Position: Clone + PartialOrd;
    type PositionRepr: Display;
    fn start<'input>(&'input self) -> Self::Position;
    fn position_repr<'input>(&'input self, p: Self::Position) -> Self::PositionRepr;
}

pub trait ParseElem: Parse {
    type Element;
    fn parse_elem(&self, pos: Self::Position) -> RuleResult<Self::Position, Self::Element>;
}

pub trait ParseLiteral: Parse {
    fn parse_string_literal(&self, pos: Self::Position, literal: &str) -> RuleResult<Self::Position, ()>;
}

pub trait ParseSlice<'input>: Parse {
    type Slice;
    fn parse_slice(&'input self, p1: Self::Position, p2: Self::Position) -> Self::Slice;
}

