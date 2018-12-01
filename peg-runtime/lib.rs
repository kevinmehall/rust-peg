use std::fmt::Display;

pub mod str;
pub mod slice;
pub mod error;

#[derive(Clone)]
pub enum RuleResult<P, T> {
    Matched(P, T),
    Failed,
}

pub trait Parse<'input> {
    type Position: Clone + PartialOrd;
    type PositionRepr: Display;
    fn start(&'input self) -> Self::Position;
    fn position_repr(&'input self, p: Self::Position) -> Self::PositionRepr;
}

pub trait ParseElem<'input>: Parse<'input> {
    type Element;
    fn parse_elem(&'input self, pos: Self::Position) -> RuleResult<Self::Position, Self::Element>;
}

pub trait ParseLiteral<'input>: Parse<'input> {
    fn parse_string_literal(&'input self, pos: Self::Position, literal: &str) -> RuleResult<Self::Position, ()>;
}

pub trait ParseSlice<'input>: Parse<'input> {
    type Slice;
    fn parse_slice(&'input self, pos: Self::Position, f: impl FnOnce(Self::Position) -> RuleResult<Self::Position, ()>) -> RuleResult<Self::Position, Self::Slice>;
}

