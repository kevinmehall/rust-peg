use std::fmt::Display;

pub mod error;
mod slice;
pub mod str;

/// The result type used internally in the parser.
///
/// You'll only need this if implementing the `Parse*` traits for a custom input
/// type. The public API of a parser adapts errors to `std::result::Result`.
#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Debug, Hash)]
pub enum RuleResult<T> {
    /// Success, with final location
    Matched(usize, T),

    /// Failure (furthest failure location is not yet known)
    Failed,
}

/// A type that can be used as input to a parser.
#[allow(clippy::needless_lifetimes)]
pub trait Parse: Copy {
    type PositionRepr: Display;
    fn start(self) -> usize;
    fn is_eof(self, p: usize) -> bool;
    fn position_repr(self, p: usize) -> Self::PositionRepr;
}

/// A parser input type supporting the `[...]` syntax.
pub trait ParseElem: Parse {
    /// Type of a single atomic element of the input, for example a character or token
    type Element: Copy;

    /// Get the element at `pos`, or `Failed` if past end of input.
    fn parse_elem(self, pos: usize) -> RuleResult<Self::Element>;
}

/// A parser input type supporting the `"literal"` syntax.
pub trait ParseLiteral: Parse {
    /// Attempt to match the `literal` string at `pos`, returning whether it
    /// matched or failed.
    fn parse_string_literal(self, pos: usize, literal: &str) -> RuleResult<()>;
}

/// A parser input type supporting the `$()` syntax.
pub trait ParseSlice: Parse {
    /// Type of a slice of the input.
    type Slice;

    /// Get a slice of input.
    fn parse_slice(self, p1: usize, p2: usize) -> Self::Slice;
}
