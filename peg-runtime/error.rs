//! Parse error reporting

use crate::{Parse, RuleResult, ParseResults, ParseResult};
use std::collections::{HashSet, BTreeSet};
use std::fmt::{self, Debug, Display};

/// A set of literals or names that failed to match
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ExpectedSet {
    expected: HashSet<&'static str>,
}

impl ExpectedSet {
    /// Iterator of expected literals
    pub fn tokens<'a>(&'a self) -> impl Iterator<Item = &'static str> + 'a {
        self.expected.iter().map(|x| *x)
    }

    /// Construct a new singleton set.
    pub(crate) fn singleton(error: &'static str) -> Self {
        let mut expected = HashSet::new();
        expected.insert(error);
        ExpectedSet { expected }
    }
}

impl Display for ExpectedSet {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.expected.is_empty() {
            write!(fmt, "<unreported>")?;
        } else if self.expected.len() == 1 {
            write!(fmt, "{}", self.expected.iter().next().unwrap())?;
        } else {
            let mut errors = self.tokens().collect::<Vec<_>>();
            errors.sort();
            let mut iter = errors.into_iter();

            write!(fmt, "one of {}", iter.next().unwrap())?;
            for elem in iter {
                write!(fmt, ", {}", elem)?;
            }
        }

        Ok(())
    }
}

/// A parse error raised by an `error!` expression.
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub struct ParseErr<L> {
    /// The location at which the error occurred.
    pub location: L,

    /// The error reported at that location.
    pub error: &'static str,
}

impl Copy for ParseErr<usize> {}

impl<L: Display> Display for ParseErr<L> {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(
            fmt,
            "error at {}: {}",
            self.location, self.error
        )
    }
}

impl<L> ParseErr<L> {
    pub fn into_parseerror(self) -> ParseError<L> {
        ParseError {
            location: self.location,
            expected: ExpectedSet::singleton(self.error)
        }
    }
}

/// A parse failure.
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseError<L> {
    /// The furthest position the parser reached in the input before failing.
    pub location: L,

    /// The set of literals that failed to match at that position.
    pub expected: ExpectedSet,
}

impl<L: Display> Display for ParseError<L> {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(
            fmt,
            "error at {}: expected {}",
            self.location, self.expected
        )
    }
}

impl<L: Display + Debug> ::std::error::Error for ParseError<L> {
    fn description(&self) -> &str {
        "parse error"
    }
}

#[doc(hidden)]
#[derive(Debug)]
pub struct ErrorState {
    /// Furthest failure we've hit so far. Not used for parse errors.
    pub max_err_pos: usize,

    /// Are we inside a lookahead/quiet block? If so, failure/error and recovery rules are disabled.
    /// Non-zero => yes, to support nested blocks.
    pub suppress_fail: usize,

    /// Are we reparsing after a failure? If so, compute and store expected set of all alternative expectations
    /// when we are at offset `max_err_pos`. Not used for parse errors.
    pub reparsing_on_failure: bool,

    /// The set of tokens we expected to find when we hit the failure. Updated when `reparsing_on_failure`.
    pub expected: ExpectedSet,

    /// The set of parse errors we have recovered from so far.
    pub errors: BTreeSet<ParseErr<usize>>,
}

impl ErrorState {
    pub fn new(initial_pos: usize) -> Self {
        ErrorState {
            max_err_pos: initial_pos,
            suppress_fail: 0,
            reparsing_on_failure: false,
            expected: ExpectedSet {
                expected: HashSet::new(),
            },
            errors: BTreeSet::new(),
        }
    }

    /// Set up for reparsing to record the details of the furthest failure.
    pub fn reparse_for_failure(&mut self) {
        self.suppress_fail = 0;
        self.reparsing_on_failure = true;
    }

    #[inline(never)]
    fn mark_failure_slow_path(&mut self, pos: usize, expected: &'static str) {
        if pos == self.max_err_pos {
            self.expected.expected.insert(expected);
        }
    }

    /// Flag a failure.
    #[inline(always)]
    pub fn mark_failure(&mut self, pos: usize, expected: &'static str) -> RuleResult<()> {
        if self.suppress_fail == 0 {
            if self.reparsing_on_failure {
                self.mark_failure_slow_path(pos, expected);
            } else if pos > self.max_err_pos {
                self.max_err_pos = pos;
            }
        }
        RuleResult::Failed
    }

    /// Flag an error.
    #[inline(always)]
    pub fn mark_error(&mut self, error: ParseErr<usize>) {
        if self.suppress_fail == 0 {
            self.errors.insert(error);
        }
    }

    /// Build `Matched` parse result.
    pub fn into_matched<T, I: Parse + ?Sized>(self, v: T, input: &I) -> ParseResults<T, I::PositionRepr> {
        ParseResults {
            result: ParseResult::Matched(v),
            errors: errors_positioned_in(self.errors, input),
        }
    }

    /// Build `Failed` parse result.
    pub fn into_failure<T, I: Parse + ?Sized>(self, input: &I) -> ParseResults<T, I::PositionRepr> {
        ParseResults {
            result: ParseResult::Failed(ParseError {
                expected: self.expected,
                location: input.position_repr(self.max_err_pos)
            }),
            errors: errors_positioned_in(self.errors, input),
        }
    }

    /// Build `Error` parse result.
    pub fn into_error<T, I: Parse + ?Sized>(self, error: ParseErr<usize>, input: &I) -> ParseResults<T, I::PositionRepr> {
        ParseResults {
            result: ParseResult::Error(ParseErr {
                location: input.position_repr(error.location),
                error: error.error,
            }),
            errors: errors_positioned_in(self.errors, input),
        }
    }
}

/// Calculate locations of set of parse errors.
fn errors_positioned_in<I: Parse + ?Sized>(errors: impl IntoIterator<Item = ParseErr<usize>>, input: &I) -> Vec<ParseErr<I::PositionRepr>> {
    let mut errors_ret = vec![];
    for error in errors {
        errors_ret.push(ParseErr { location: input.position_repr(error.location), error: error.error })
    }
    errors_ret
}
