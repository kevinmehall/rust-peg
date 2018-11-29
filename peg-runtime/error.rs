use std::fmt::{ Display, Debug };
use crate::RuleResult;

fn escape_default(s: &str) -> String {
    s.chars().flat_map(|c| c.escape_default()).collect()
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseError<L> {
    pub location: L,
    pub expected: ::std::collections::HashSet<&'static str>,
}

impl<L: Display> Display for ParseError<L> {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(fmt, "error at {}: expected ", self.location)?;
        if self.expected.is_empty() {
            write!(fmt, "EOF")?;
        } else if self.expected.len() == 1 {
            write!(fmt, "`{}`", escape_default(self.expected.iter().next().unwrap()))?;
        } else {
            let mut iter = self.expected.iter();

            write!(fmt, "one of `{}`", escape_default(iter.next().unwrap()))?;
            for elem in iter {
                write!(fmt, ", `{}`", escape_default(elem))?;
            }
        }

        Ok(())
    }
}

impl<L: Display + Debug> ::std::error::Error for ParseError<L> {
    fn description(&self) -> &str {
        "parse error"
    }
}

pub struct ErrorState<P> {
    pub max_err_pos: P,
    pub suppress_fail: usize,
    pub reparsing_on_error: bool,
    pub expected: ::std::collections::HashSet<&'static str>,
}

impl<P: PartialOrd> ErrorState<P> {
    pub fn new(initial_pos: P, reparsing: bool) -> ErrorState<P> {
        ErrorState {
            max_err_pos: initial_pos,
            suppress_fail: 0,
            reparsing_on_error: reparsing,
            expected: ::std::collections::HashSet::new(),
        }
    }

    #[inline(never)]
    pub fn mark_failure_slow_path(&mut self, pos: P, expected: &'static str) {
        if pos == self.max_err_pos {
            self.expected.insert(expected);
        }
    }

    #[inline(always)]
    pub fn mark_failure(&mut self, pos: P, expected: &'static str) -> RuleResult<P, ()> {
        if self.suppress_fail == 0 {
            if self.reparsing_on_error {
                self.mark_failure_slow_path(pos, expected);
            } else if pos > self.max_err_pos {
                self.max_err_pos = pos;
            }
        }
        RuleResult::Failed
    }
}