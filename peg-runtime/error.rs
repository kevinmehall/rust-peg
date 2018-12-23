use std::fmt::{ Display, Debug };
use crate::{ RuleResult, Parse };

fn escape_default(s: &str) -> String {
    s.chars().flat_map(|c| c.escape_default()).collect()
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Expected {
    expected: ::std::collections::HashSet<&'static str>,
}

impl Expected {
    pub fn tokens<'a>(&'a self) -> impl Iterator<Item = &'static str> + 'a {
        self.expected.iter().map(|x| *x)
    }

    pub fn eof(&self) -> bool {
        self.expected.is_empty()
    }
}

impl Display for Expected {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
         if self.eof() {
            write!(fmt, "EOF")?;
        } else if self.expected.len() == 1 {
            write!(fmt, "`{}`", escape_default(self.expected.iter().next().unwrap()))?;
        } else {
            let mut errors = self.tokens().collect::<Vec<_>>();
            errors.sort();
            let mut iter = errors.into_iter();

            write!(fmt, "one of `{}`", escape_default(iter.next().unwrap()))?;
            for elem in iter {
                write!(fmt, ", `{}`", escape_default(elem))?;
            }
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ParseError<L> {
    pub location: L,
    pub expected: Expected,
}

impl<L: Display> Display for ParseError<L> {
    fn fmt(&self, fmt: &mut ::std::fmt::Formatter) -> ::std::result::Result<(), ::std::fmt::Error> {
        write!(fmt, "error at {}: expected {}", self.location, self.expected)
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
    pub fn new(initial_pos: P) -> ErrorState<P> {
        ErrorState {
            max_err_pos: initial_pos,
            suppress_fail: 0,
            reparsing_on_error: false,
            expected: ::std::collections::HashSet::new(),
        }
    }

    pub fn reparse_for_error(&mut self) {
        self.suppress_fail = 0;
        self.reparsing_on_error = true;
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

    pub fn into_parse_error<'i, I: Parse<'i> + ?Sized>(self, input: &'i I) -> ParseError<I::PositionRepr> where I::Position: From<P> {
        ParseError {
            location: Parse::position_repr(input, self.max_err_pos.into()),
            expected: Expected { expected: self.expected }
        }
    }
}