//! `rust-peg` is a simple yet flexible parser generator that makes it easy to
//! write robust parsers. Based on the [Parsing Expression
//! Grammar][wikipedia-peg] formalism, it provides a Rust macro that builds a
//! recursive descent parser from a concise definition of the grammar.
//!
//! [wikipedia-peg]: https://en.wikipedia.org/wiki/Parsing_expression_grammar
//!
//! ## Features
//!
//! * Parse input from `&str`, `&[u8]`, `&[T]` or custom types implementing
//!   traits
//! * Customizable reporting of parse errors
//! * Rules can accept arguments to create reusable rule templates
//! * Precedence climbing for prefix/postfix/infix expressions
//! * Helpful `rustc` error messages for errors in the grammar definition or the
//!   Rust code embedded within it
//! * Rule-level tracing to debug grammars
//! * Error recovery

//! ## Overview
//!
//! The `peg::parser!{}` macro encloses a `grammar NAME() for INPUT_TYPE { ...
//! }` definition containing a set of rules which match components of your
//! language.
//!
//! Rules are defined with `rule NAME(PARAMETERS) -> RETURN_TYPE = PEG_EXPR`.
//! The body of the rule, following the `=`, is a PEG expression, definining how
//! the input is matched to produce a value.
//!
//! PEG expressions are evaluated at a particular position of the input. When an
//! expression matches, it advances the position and optionally returns a value.
//! The expression syntax and behavior is [documented
//! below](#expression-reference).
//!
//! The macro expands to a Rust `mod` containing a function for each rule marked
//! `pub` in the grammar. To parse an input sequence, call one of these
//! functions. The call returns a `ParseResults<T,L>`, which carries either the
//! successfully parsed value, the furthest failure, or an error, along with the
//! set of errors which have been recovered from (if any).
//!
//! * Use `unwrap()` to obtain the successful `T` or panic.
//! * Use `into_result()` to obtain a `Result<T, _>`.
//!   Failures and errors are combined, and recovery is treated as failure.
//! * Use `recover_result()` to obtain a `Result<T, _>`.
//!   Failures and errors are combined, and recovery can result in success.
//! * For full details, match on the `result` field and examine the set of
//!   recovered errors in the `errors` field.
//!
//! ## Example
//!
//! Parse a comma-separated list of numbers surrounded by brackets into a `Vec<u32>`:
//!
//! ```rust
//! peg::parser!{
//!   grammar list_parser() for str {
//!     rule number() -> u32
//!       = n:$(['0'..='9']+) {? n.parse().or(Err("u32")) }
//!
//!     pub rule list() -> Vec<u32>
//!       = "[" l:(number() ** ",") "]" { l }
//!   }
//! }
//!
//! pub fn main() {
//!     assert_eq!(list_parser::list("[1,1,2,3,5,8]").into_result(), Ok(vec![1, 1, 2, 3, 5, 8]));
//! }
//! ```
//!
//! ## Expression Reference
//!
//! ### Atoms
//!
//!   * `"keyword"` - _Literal:_ match a literal string.
//!   * `['0'..='9']`  - _Pattern:_ match a single element that matches a Rust `match`-style
//!     pattern. [(details)](#pattern-expressions)
//!   * `[^ '0'..='9']`  - _Inverted pattern:_ match a single element that does not match a Rust `match`-style
//!     pattern. [(details)](#pattern-expressions)
//!   * `some_rule()` - _Rule:_ match a rule defined elsewhere in the grammar and return its
//!     result. Arguments in the parentheses are Rust expressions.
//!   * `_` or `__` or `___` - _Rule (underscore):_ As a special case, rule names
//!     consisting of underscores can be defined and invoked without parentheses. These are
//!     conventionally used to match whitespace between tokens.
//!   * `(e)` - _Parentheses:_ wrap an expression into a group to override
//!     normal precedence. Returns the same value as the inner expression. (Use
//!     an _Action_ block to set the return value for a sequence).
//!
//! ### Combining
//!
//!   * `e1 e2 e3` - _Sequence:_ match expressions in sequence (`e1` followed by `e2` followed by
//!     `e3`), ignoring the return values.
//!   * `a:e1 e2 b:e3 c:e4 { rust }` - _Action:_ match `e1`, `e2`, `e3`, `e4` in
//!     sequence, like above. If they match successfully, run the Rust code in
//!     the block and return its return value. The variable names before the
//!     colons in the sequence are bound to the results of the
//!     corresponding expressions. It is important that the Rust code embedded
//!     in the grammar is deterministic and free of side effects, as it may be
//!     called multiple times.
//!   * `a:e1 b:e2 c:e3 {? rust }` - _Conditional action:_ Like above, but the
//!     Rust block returns a `Result<T, &str>` instead of a value directly. On
//!     `Ok(v)`, it matches successfully and returns `v`. On `Err(e)`, the match
//!     of the entire expression fails and it tries alternatives or reports a
//!     parse failure with the `&str` `e`.
//!   * `e1 / e2 / e3` - _Ordered choice:_ try to match `e1`. If the match succeeds, return its
//!     result, otherwise try `e2`, and so on.
//!
//! ### Repetition
//!   * `expression?` - _Optional:_ match zero or one repetitions of `expression`. Returns an
//!     `Option`.
//!   * `expression*` - _Repeat:_ match zero or more repetitions of `expression` and return the
//!     results as a `Vec`.
//!   * `expression+` - _One-or-more:_ match one or more repetitions of `expression` and return the
//!     results as a `Vec`.
//!   * `expression*<n,m>` - _Range repeat:_ match between `n` and `m` repetitions of `expression`
//!     return the results as a `Vec`. [(details)](#repeat-ranges)
//!   * `expression ** delim` - _Delimited repeat:_ match zero or more repetitions of `expression`
//!     delimited with `delim` and return the results as a `Vec`.
//!   * `expression **<n,m> delim` - _Delimited repeat (range):_ match between `n` and `m` repetitions of `expression`
//!     delimited with `delim` and return the results as a `Vec`. [(details)](#repeat-ranges)
//!   * `expression ++ delim` - _Delimited repeat (one or more):_ match one or more repetitions of `expression`
//!     delimited with `delim` and return the results as a `Vec`.
//!
//!  ### Special
//!   * `$(e)` - _Slice:_ match the expression `e`, and return the slice of the input
//!     corresponding to the match.
//!   * `&e` - _Positive lookahead:_ Match only if `e` matches at this position,
//!     without consuming any characters.
//!   * `!e` - _Negative lookahead:_ Match only if `e` does not match at this
//!     position, without consuming any characters.
//!   * `position!()` - return a `usize` representing the current offset into
//!     the input without consuming anything.
//!   * `quiet!{ e }` - match the expression `e`, but don't report literals within it as "expected" in
//!     error messages.
//!   * `expected!("something")` - fail to match, and report the specified string as expected
//!     at the current location.
//!   * `error!{ "message" e2 }` - report error at this location, then attempt to recover
//!     by matching the expression or sequence `e2` and returning its value.
//!     If recovery is not required, use `!()` as the recovery expression.
//!   * `error_if!{ e1 | "message" e2 }` - attempt to match the expression or sequence `e1`.
//!     If it matches, report error at the start of `e1`, then attempt to recover as for `error!{}`.
//!   * `error_unless!{ e1 | "message" e2 }` - attempt to match the expression or sequence `e1`.
//!     If it does not match, report error, then attempt to recover as for `error!{}`.
//!   * `precedence!{ ... }` - Parse infix, prefix, or postfix expressions by precedence climbing.
//!     [(details)](#precedence-climbing)
//!
//! ## Expression details
//!
//! ### Pattern expressions
//!
//! The `[pat]` syntax expands into a [Rust `match`
//! pattern](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html) against the next character
//! (or element) of the input. When the pattern begins with `^`, the matching behavior is inverted:
//! the expression succeeds only if the pattern does *not* match.
//!
//! To match sets of characters, use Rust's `..=` inclusive range pattern
//! syntax and `|` to match multiple patterns. For example `['a'..='z' | 'A'..='Z']` matches an
//! upper or lower case ASCII alphabet character.
//!
//! If your input type is a slice of an enum type, a pattern could match an enum variant like
//! `[Token::Operator('+')]`.
//!
//! Variables captured by the pattern are accessible in a subsequent action
//! block: `[Token::Integer(i)] { i }`
//!
//! `[_]` matches any single element. As this always matches except at end-of-file, combining it
//! with negative lookahead as `![_]` is the idiom for matching EOF in PEG.
//!
//! ### Repeat ranges
//!
//! The repeat operators `*` and `**` can be followed by an optional range specification of the
//! form `<n>` (exact), `<n,>` (min), `<,m>` (max) or `<n,m>` (range), where `n` and `m` are either
//! integers, or a Rust `usize` expression enclosed in `{}`.
//!
//! ### Errors
//!
//! Errors support recovery: they allow the parser to report an error but continue to parse the rest of the
//! input anyway. This means the parser can report more than one error at once, and can provide
//! a sensible parse of most of the input even when there are errors.
//! These are useful in applications like IDEs.
//!
//! There are three expressions for raising an error:
//!
//!   * `error!{ "message" e2 }` is the simplest form. It reports an error with the given
//!     message at the current location, then attempts to recover by parsing `e2` instead.
//!   * `error_if!{ e1 | "message" e2 }` allows the error to be reported at an earlier
//!     location. It attempts to parse `e1`; if it succeeds it reports an error with
//!     the given message at the start of `e1`, then attempts to recover by parsing `e2` instead.
//!   * `error_unless!{ e1 | "message" e2 }` is shorthand for a choice expression.
//!     It is useful in order to report an error when an expression fails to match.
//!     It attempts to parse `e1`; if it fails it reports an error with the given message
//!     at the current location, then attempts to recover by parsing `e2` instead.
//!
//! Error handling works as follows:
//!
//! * When an error is reported, no further alternatives are tried: parsing immediately stops
//!   at this point (unlike ordinary failure which causes the parser to try other choices).
//! * The reported error is added to the list of errors.
//! * Then the parser attempts to continue by matching the recovery expression.
//!   The resulting value is returned as the result of the `error!` expression;
//!   typically this is just a placeholder value of the right type.
//!   If the recovery expression fails or encounters an error, recovery is abandoned
//!   and the `error!` expression returns the indicated error (`"message"`).
//!
//! There is a worked example in `tests/run-pass/arithmetic_recovery.rs`.
//!
//! This mechanism is based on Sérgio Medeiros and Fabio Mascarenhas,
//! [_Syntax Error Recovery in Parsing Expression Grammars_](https://arxiv.org/abs/1806.11150)
//! and Sérgio Queiroz de Medeiros, Gilney de Azevedo Alvez Junior, and Fabio Mascarenhas,
//! [_Automatic Syntax Error Reporting and Recovery in Parsing Expression Grammars_](https://arxiv.org/abs/1905.02145).
//! The `error_unless!{}` form corresponds to the labelled-expression sugar in the paper.
//! `rust-peg` simplifies tracking the furthest failure position by observing that an error
//! is always reported over a failure even if a previous failure was further through the input;
//! thus there is no need to track the error location separately.
//! `rust-peg` treats failure or error of a recovery expression differently than Medeiros and
//! Mascarenhas - we report the original error (as if there was no recovery expression),
//! whereas Medeiros and Mascarenhas report the failure or error of the recovery expression.
//!
//! ### Precedence climbing
//!
//! `precedence!{ rules... }` provides a convenient way to parse infix, prefix, and postfix
//! operators using the [precedence
//! climbing](http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing)
//! algorithm.
//!
//! ```rust,no_run
//! # peg::parser!{grammar doc() for str {
//! # pub rule number() -> i64 = "..." { 0 }
//! pub rule arithmetic() -> i64 = precedence!{
//!   x:(@) "+" y:@ { x + y }
//!   x:(@) "-" y:@ { x - y }
//!   --
//!   x:(@) "*" y:@ { x * y }
//!   x:(@) "/" y:@ { x / y }
//!   --
//!   x:@ "^" y:(@) { x.pow(y as u32) }
//!   --
//!   n:number() { n }
//!   "(" e:arithmetic() ")" { e }
//! }
//! # }}
//! # fn main() {}
//! ```
//!
//! Each `--` introduces a new precedence level that binds more tightly than previous precedence
//! levels. The levels consist of one or more operator rules each followed by a Rust action
//! expression.
//!
//! The `(@)` and `@` are the operands, and the parentheses indicate associativity. An operator
//! rule beginning and ending with `@` is an infix expression. Prefix and postfix rules have one
//! `@` at the beginning or end, and atoms do not include `@`.
//!
//! ## Input types
//!
//!  The first line of the grammar declares an input type. This is normally
//!  `str`, but  `rust-peg` handles input types through a series of traits. The
//!  library comes with implementations for `str`, `[u8]`, and `[T]`. Define the
//!  traits below to use your own types as input to `peg` grammars:
//!
//!   * [`Parse`] is the base trait required for all inputs. The others are only required to use the
//!     corresponding expressions.
//!   * [`ParseElem`] implements the `[_]` pattern operator, with a method returning the next item of
//!     the input to match.
//!   * [`ParseLiteral`] implements matching against a `"string"` literal.
//!   * [`ParseSlice`] implements the `$()` operator, returning a slice from a span of indexes.
//!
//! As a more complex example, the body of the `peg::parser!{}` macro itself is
//! parsed with `peg`, using a [definition of these traits][gh-flat-token-tree]
//! for a type that wraps Rust's `TokenTree`.
//!
//! [gh-flat-token-tree]: https://github.com/kevinmehall/rust-peg/blob/master/peg-macros/tokens.rs
//!
//! ## End-of-file handling
//!
//! Normally, parsers report an error if the top-level rule matches without consuming all the input.
//! To allow matching a prefix of the input, add the `#[no_eof]` attribute before `pub rule`.
//! Take care to not miss a malformed `x` at the last position if the rule ends with a `x()*`
//! repeat expression.
//!
//! ## Rule parameters
//!
//! Rules can be parameterized with types, lifetimes, and values, just like Rust functions.
//!
//! In addition to Rust values, rules can also accept PEG expression fragments as arguments by using
//! `rule<R>` as a parameter type. When calling such a rule, use `<>` around a PEG expression in the
//! argument list to capture the expression and pass it to the rule.
//!
//! For example:
//!
//! ```rust,no_run
//! # peg::parser!{grammar doc() for str {
//! rule num_radix(radix: u32) -> u32
//!   = n:$(['0'..='9']+) {? u32::from_str_radix(n, radix).or(Err("number")) }
//!
//! rule list<T>(x: rule<T>) -> Vec<T> = "[" v:(x() ** ",") ","? "]" {v}
//!
//! pub rule octal_list() -> Vec<u32> = list(<num_radix(8)>)
//! # }}
//! # fn main() {}
//! ```
//!
//! ## Failure reporting
//!
//! When a match fails, position information is automatically recorded to report a set of
//! "expected" tokens that would have allowed the parser to advance further.
//!
//! Some rules should never appear in error messages, and can be suppressed with `quiet!{e}`:
//! ```rust,no_run
//! # peg::parser!{grammar doc() for str {
//! rule whitespace() = quiet!{[' ' | '\n' | '\t']+}
//! # }}
//! # fn main() {}
//! ```
//!
//! If you want the "expected" set to contain a more helpful string instead of character sets, you
//! can use `quiet!{}` and `expected!()` together:
//!
//! ```rust,no_run
//! # peg::parser!{grammar doc() for str {
//! rule identifier()
//!   = quiet!{[ 'a'..='z' | 'A'..='Z']['a'..='z' | 'A'..='Z' | '0'..='9' ]+}
//!   / expected!("identifier")
//! # }}
//! # fn main() {}
//! ```
//!
//! ## Imports
//!
//! ```rust,no_run
//! mod ast {
//!    pub struct Expr;
//! }
//!
//! peg::parser!{grammar doc() for str {
//!     use self::ast::Expr;
//! }}
//! # fn main() {}
//! ```
//!
//! The grammar may begin with a series of `use` declarations, just like in Rust, which are
//! included in the generated module. Unlike normal `mod {}` blocks, `use super::*` is inserted by
//! default, so you don't have to deal with this most of the time.
//!
//! ## Rustdoc comments
//!
//! `rustdoc` comments with `///` before a `grammar` or `pub rule` are propagated to the resulting
//! module or function:
//!
//! ```rust,no_run
//! # peg::parser!{grammar doc() for str {
//! /// Parse an array expression.
//! pub rule array() -> Vec<i32> = "[...]" { vec![] }
//! # }}
//! # fn main() {}
//! ```
//!
//! As with all procedural macros, non-doc comments are ignored by the lexer and can be used like
//! in any other Rust code.
//!
//! ## Caching and left recursion
//!
//! A `rule` without parameters can be prefixed with `#[cache]` if it is likely
//! to be checked repeatedly in the same position. This memoizes the rule result
//! as a function of input position, in the style of a [packrat
//! parser][wp-peg-packrat].
//!
//! [wp-peg-packrat]: https://en.wikipedia.org/wiki/Parsing_expression_grammar#Implementing_parsers_from_parsing_expression_grammars
//!
//! However, idiomatic code avoids structures that parse the same input
//! repeatedly, so the use of `#[cache]` is often not a performance win. Simple
//! rules may also be faster to re-match than the additional cost of the hash
//! table lookup and insert.
//!
//! For example, a complex rule called `expr` might benefit from caching if used
//! like `expr() "x" / expr() "y" / expr() "z"`, but this could be rewritten to
//! `expr() ("x" / "y" / "z")` which would be even faster.
//!
//! `#[cache_left_rec]` extends the `#[cache]` mechanism with the ability to resolve
//! left-recursive rules, which are otherwise an error.
//!
//! The `precedence!{}` syntax is another way to handle nested operators and avoid
//! repeatedly matching an expression rule.
//!
//! ## Tracing
//!
//! If you pass the `peg/trace` feature to Cargo when building your project, a
//! trace of the rules attempted and matched will be printed to stdout when
//! parsing. For example,
//! ```sh
//! $ cargo run --features peg/trace
//! ...
//! [PEG_TRACE] Matched rule type at 8:5
//! [PEG_TRACE] Attempting to match rule ident at 8:12
//! [PEG_TRACE] Attempting to match rule letter at 8:12
//! [PEG_TRACE] Failed to match rule letter at 8:12
//! ...
//! ```

extern crate peg_macros;
extern crate peg_runtime as runtime;

pub use peg_macros::parser;
pub use runtime::*;
