//! `rust-peg` is a simple yet flexible parser generator based on the [Parsing Expression
//! Grammar][wikipedia-peg] formalism. It provides the `parser!{}` macro that builds a recursive
//! descent parser from a concise definition of the grammar.
//!
//! [wikipedia-peg]: https://en.wikipedia.org/wiki/Parsing_expression_grammar
//!
//! The `parser!{}` macro encloses a `grammar` definition containing a set of
//! `rule`s which match components of your language. The grammar is defined over
//! an [input type, normally `str`](#custom-input-types), and expands to a Rust
//! `mod`.
//! 
//! Rules can accept parameters and optionally return a value when they match. A
//! `rule` not marked `pub` can only be called from other rules within the
//! grammar.
//! 
//! Each `rule` marked `pub` expands to a function in the module which
//! accepts a reference to an input sequence, followed by any additional
//! parameters defined on the `rule`. It returns a `Result<T, ParseError>`
//! carrying either the successfully parsed value, or a `ParseError` containing
//! the failure position and the set of tokens expected there.
//! 
//! The body of the rule, following the `=`, is a PEG expression, definining how
//! the input is matched to produce a value.
//! 
//! ```rust
//! peg::parser!{
//!   grammar list_parser() for str {
//!     rule number() -> u32
//!       = n:$(['0'..='9']+) { n.parse().unwrap() }
//!
//!     pub rule list() -> Vec<u32>
//!       = "[" l:number() ** "," "]" { l }
//!   }
//! }
//!
//! pub fn main() {
//!     assert_eq!(list_parser::list("[1,1,2,3,5,8]"), Ok(vec![1, 1, 2, 3, 5, 8]));
//! }
//! ```
//! 
//! ## Expressions
//!
//!   * `"keyword"` - _Literal:_ match a literal string.
//!   * `['0'..='9']`  - _Pattern:_ match a single element that matches a Rust `match`-style
//!     pattern. [(details)](#match-expressions)
//!   * `some_rule()` - _Rule:_ match a rule defined elsewhere in the grammar and return its
//!     result. Arguments in the parentheses are Rust expressions.
//!   * `_` or `__` or `___`: _Rule (underscore):_ As a special case, rule names
//!     consisting of underscores are invoked without parentheses. These are
//!     conventionally used to match whitespace between tokens.
//!   * `e1 e2 e3` - _Sequence:_ match expressions in sequence (`e1` followed by `e2` followed by
//!     `e3`).
//!   * `a:e1 e2 b:e3 c:e4 { rust }` - _Action:_ Match `e1`, `e2`, `e3`, `e4` in
//!     sequence, like above. If they match successfully, run the Rust code in
//!     the block and return its return value. The variable names before the
//!     colons in the preceding sequence are bound to the results of the
//!     corresponding expressions. It is important that the Rust code embedded
//!     in the grammar is deterministic and free of side effects, as it may be
//!     called multiple times.
//!   * `a:e1 b:e2 c:e3 {? rust }` - _Conditional action:_ Like above, but the
//!     Rust block returns a `Result<T, &str>` instead of a value directly. On
//!     `Ok(v)`, it matches successfully and returns `v`. On `Err(e)`, the match
//!     of the entire expression fails and it tries alternatives or reports a
//!     parse error with the `&str` `e`.
//!   * `e1 / e2 / e3` - _Ordered choice:_ try to match `e1`. If the match succeeds, return its
//!     result, otherwise try `e2`, and so on.
//!   * `expression?` - _Optional:_ match one or zero repetitions of `expression`. Returns an
//!     `Option`.
//!   * `expression*` - _Repeat:_ match zero or more repetitions of `expression` and return the
//!     results as a `Vec`.
//!   * `expression+` - _One-or-more:_ match one or more repetitions of `expression` and return the
//!     results as a `Vec`.
//!   * `expression*<n,m>` - _Range repeat:_ match between `n` and `m` repetitions of `expression`
//!     return the results as a `Vec`. [(details)](#repeat-ranges)
//!   * `expression ** delim` - _Delimited repeat:_ match zero or more repetitions of `expression`
//!     delimited with `delim` and return the results as a `Vec`.
//!   * `&expression` - _Positive lookahead:_ Match only if `expression` matches at this position,
//!     without consuming any characters.
//!   * `!expression` - _Negative lookahead:_ Match only if `expression` does not match at this
//!     position, without consuming any characters.
//!   * `$(e)` - _Slice:_ match the expression `e`, and return the `&str` slice of the input
//!     corresponding to the match.
//!   * `position!()` - return a `usize` representing the current offset into the input, and
//!     consumes no characters.
//!   * `quiet!{ e }` - match expression, but don't report literals within it as "expected" in
//!     error messages.
//!   * `expected!("something")` - fail to match, and report the specified string as an expected
//!     symbol at the current location.
//!   * `precedence!{ ... }` - Parse infix, prefix, or postfix expressions by precedence climbing.
//!     [(details)](#precedence-climbing)
//!
//! ### Match expressions
//!
//! The `[pat]` syntax expands into a [Rust `match`
//! pattern](https://doc.rust-lang.org/book/ch18-03-pattern-syntax.html) against the next character
//! (or element) of the input.
//!
//! This is commonly used for matching sets of characters with Rust's `..=` inclusive range pattern
//! syntax and `|` to match multiple patterns. For example `['a'..='z' | 'A'..='Z']` matches an
//! upper or lower case ASCII alphabet character.
//!
//! If your input type is a slice of an enum type, a pattern could match an enum variant like
//! `[Token::Operator('+')]`.
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
//! ## Custom input types
//!
//! `rust-peg` handles input types through a series of traits, and comes with implementations for
//! `str`, `[u8]`, and `[T]`. Define the traits below to use your own types as
//! input to `peg` grammars:
//!
//!   * `Parse` is the base trait required for all inputs. The others are only required to use the
//!     corresponding expressions.
//!   * `ParseElem` implements the `[_]` pattern operator, with a method returning the next item of
//!     the input to match.
//!   * `ParseLiteral` implements matching against a `"string"` literal.
//!   * `ParseSlice` implements the `$()` operator, returning a slice from a span of indexes.
//!
//! As a more complex example, the body of the `peg::parser!{}` macro itself is
//! parsed with `peg`, using a [definition of these traits][gh-flat-token-tree]
//! for a type that wraps Rust's `TokenTree`.
//! 
//! [gh-flat-token-tree]: https://github.com/kevinmehall/rust-peg/blob/master/peg-macros/tokens.rs
//! 
//! ### Error reporting
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
