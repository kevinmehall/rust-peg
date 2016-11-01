# Parsing Expression Grammars in Rust

This is a simple parser generator based on the [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) formalism.

## Grammar Definition Syntax

```rust
use super::name;
```

The grammar may begin with a series of `use` declarations, just like in Rust, which are included in
the generated module. Since the grammar is in its own module, you must `use super::StructName;` to
access a structure from the parent module.

```rust
#[pub]
rule_name -> type
   = expression
```

If a rule is marked with `#[pub]`, the generated module has a public function that begins parsing at that rule.

  * `.` - match any single character
  * `"literal"` - match a literal string
  * `[a-z]`  - match a single character from a set
  * `[^a-z]` - match a single character not in a set
  * `rule` - match a production defined elsewhere in the grammar and return its result
  * `expression*` - Match zero or more repetitions of `expression` and return the results as a `Vec`
  * `expression+` - Match one or more repetitions of `expression` and return the results as a `Vec`
  * `expression?` - Match one or zero repetitions of `expression`. Returns an `Option`
  * `&expression` - Match only if `expression` matches at this position, without consuming any characters
  * `!expression` - Match only if `expression` does not match at this position, without consuming any characters
  * `expression ** delim` - Match zero or more repetitions of `expression` delimited with `delim` and return the results as a `Vec`
  * `expression ++ delim` - Match one or more repetitions of `expression` delimited with `delim` and return the results as a `Vec`
  * `e1 / e2 / e3` - Try to match e1. If the match succeeds, return its result, otherwise try e2, and so on.
  * `e1 e2 e3` - Match expressions in sequence
  * `a:e1 b:e2 c:e3 { rust }` - Match e1, e2, e3 in sequence. If they match successfully, run the Rust code in the block and return its return value. The variable names before the colons in the preceding sequence are bound to the results of the corresponding expressions. The Rust code must contain matched curly braces, including those in strings and comments.
  * `a:e1 b:e2 c:e3 {? rust }` - Like above, but the Rust block returns a `Result` instead of a value directly. On `Ok(v)`, it matches successfully and returns `v`. On `Err(e)`, the match of the entire expression fails and it tries alternatives or reports a parse error with the `&str` `e`.
  * `$(e)` - matches the expression e, and returns the `&str` slice of the input string corresponding to the match
  * `#position` - returns a `usize` representing the current offset into the input string, and consumes no characters

## Usage

### With a build script

A Cargo build script can compile your PEG grammar to Rust source automatically.

Add to your `Cargo.toml`:

```
# Under [package]
build = "build.rs"

[build-dependencies]
peg = { version = "0.4" }
```

Create `build.rs` with:

```
extern crate peg;

fn main() {
    peg::cargo_build("src/my_grammar.rustpeg");
}
```

And import the generated code:

```
mod my_grammar {
    include!(concat!(env!("OUT_DIR"), "/my_grammar.rs"));
}
```


### As a syntax extension

`rust-syntax-ext` only works on Nightly builds of Rust.

Add to your Cargo.toml:

```toml
[dependencies]
peg-syntax-ext = "0.4.0"
```

Add to your crate root:
```rust
#![feature(plugin)]
#![plugin(peg_syntax_ext)]
```

Use `peg_file! modname("mygrammarfile.rustpeg");` to include the grammar from an external file. The macro expands into a module called `modname` with functions corresponding to the `#[pub]` rules in your grammar.

Or, use
```rust
peg! modname(r#"
  // grammar rules here
"#);`
```

to embed a short PEG grammar inline in your Rust source file. [Example](peg-syntax-ext/tests/test_arithmetic.rs).

### As a standalone code generator

Run `peg input_file.rustpeg` to compile a grammar and generate Rust code on stdout.

## Tracing

If you pass the `peg/trace` feature to Cargo when building your project, a trace of the parsing will be output to stdout when running the binary. For example,
```
$ cargo run --features peg/trace
...
[PEG_TRACE] Matched rule type at 8:5
[PEG_TRACE] Attempting to match rule ident at 8:12
[PEG_TRACE] Attempting to match rule letter at 8:12
[PEG_TRACE] Failed to match rule letter at 8:12
...
```

## Migrating from 0.3

* If you were using the syntax extension, replace `peg = "0.3.0"` with `peg-syntax-ext = "0.4.0"` in your `Cargo.toml`'s `[dependencies]` section. The library name in `#![plugin(peg_syntax_ext)]` remains the same. Consider moving to the build script for compatibility with Rust stable.

* The `match_str` variable has been removed in favor of the `$(expr)` syntax.  Replace `[0-9]+ { match_str.parse().unwrap() }` with `n:$([0-9]+) { n.parse().unwrap() } `

* `start_pos` and `pos` variables have been removed. Use `#position` as an expression, which returns a `usize` offset into the source string. Replace `foo:x { Span(start_pos, pos, foo) }` with `start:#position foo:x end:#position { Span(start, end, foo) }`
