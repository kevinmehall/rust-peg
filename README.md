# Parsing Expression Grammars in Rust

This is a simple parser generator based on the [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) formalism.

## Usage

`rust-peg` relies on the unstable `libsyntax` crate, and only works on Nightly builds of Rust.
However, generated parsers are compatible with 1.0 stable.

### As a syntax extension
Add to your Cargo.toml:

```
[dependencies]
peg = "0.1.0"
```

Add to your crate root:
```
#![feature(plugin)]
#![plugin(peg_syntax_ext)]
```

Use `peg_file! modname("mygrammarfile.rustpeg");` to include the grammar from an external file. The macro expands into a module called `modname` with functions corresponding to the `#[pub]` rules in your grammar.

Or, use
```
peg! modname(r#"
  // grammar rules here
"#);`
```

to embed a short PEG grammar inline in your Rust source file. [Example](tests/test_arithmetic.rs).

### As a standalone code generator
Run `peg input_file.rustpeg` to compile a grammar and generate Rust code on stdout.

## Grammar Syntax

```
use super::name;
```

The grammar may begin with a series of `use` declarations, just like in Rust, which are included in
the generated module. Since the grammar is in its own module, you must `use super::StructName;` to
access a structure from the parent module.

```
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

Match actions can extract data from the match using these variables:

  * **match_str** - the matched string, as a `&str` slice. Examples:

```
name -> String
  = [a-zA-Z0-9_]+ { match_str.to_string() }
```

```
number -> int
  = [0-9]+ { from_str::<u64>(match_str).unwrap() }
```

  * **start_pos** - the byte index into the string at which the match starts, inclusive
  * **pos** - the byte index into the string at which the match ends, exclusive


## To Do

  * Improve parse error reporting
  * Memoization
  * Support passing user-specified objects (e.g. filename for source mapping, string interner) into action code
