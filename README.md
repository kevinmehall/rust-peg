# Parsing Expression Grammars in Rust

This is a simple parser generator based on [Parsing Expression Grammars](https://en.wikipedia.org/wiki/Parsing_expression_grammar).

Please see the [release notes](https://github.com/kevinmehall/rust-peg/releases) for updates.

## Grammar Definition Syntax

```rust
use super::name;
```

The grammar may begin with a series of `use` declarations, just like in Rust, which are included in
the generated module. Since the grammar is in its own module, you must `use super::StructName;` to
access a structure from the parent module.

The remainder of the grammar defines a series of grammar rules which match components of
your language:

```rust
pub rule_name -> return_type
   = expression
```

If a rule is marked with `pub`, the generated module has a public function that begins parsing at that rule.

### Expressions

  * `"literal"` - match a literal string
  * `[a-zA-Z]`  - match a single character from a set
  * `[^a-zA-Z]` - match a single character not in a set
  * `.` - match any single character
  * `some_rule` - match a rule defined elsewhere in the grammar and return its result
  * `some_template<arg1, arg2>` - Expand a [template rule](#template-rules) with parameters
  * `e1 e2 e3` - Match expressions in sequence
  * `e1 / e2 / e3` - Try to match e1. If the match succeeds, return its result, otherwise try e2, and so on.
  * `expression?` - Match one or zero repetitions of `expression`. Returns an `Option`
  * `expression*` - Match zero or more repetitions of `expression` and return the results as a `Vec`
  * `expression+` - Match one or more repetitions of `expression` and return the results as a `Vec`
  * `expression*<n>` - Match `n` repetitions of `expression` and return the results as a `Vec`
  * `expression*<n,m>` - Match between `n` and `m` repetitions of `expression` and return the results as a `Vec`.
  * `expression*<{foo}>` - Evaluate the Rust expression `foo` returning usize and match that many repetitions of `expression`
  * `expression ** delim` - Match zero or more repetitions of `expression` delimited with `delim` and return the results as a `Vec`
  * `expression **<n,m> delim` - Match between `n` and `m` repetitions of `expression` delimited with `delim` and return the results as a `Vec`
  * `expression ++ delim` - Match one or more repetitions of `expression` delimited with `delim` and return the results as a `Vec`
  * `&expression` - Match only if `expression` matches at this position, without consuming any characters
  * `!expression` - Match only if `expression` does not match at this position, without consuming any characters
  * `a:e1 b:e2 c:e3 { rust }` - Match e1, e2, e3 in sequence. If they match successfully, run the Rust code in the block and return its return value. The variable names before the colons in the preceding sequence are bound to the results of the corresponding expressions. The Rust code must contain matched curly braces, including those in strings and comments.
  * `a:e1 b:e2 c:e3 {? rust }` - Like above, but the Rust block returns a `Result` instead of a value directly. On `Ok(v)`, it matches successfully and returns `v`. On `Err(e)`, the match of the entire expression fails and it tries alternatives or reports a parse error with the `&str` `e`.
  * `$(e)` - matches the expression `e`, and returns the `&str` slice of the input string corresponding to the match
  * `#position` - returns a `usize` representing the current offset into the input string, and consumes no characters
  * `#quiet<expression>` - match expression, but don't report literals within it as "expected" in error messages.
  * `#expected("str")` - fails to match, and report the specified string as an expected symbol at the current location.
  * `#infix<atom> { ... }` - Parse infix expressions by precedence climbing. [Details below](#infix-expressions).

### Comments
You can use line comments and block comments just as in Rust code, for example:

```rust
// comment
name -> String
  = /* weirdly placed comment */ n:$([0-9]+) { from_str::<u64>(n).unwrap() } // comment
```

### Error reporting

When a match fails, position information is automatically recorded to report a set of "expected" tokens that would have allowed the parser to advance further.

Some rules should never appear in error messages, and can be suppressed with `#quiet<e>`:
```rust
whitespace = #quiet<[ \n\t]+>.
```

If you want the "expected" set to contain a more helpful string instead of character sets, you
can use `#quiet` and `#expected` together:

```rust
identifier = #quiet<[a-zA-Z][a-zA-Z0-9_]+> / #expected("identifier")
```

### Infix expressions

`#infix<atom> { rules... }` provides a convenient way to parse binary infix operators using the [precedence climbing](http://eli.thegreenplace.net/2012/08/02/parsing-expressions-by-precedence-climbing) algorithm.

```rust
pub arithmetic -> i64 = #infix<number> {
	#L x "+" y { x + y }
	   x "-" y { x - y }
	#L x "*" y { x * y }
	   x "/" y { x / y }
	#R x "^" y { x.pow(y as u32) }
}

```

The atom (in this example, `number`), is the rule used to parse the "things between the operators". It must be a name of a rule defined elsewhere in the grammar, not an expression, because its return type is used in the generated code. Each operator's action code and the `#infix` expression as a whole return the same type as this rule. When building an AST, this type is commonly an enum with a variant for the atom, and variant(s) for operators.

Each `#L` or `#R` introduces a new precedence level that binds more tightly than previous precedence levels, and contains left (`#L`) or right (`#R`) associative operators. Each operator rule consists of a left operand variable name, an operator expression, a right variable name, and a Rust action expression. The Rust code has access to the two named operands.

You can think of the above example as a PEG parser `number (("+" / "-" / "*" / "/" / "^") number)*`,
followed by precedence climbing to associate the atoms and operators into a tree following associativity and precedence rules, and running the action code for each level of the tree. No intermediate vector is allocated.

### Template rules

Rule templating can reduce duplicated code in your grammar:

Example:
```rust
keyword<E> = E !identifierChar whitespace*

STRUCT = keyword<"struct">
ENUM = keyword<"enum">
```

Templates are inlined every place they are used, and cannot be marked `pub`.

### Context arguments

You can pass parameters throughout the parser by adding a declaration like

```
#![arguments(filename: &str, interner: &mut Interner)]
```

to the top of your grammar. All public parse functions will take these additional arguments after
the input string parameter, and they are available by name in all action code blocks.

For an example see [the test](peg-syntax-ext/tests/grammar_args.rs).

The arguments will be passed to each internal parse function, so they must be `Copy` or be a `&`
or `&mut` reference. Be careful with mutable arguments. Remember that rule actions can run on parse
paths that later fail and do not contribute to the final parse.

## Usage

### With a build script

A Cargo build script can compile your PEG grammar to Rust source automatically. This method works
on stable Rust.

[Example crate using rust-peg with a build script](peg-tests/)

Add to your `Cargo.toml`:

```toml
[package] # this section should already exist
build = "build.rs"

[build-dependencies]
peg = { version = "0.5" }
```

Create `build.rs` with:

```rust
extern crate peg;

fn main() {
    peg::cargo_build("src/my_grammar.rustpeg");
}
```

(If you already have a `build.rs`, just add the `extern crate peg;` and the `peg::cargo_build(...)` call.)

And import the generated code:

```rust
mod my_grammar {
    include!(concat!(env!("OUT_DIR"), "/my_grammar.rs"));
}
```

You can then use the `pub` rules in the grammar as functions in the module. They accept a `&str` to parse, and return a `Result` with the parse result or parse error.

```rust
match my_grammar::my_rule("2 + 2") {
  Ok(r) => println!("Parsed as: {:?}", r),
  Err(e) => println!("Parse error: {}", e),
}
```

### As a syntax extension

`rust-syntax-ext` only works on Nightly builds of Rust.

[Examples using rust-peg as a syntax extension](peg-syntax-ext/tests/)

Add to your Cargo.toml:

```toml
[dependencies]
peg-syntax-ext = "0.5.0"
```

Add to your crate root:
```rust
#![feature(plugin)]
#![plugin(peg_syntax_ext)]
```

Use `peg_file! modname("mygrammarfile.rustpeg");` to include the grammar from an external file. The macro expands into a module called `modname` with functions corresponding to the `pub` rules in your grammar.

Or, use
```rust
peg! modname(r#"
  // grammar rules here
"#);`
```

to embed a short PEG grammar inline in your Rust source file. [Example](peg-syntax-ext/tests/test_arithmetic.rs).

### As a standalone code generator

Run `rust-peg input_file.rustpeg` to compile a grammar and generate Rust code on stdout.

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

# Editor highlighting plugins

Users have created text editor syntax highlighting plugins for the `.rustpeg` syntax:

* [vim plugin](https://github.com/treycordova/rustpeg.vim) by Trey Cordova
* [vim plugin](https://github.com/rhysd/vim-rustpeg) by rhysd
