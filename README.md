# Parsing Expression Grammars in Rust

This is a simple parser generator based on the [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) formalism.

## Usage

Run `peg input_file.rustpeg` to compile a grammar and generate Rust code on stdout.

## Grammar Syntax

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
`a:e1 b:e2 c:e3 { rust }` - Match e1, e2, e3 in sequence. If they match successfully, run the Rust code in the action and return its result. The variables before the colons in the preceding sequence are bound to the results of the corresponding expressions

Match actions can extract data from the match using these variables:

  * **match_str** - the matched string, as a `&str` slice. Examples:

```
name -> String
  = [a-zA-Z0-9_]+ { match_str.to_string() }
```

```
number -> int
  = [0-9]+ { from_str::<uint>(match_str).unwrap() }
```

  * **start_pos** - the index into the string at which the match starts, inclusive
  * **pos** - the index into the string at which the match ends, exclusive


## To Do

  * Improve parse error reporting
  * Caching
  * Support passing user-specified objects (e.g. filename for source mapping, string interner) into action code
