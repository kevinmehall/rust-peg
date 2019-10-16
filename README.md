# Parsing Expression Grammars in Rust

[Documentation](https://docs.rs/peg) | [Release Notes](https://github.com/kevinmehall/rust-peg/releases)

`rust-peg` is a simple yet flexible parser generator based on the [Parsing Expression Grammar](https://en.wikipedia.org/wiki/Parsing_expression_grammar) formalism. It provides a Rust macro that builds a recursive descent parser from a concise definition of the grammar.

## Features

- Parse input from `&str`, `&[u8]`, `&[T]` or custom types implementing traits
- Customizable reporting of parse errors
- Rules can accept arguments to create reusable rule templates
- Precedence climbing for prefix/postfix/infix expressions
- Helpful `rustc` error messages for errors in the grammar definition or the Rust
  code embedded within it
- Rule-level tracing to debug grammars

## Example

```rust
use peg::parser;

parser!{
  grammar list_parser() for str {
    rule number() -> u32
      = n:$(['0'..='9']+) { n.parse().unwrap() }

    pub rule list() -> Vec<u32>
      = "[" l:number() ** "," "]" { l }
  }
}

pub fn main() {
    assert_eq!(list_parser::list("[1,1,2,3,5,8]"), Ok(vec![1, 1, 2, 3, 5, 8]));
}
```

[See the tests for more examples](./tests/run-pass/)  
[Grammar rule syntax reference in rustdoc](https://docs.rs/peg)

## Comparison with similar parser generators

| crate     | parser type | action code | integration        | input type             | precedence climbing | parameterizd rules | streaming input |
| --------- | ----------- | ----------- | ------------------ | ---------------------- | ------------------- | ------------------ | --------------- |
| peg       | PEG         | in grammar  | proc macro (block) | `&str`, `&[T]`, custom | Yes                 | Yes                | No              |
| [pest]    | PEG         | external    | proc macro (file)  | `&str`                 | Yes                 | No                 | No              |
| [nom]     | combinators | in source   | library            | `&[u8]`, custom        | No                  | Yes                | Yes             |
| [lalrpop] | LR(1)       | in grammar  | build script       | `&str`                 | No                  | Yes                | No              |

[pest]: https://github.com/pest-parser/pest
[nom]: https://github.com/geal/nom
[lalrpop]: https://github.com/lalrpop/lalrpop
