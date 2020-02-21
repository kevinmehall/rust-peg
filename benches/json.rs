#![feature(test)]
extern crate peg;

extern crate test;

use test::Bencher;

peg::parser!(grammar parser() for str {
// JSON grammar (RFC 4627). Note that this only checks for valid JSON and does not build a syntax
// tree.

pub rule json() = _ (object() / array()) _

rule _() = [' ' | '\t' | '\r' | '\n']*
rule value_separator() = _ "," _

rule value()
    = "false" / "true" / "null" / object() / array() / number() / string()

rule object()
    = "{" _ member() ** value_separator() _ "}"

rule member()
    = string() _ ":" _ value()

rule array()
    = "[" _ (value() ** value_separator()) _ "]"

rule number()
    = "-"? int() frac()? exp()? {}

rule int()
    = ['0'] / ['1'..='9']['0'..='9']*

rule exp()
    = ("e" / "E") ("-" / "+")? ['0'..='9']*<1,>

rule frac()
    = "." ['0'..='9']*<1,>

// note: escaped chars not handled
rule string()
    = "\"" (!"\"" [_])* "\""
});

#[bench]
fn json(b: &mut Bencher) {
    let bench_str = r#"
{
	"X": 0.6e2,
	"Y": 5,
	"Z": -5.312344,
	"Bool": false,
	"Bool": true,
	"Null": null,
	"Attr": {
		"Name": "bla",
		"Siblings": [6, 1, 2, {}, {}, {}]
	},
	"Nested Array": [[[[[[[[[]]]]]]]]],
	"Obj": {
		"Child": {
			"A": [],
			"Child": {
				"Child": {}
			}
		}
	}
}
"#;

    b.bytes = bench_str.len() as u64;
    b.iter(|| {
        parser::json(bench_str).unwrap();
    });
}
