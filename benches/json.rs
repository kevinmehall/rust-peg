#![feature(test)]
extern crate peg;
use peg::peg;

extern crate test;

use test::Bencher;

peg!(parser r#"
// JSON grammar (RFC 4627). Note that this only checks for valid JSON and does not build a syntax
// tree.

pub json = object / array

ws = [' ' | '\t' | '\r' | '\n']*
begin_array = ws "[" ws
begin_object = ws "{" ws
end_array = ws "]" ws
end_object = ws "}" ws
name_separator = ws ":" ws
value_separator = ws "," ws

value
    = "false" / "true" / "null" / object / array / number / string

object
    = begin_object (member (value_separator member)*)? end_object

member
    = string name_separator value

array
    = begin_array (value (value_separator value)*)? end_array

number
    = "-"? int frac? exp? {}

int
    = ['0'] / ['1'..='9']['0'..='9']*

exp
    = ("e" / "E") ("-" / "+")? ['0'..='9']*<1,>

frac
    = "." ['0'..='9']*<1,>

// note: escaped chars not handled
string
    = "\"" (!"\"" .)* "\""
"#);

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
