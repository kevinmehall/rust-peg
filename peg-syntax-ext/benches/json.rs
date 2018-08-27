#![feature(test, proc_macro_gen)]
extern crate peg_syntax_ext;
use peg_syntax_ext::peg_file;

extern crate test;

use test::Bencher;

peg_file!(parser("json.rustpeg"));

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
