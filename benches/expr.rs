#![feature(plugin, test)]
#![plugin(peg_syntax_ext)]

extern crate test;

use test::Bencher;

peg_file! parser("expr.rustpeg");

#[bench]
fn expr(b: &mut Bencher) {
	let bench_str = "1+2+3+4*5*6^7^8^(0^1*2+1)";

	b.bytes = bench_str.len() as u64;
	b.iter(|| {
		parser::expr(bench_str).unwrap();
	});
}
