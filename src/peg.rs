#![feature(quote, box_syntax, core, collections, rustc_private, io, old_path, box_patterns, exit_status)]
extern crate syntax;

use std::io::{Read, Write};
use std::io::{stdin,stdout,stderr};
use std::fs::File;
use std::env;
use translate::{compile_grammar};

mod translate;
mod grammar;
mod rustast;
mod fake_extctxt;

fn print_usage(prog: &str) {
	println!("Usage: {} [file]", prog)
}

fn main() {
	let args = env::args_os().collect::<Vec<_>>();
	let progname = args[0].to_str().unwrap();

	let mut source = String::new();
	match &args[1..] {
		[ref arg] if arg.to_str() == Some("-h") => return print_usage(progname),
		[ref fname] => File::open(&Path::new(fname.to_str().unwrap())).unwrap().read_to_string(&mut source).unwrap(),
		[] => stdin().read_to_string(&mut source).unwrap(),
		_ => return print_usage(progname),
	};

	let grammar_def = grammar::grammar(&source);

	match grammar_def {
		Ok(grammar) => {
			fake_extctxt::with_fake_extctxt(|e| {

				let ast = compile_grammar(e, &grammar);
				let mut out = stdout();

				writeln!(&mut out, "// Generated by rust-peg. Do not edit.").unwrap();
				writeln!(&mut out, "#![allow(non_snake_case, unused)]").unwrap();

				for item in ast.items.iter() {
					writeln!(&mut out, "{}", rustast::item_to_string(&**item).as_slice()).unwrap();
				}
			})
		}

		Err(msg) => {
			let mut e = stderr();
			(writeln!(&mut e, "Error parsing language specification: {}", msg)).unwrap();
			env::set_exit_status(1);
		}
	}
}
