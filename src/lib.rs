#![recursion_limit = "192"]

#[macro_use]
extern crate quote;

use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::fs::File;
use std::process::exit;
use std::env;

mod translate;
mod grammar;

/// Compile a peg grammar to Rust source
pub fn compile(input: &str) -> Result<String, String> {
    let grammar_def = match grammar::grammar(&input) {
        Ok(g) => g,
        Err(msg) => {
            return Err(format!("Error parsing language specification: {}", msg))
        }
    };

    let output_tokens = translate::compile_grammar(&grammar_def);
    Ok(output_tokens?.to_string())
}

/// Compile the PEG grammar in the specified filename to cargo's OUT_DIR.
/// Errors are emitted to stderr and terminate the process.
pub fn cargo_build<T: AsRef<Path> + ?Sized>(input_path: &T) {
    let mut stderr = io::stderr();
    let input_path = input_path.as_ref();

    let mut peg_source = String::new();
    if let Err(e) = File::open(input_path).and_then(|mut x| x.read_to_string(&mut peg_source)) {
        writeln!(stderr, "Could not read PEG input file `{}`: {}", input_path.display(), e).unwrap();
        exit(1);
    }

    println!("cargo:rerun-if-changed={}", input_path.display());

    let rust_source = match compile(&peg_source) {
        Ok(s) => s,
        Err(e) => {
            writeln!(stderr, "Error compiling PEG grammar `{}`:\n\t{}", input_path.display(), e).unwrap();
            exit(1);
        }
    };

    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let rust_path = out_dir.join(input_path.file_name().unwrap()).with_extension("rs");

    let mut output_file = File::create(&rust_path).unwrap();
    output_file.write_all(rust_source.as_bytes()).unwrap();
}
