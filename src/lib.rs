#![recursion_limit = "192"]

#[macro_use]
extern crate quote;
extern crate codemap;
extern crate codemap_diagnostic;

use std::io;
use std::io::prelude::*;
use std::path::{Path, PathBuf};
use std::convert::AsRef;
use std::fs::File;
use std::process::exit;
use std::env;

use codemap::{ CodeMap, Span };
use codemap_diagnostic::{ Diagnostic, Level, SpanLabel, SpanStyle, Emitter, ColorConfig };

mod translate;
mod grammar;

struct PegCompiler {
    codemap: CodeMap,
    diagnostics: Vec<codemap_diagnostic::Diagnostic>
}

impl PegCompiler {
    fn new() -> PegCompiler {
        PegCompiler {
            codemap: CodeMap::new(),
            diagnostics: vec![],
        }
    }

    fn has_error(&self) -> bool {
        self.diagnostics.iter().any(|d| d.level == Level::Error || d.level == Level::Bug)
    }

    fn span_error(&mut self, error: String, span: Span, label: Option<String>) {
        self.diagnostics.push(Diagnostic {
            level: Level::Error,
            message: error,
            code: None,
            spans: vec![SpanLabel { span, label, style: SpanStyle::Primary }]
        });
    }

    fn span_warning(&mut self, error: String, span: Span, label: Option<String>) {
        self.diagnostics.push(Diagnostic {
            level: Level::Warning,
            message: error,
            code: None,
            spans: vec![SpanLabel { span, label, style: SpanStyle::Primary }]
        });
    }

    fn print_diagnostics(&mut self) {
        if !self.diagnostics.is_empty() {
            let mut emitter = Emitter::stderr(ColorConfig::Auto, Some(&self.codemap));
            emitter.emit(&self.diagnostics[..]);
            self.diagnostics.clear();
        }
    }

    fn compile(&mut self, filename: String, input: String) -> Result<String, ()> {
        let file = self.codemap.add_file(filename, input);

        let ast_items = match grammar::items(&file.source(), file.span) {
            Ok(g) => g,
            Err(e) => {
                self.span_error(
                    "Error parsing language specification".to_owned(),
                    file.span.subspan(e.offset as u64, e.offset as u64),
                    Some(format!("{}", e))
                );
                return Err(())
            }
        };

        let grammar_def = translate::Grammar::from_ast(self, ast_items)?;
        let output_tokens = translate::compile_grammar(self, &grammar_def);

        if self.has_error() {
            Err(())
        } else {
            Ok(output_tokens?.to_string())
        }
    }
}

/// Compile a peg grammar to Rust source, printing errors to stderr
pub fn compile(filename: String, input: String) -> Result<String, ()> {
    let mut compiler = PegCompiler::new();
    let result = compiler.compile(filename, input);
    compiler.print_diagnostics();
    result
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

    let mut compiler = PegCompiler::new();
    let result = compiler.compile(input_path.to_string_lossy().into_owned(), peg_source);
    compiler.print_diagnostics();

    let rust_source = match result {
        Ok(s) => s,
        Err(()) => {
            writeln!(stderr, "Error compiling PEG grammar").unwrap();
            exit(1);
        }
    };

    let out_dir: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    let rust_path = out_dir.join(input_path.file_name().unwrap()).with_extension("rs");

    let mut output_file = File::create(&rust_path).unwrap();
    output_file.write_all(rust_source.as_bytes()).unwrap();
}
