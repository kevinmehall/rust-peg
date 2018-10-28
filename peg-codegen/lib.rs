#![recursion_limit = "192"]

#[macro_use]
extern crate quote;
extern crate proc_macro2;
extern crate codemap;
extern crate codemap_diagnostic;

use codemap::{ CodeMap, Span };
use codemap_diagnostic::{ Diagnostic, Level, SpanLabel, SpanStyle, Emitter, ColorConfig };

mod translate;
mod grammar;

#[cfg(test)]
mod test;

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

