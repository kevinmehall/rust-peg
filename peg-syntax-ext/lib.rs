#![feature(proc_macro_span)]

extern crate peg;
extern crate proc_macro;

use std::fs;
use std::iter;

use proc_macro::{ TokenStream, TokenTree, Span, Delimiter };

#[proc_macro]
pub fn peg(input: TokenStream) -> TokenStream {
    let (name, source, span) = parse_peg_args(input);

    let line = span.start().line;
    let fname = span.source_file().path().display().to_string();

    // Make PEG line numbers match source line numbers
    let source = iter::repeat('\n').take(line - 1).collect::<String>() + &source;

    expand_peg(name, fname, source)
}

/// Parse a TokenStream of the form `name r#""#`
fn parse_peg_args(input: TokenStream) -> (String, String, Span) {
    let mut iter = input.into_iter();
    let name = match iter.next() {
        Some(TokenTree::Ident(i)) => i.to_string(),
        Some(other) => panic!("Expected grammar name, found {}", other),
        None => panic!("Unexpected end of macro input")
    };
    let (body_literal, span) = match iter.next() {
        Some(TokenTree::Literal(l)) => (l.to_string(), l.span()),
        Some(other) => panic!("Expected raw string literal, found {}", other),
        None => panic!("Unexpected end of macro input")
    };
    if !body_literal.starts_with("r#\"") || !body_literal.ends_with("\"#") {
        panic!("Expected raw string literal (`r#\"...\"#`)");
    }
    let body_string = body_literal[3..body_literal.len()-2].to_string();
    match iter.next() {
        None => {}
        Some(_) => panic!("Unexpected trailing tokens in macro")
    }
    (name, body_string, span)
}

#[proc_macro]
pub fn peg_file(input: TokenStream) -> TokenStream {
    let (name, fname, span) = parse_peg_file_args(input);

    if !span.source_file().is_real() {
        panic!("Can't resolve path relative to {:?}", span.source_file().path());
    }
    let caller_path = span.source_file().path();
    let source_path = caller_path.parent().unwrap().join(&fname);

    let source = fs::read_to_string(source_path).expect("Error reading file");

    expand_peg(name, fname, source)
}


/// Parse a TokenStream of the form `name("filename")`
fn parse_peg_file_args(input: TokenStream) -> (String, String, Span) {
    let mut iter = input.into_iter();
    let name = match iter.next() {
        Some(TokenTree::Ident(i)) => i.to_string(),
        Some(other) => panic!("Expected grammar name, found {}", other),
        None => panic!("Unexpected end of macro input")
    };
    let (mut body_iter, span) = match iter.next() {
        Some(TokenTree::Group(ref g)) if g.delimiter() == Delimiter::Parenthesis => (g.stream().into_iter(), g.span()),
        Some(other) => panic!("Expected parenthesis, found {}", other),
        None => panic!("Unexpected end of macro input")
    };
    let body_literal = match body_iter.next() {
        Some(TokenTree::Literal(l)) => l.to_string(),
        Some(other) => panic!("Expected string literal, found {}", other),
        None => panic!("Expected file name")
    };
    if !body_literal.starts_with("\"") || !body_literal.ends_with("\"") {
        panic!("Expected string literal");
    }
    if body_literal.contains("\\") {
        panic!("Unsupported string escape");
    }
    let body_string = body_literal[1..body_literal.len()-1].to_string();
    match iter.next() {
        None => {}
        Some(_) => panic!("Unexpected trailing tokens in macro")
    }

    (name, body_string, span)
}

fn expand_peg(name: String, filename: String, source: String) -> TokenStream {
    let code = match peg::compile(filename, source) {
        Ok(code) => code,
        Err(()) => panic!("Errors above in rust-peg grammar"),
    };

    format!("mod {} {{ {} }}", name, code).parse().unwrap()
}
