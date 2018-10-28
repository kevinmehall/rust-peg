extern crate peg_codegen;
extern crate proc_macro;

use proc_macro::{ TokenStream, TokenTree, Span };

#[proc_macro]
pub fn peg(input: TokenStream) -> TokenStream {
    let (name, source, _) = parse_peg_args(input);

    expand_peg(name, "<peg>".into(), source)
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

fn expand_peg(name: String, filename: String, source: String) -> TokenStream {
    let code = match peg_codegen::compile(filename, source) {
        Ok(code) => code,
        Err(()) => panic!("Errors above in rust-peg grammar"),
    };

    format!("mod {} {{ {} }}", name, code).parse().unwrap()
}
