extern crate quote;
extern crate proc_macro2;

use quote::quote_spanned;
use proc_macro2::TokenStream;

// This can't use the `peg` crate as it would be a circular dependency, but the generated code in grammar.rs
// requires `::peg` paths.
extern crate peg_runtime as peg;

mod ast;
mod tokens;
mod grammar;
mod translate;
mod analysis;

pub fn compile_tokens(input: TokenStream) -> TokenStream {
    ::std::panic::take_hook();
    let tokens = tokens::FlatTokenStream::new(input);
    let grammar = match grammar::peg::peg_grammar(&tokens) {
        Ok(g) => g,
        Err(err) => {
            let msg = format!("expected {}", err.expected);
            return quote_spanned!(err.location.0=> compile_error!(#msg););
        }
    };

    translate::compile_grammar(&grammar)
}

