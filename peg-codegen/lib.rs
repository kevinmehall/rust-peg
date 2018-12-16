#![recursion_limit = "192"]

#[macro_use]
extern crate quote;
extern crate proc_macro2;
use proc_macro2::TokenStream;

// This can't use the `peg` crate as it would be a circular dependency, but the generated code in grammar.rs
// requires `::peg` paths.
extern crate peg_runtime as peg;

#[cfg(test)]
mod test;
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
            let msg = err.to_string();
            return quote_spanned!(err.location.0=> compile_error!(#msg););
        }
    };

    let mut errors = Vec::new();
    analysis::check(&grammar, &mut |err| errors.push(err.to_compile_error()));
    let res = translate::compile_grammar(&grammar);

    quote! {
        #res
        #(#errors)*
    }
}

