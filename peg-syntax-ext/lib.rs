extern crate peg_codegen;
extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro]
pub fn parser(input: TokenStream) -> TokenStream {
    peg_codegen::compile_tokens(input.into()).into()
}
