use parser::parse;
use proc_macro::TokenStream;

mod parser;

#[proc_macro_derive(Brain, attributes(token, regex))]
pub fn brain(input: TokenStream) -> TokenStream {
    parse(input)
}
