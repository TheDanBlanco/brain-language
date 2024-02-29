use brain_token::parse;
use proc_macro::TokenStream;
use syn::parse_macro_input;

#[proc_macro_derive(Brain, attributes(token, regex))]
pub fn brain(input: TokenStream) -> TokenStream {
    let derived = parse_macro_input!(input as syn::DeriveInput);

    let tokens = parse(derived);

    tokens.into()
}
