use brain_token::parse;
use proc_macro::TokenStream;

#[proc_macro_derive(Brain, attributes(token, regex))]
pub fn brain(input: TokenStream) -> TokenStream {
    let derive = syn::parse_macro_input!(input as syn::DeriveInput);

    parse(derive).into()
}
