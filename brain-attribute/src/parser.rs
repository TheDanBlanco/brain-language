use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

pub fn parse(stream: TokenStream) -> TokenStream {
    let DeriveInput { ident, .. } = parse_macro_input!(stream as DeriveInput);

    println!("Ident: {:?}", ident);

    let expanded = quote! {
        impl #ident {
            fn lex(&self) -> Result<(), String> {
                Ok(())
            }
        }
    };

    TokenStream::from(expanded)
}
