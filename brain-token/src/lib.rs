use proc_macro2::TokenStream as ProcTokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, Data, LitStr, Meta, Token};

pub mod attributes;
pub mod lexer;
pub mod stream;
pub mod token;

pub trait Brain: Sized {
    fn lex(input: String) -> Result<stream::TokenStream<Self>, Box<dyn std::error::Error>>;
}

pub fn parse(input: syn::DeriveInput) -> ProcTokenStream {
    let ident = &input.ident;

    let data = match &input.data {
        Data::Enum(data) => data,
        _ => panic!("Brain can only be implemented for enums"),
    };

    let mut attributes = vec![];

    for variant in &data.variants {
        let variant_ident = &variant.ident;

        for attr in &variant.attrs {
            let list = match &attr.meta {
                Meta::List(list) => list,
                _ => unreachable!(),
            };

            let attribute_ident = list
                .path
                .get_ident()
                .expect("Expected attribute identifier")
                .to_string();

            let nested = list
                .parse_args_with(Punctuated::<LitStr, Token![,]>::parse_terminated)
                .expect("Error parsing nested meta");

            for meta in nested {
                let meta_attribute = meta.value();

                attributes.push(quote! {
                    lexer.add_attribute(
                        Attribute::new(
                            #ident::#variant_ident,
                            String::from(#attribute_ident),
                            String::from(#meta_attribute)
                        )
                    );
                });
            }
        }
    }

    let expanded = quote! {
        #[automatically_derived]
        use ::brain_token::Brain;

        impl Brain for #ident {
            fn lex(input: String) -> Result<::brain_token::stream::TokenStream<Self>, Box<dyn std::error::Error>> {
                use ::brain_token::{lexer::Lexer, attributes::Attribute};

                let mut lexer = Lexer::<Self>::new();

                #(#attributes)*

                lexer.lex(input)
            }
        }

    };

    return expanded.into();
}
