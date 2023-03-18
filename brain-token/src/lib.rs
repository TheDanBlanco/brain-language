use proc_macro2::TokenStream as ProcTokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Meta, NestedMeta};

use crate::attributes::{REGEX, TOKEN};

pub mod attributes;
pub mod lexer;
pub mod stream;
pub mod token;

const BRAIN_WRONG_ATTRIBUTE_ERROR: &str =
    "Brain can only be derived for enums with #[token(\"...\")] or #[regex(\"...\")] attributes";

pub trait Brain: Sized {
    fn lex(input: String) -> Result<stream::TokenStream<Self>, Box<dyn std::error::Error>>;
}

pub fn parse(input: DeriveInput) -> ProcTokenStream {
    let ident = input.ident;

    if let Data::Enum(data) = input.data.clone() {
        let mut attributes = Vec::new();

        for variant in data.variants {
            let variant_ident = variant.ident;
            let mut attribute = quote! { None };
            for attr in variant.attrs {
                if let Meta::List(list) = attr.parse_meta().expect(BRAIN_WRONG_ATTRIBUTE_ERROR) {
                    let attribute_ident = match list.path.get_ident() {
                        Some(id) => match id.to_string().as_str() {
                            TOKEN | REGEX => id,
                            _ => panic!("{}", BRAIN_WRONG_ATTRIBUTE_ERROR),
                        },
                        None => panic!("{}", BRAIN_WRONG_ATTRIBUTE_ERROR),
                    };

                    let attr_ident = attribute_ident.to_string();

                    for meta in list.nested {
                        if let NestedMeta::Lit(lit) = meta {
                            if let syn::Lit::Str(lit) = lit {
                                attribute = quote! {
                                    lexer.add_attribute(Attribute::new(#ident::#variant_ident, #attr_ident.to_string(), #lit.to_string()));
                                };
                            } else {
                                panic!("{}", BRAIN_WRONG_ATTRIBUTE_ERROR);
                            }
                        } else {
                            panic!("{}", BRAIN_WRONG_ATTRIBUTE_ERROR);
                        }
                    }
                } else {
                    panic!("{}", BRAIN_WRONG_ATTRIBUTE_ERROR);
                }
            }

            attributes.push(attribute);
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

    panic!("Brain can only be derived for enums");
}

// how do you even test this LOL
