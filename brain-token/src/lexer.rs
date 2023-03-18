use brain_error::{Error, ErrorKind};
use regex::Regex;

use crate::{attributes::Attribute, stream::TokenStream, token::Token};

pub struct Lexer<T> {
    pub tokens: TokenStream<T>,
    pub start: usize,
    pub current: usize,
    pub input: String,
    pub attributes: Vec<Attribute<T>>,
}

impl<T> Lexer<T>
where
    T: Clone + std::fmt::Debug + PartialEq,
{
    pub fn new() -> Self {
        Lexer {
            tokens: TokenStream::<T>::new(),
            start: 0,
            current: 0,
            input: "".to_string(),
            attributes: vec![],
        }
    }

    pub fn add_attribute(&mut self, attribute: Attribute<T>) {
        self.attributes.push(attribute);
    }

    pub fn lex(&mut self, input: String) -> Result<TokenStream<T>, Box<dyn std::error::Error>> {
        self.input = input;

        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token()?;
        }

        Ok(self.tokens.clone())
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn scan_token(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        for attribute in &self.attributes {
            match attribute {
                Attribute::Literal(literal) => {
                    let end = self.current + literal.literal.len();
                    if end > self.input.len() {
                        continue;
                    }

                    let span = self.start..end;
                    let next = &self.input[span.clone()];

                    if literal.literal != (next) {
                        continue;
                    }

                    self.tokens.push(Token::new(
                        span.clone(),
                        literal.token.clone(),
                        Some(next.to_string()),
                    ));
                    self.current += next.len();
                    self.skip_whitespace();
                    return Ok(());
                }
                Attribute::Regex(regex_attribute) => {
                    let regex = Regex::new(&regex_attribute.regex).unwrap();
                    let next = &self.input[self.current..];

                    if regex.captures(next).is_none() {
                        continue;
                    }

                    let captures = regex.captures(next).unwrap();
                    let capture = captures.get(0).unwrap().as_str();
                    let span = self.current..(self.current + capture.len());
                    let next = &self.input[span.clone()];

                    if (next) != capture {
                        continue;
                    }

                    self.tokens.push(Token::new(
                        span.clone(),
                        regex_attribute.token.clone(),
                        Some(capture.to_string()),
                    ));
                    self.current += capture.len();
                    self.skip_whitespace();
                    return Ok(());
                }
            }
        }

        return Err(Error::new(
            ErrorKind::ParseError,
            format!(
                "No remaining tokens matched for : {}",
                &self.input[self.current..]
            ),
        ));
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.chars().nth(self.current) {
            if !c.is_whitespace() {
                break;
            }
            self.current += 1;
        }
    }
}
