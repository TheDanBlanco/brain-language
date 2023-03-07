use crate::lang::grammar::error::{Error, ErrorKind};

use super::{token::Token, tokenkind::TokenKind};

#[derive(Debug, Clone)]
pub struct TokenStream {
    stream: Vec<Token>,
    position: usize,
}

impl TokenStream {
    pub fn new() -> Self {
        TokenStream {
            stream: vec![],
            position: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        self.position = self.position + 1;
        self.stream.get(self.position)
    }

    pub fn peek(&self) -> Option<&Token> {
        self.stream.get(self.position + 1)
    }

    pub fn push(&mut self, token: Token) {
        self.stream.push(token)
    }

    pub fn expect(&mut self, token: TokenKind) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(next) = self.peek() {
            if token == next.token {
                return Ok(());
            }

            return Err(Error::new(
                ErrorKind::UnexpectedToken,
                format!("Expected {token}, found {}", next.token),
            ));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!("Expected {token}, but no more tokens remain"),
        ));
    }

    pub fn check(&self, token: TokenKind) -> bool {
        if let Some(next) = self.peek() {
            return token == next.token;
        }

        false
    }

    pub fn skip_if(&mut self, token: TokenKind) {
        if self.check(token) {
            self.position = self.position + 1;
        }
    }
}
