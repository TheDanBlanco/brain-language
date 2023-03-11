use crate::lang::grammar::error::{Error, ErrorKind};

use super::{token::Token, tokenkind::TokenKind};

#[derive(Debug, Clone, PartialEq)]
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

    pub fn from_vec(stream: Vec<Token>) -> Self {
        TokenStream {
            stream,
            position: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token> {
        let next = self.stream.get(self.position).clone();
        self.position = self.position + 1;
        next
    }

    pub fn skip(&mut self) {
        self.next();
    }

    pub fn peek(&self) -> Option<&Token> {
        self.stream.get(self.position)
    }

    pub fn double_peek(&self) -> (Option<&Token>, Option<&Token>) {
        (
            self.stream.get(self.position),
            self.stream.get(self.position + 1),
        )
    }

    pub fn push(&mut self, token: Token) {
        self.stream.push(token)
    }

    pub fn expect(&mut self, token: TokenKind) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(next) = self.next() {
            if token == next.token {
                return Ok(());
            }

            return Err(Error::new(
                ErrorKind::UnexpectedToken,
                format!("Expected {token}, found {}", next.token),
            ));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedEndOfFile,
            format!("Expected {token}, but found End of File"),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new() {
        let stream = TokenStream::new();

        assert_eq!(
            stream,
            TokenStream {
                stream: vec![],
                position: 0,
            }
        )
    }

    #[test]
    fn push() {
        let stream = &mut TokenStream::new();

        stream.push(Token::new(0, 0, TokenKind::Let));

        assert_eq!(
            stream,
            &mut TokenStream {
                stream: vec![Token::new(0, 0, TokenKind::Let)],
                position: 0,
            }
        )
    }

    #[test]
    fn peek() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0, 0, TokenKind::Let));
    }

    #[test]
    fn multiple_peek() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let mut peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0, 0, TokenKind::Let));

        peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0, 0, TokenKind::Let));
    }

    #[test]
    fn next() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let next = stream.next();

        assert_eq!(next.unwrap(), &Token::new(0, 0, TokenKind::Let));
    }

    #[test]
    fn next_consume_stream() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert_eq!(stream.next().unwrap(), &Token::new(0, 0, TokenKind::Let));
        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0, 0, TokenKind::Identifier("x".to_string()))
        );
        assert_eq!(stream.next().unwrap(), &Token::new(0, 0, TokenKind::Assign));
        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0, 0, TokenKind::Number(0))
        );
        assert!(stream.next().is_none());
    }

    #[test]
    fn skip() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        stream.skip();

        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0, 0, TokenKind::Identifier("x".to_string()))
        );
    }

    #[test]
    fn skip_consume_stream() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        stream.skip();
        stream.skip();
        stream.skip();
        stream.skip();

        assert!(stream.next().is_none());
    }

    #[test]
    fn expect() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.expect(TokenKind::Let).is_ok())
    }

    #[test]
    fn expect_consume_stream() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.expect(TokenKind::Let).is_ok());
        assert!(stream
            .expect(TokenKind::Identifier("x".to_string()))
            .is_ok());
        assert!(stream.expect(TokenKind::Assign).is_ok());
        assert!(stream.expect(TokenKind::Number(0)).is_ok());
    }

    #[test]
    fn expect_wrong_token() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = stream.expect(TokenKind::LeftParen);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected Token::LeftParen, found Token::Let"
        );
    }

    #[test]
    fn expect_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = stream.expect(TokenKind::LeftParen);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected Token::LeftParen, but found End of File"
        );
    }

    #[test]
    fn check() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.check(TokenKind::Let));
        assert!(!stream.check(TokenKind::LeftParen));
    }

    #[test]
    fn skip_if() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        stream.skip_if(TokenKind::Let);

        assert_ne!(stream.peek().unwrap(), &Token::new(0, 0, TokenKind::Let));
    }

    #[test]
    fn double_peek() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Let),
            Token::new(0, 0, TokenKind::Identifier("x".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.double_peek();

        assert_eq!(
            peek,
            (
                Some(&Token::new(0, 0, TokenKind::Let)),
                Some(&Token::new(0, 0, TokenKind::Identifier("x".to_string())))
            )
        )
    }

    #[test]
    fn double_peek_none() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.double_peek();

        assert_eq!(peek, (None, None))
    }
}
