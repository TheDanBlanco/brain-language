use brain_error::{Error, ErrorKind};

use super::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub struct TokenStream<T> {
    stream: Vec<Token<T>>,
    position: usize,
}

impl<T> TokenStream<T>
where
    T: core::fmt::Debug + PartialEq + Clone,
{
    pub fn new() -> Self {
        TokenStream {
            stream: vec![],
            position: 0,
        }
    }

    pub fn from_vec(stream: Vec<Token<T>>) -> Self {
        TokenStream {
            stream,
            position: 0,
        }
    }

    pub fn next(&mut self) -> Option<&Token<T>> {
        let next = self.stream.get(self.position).clone();
        self.position = self.position + 1;
        next
    }

    pub fn skip(&mut self) {
        self.next();
    }

    pub fn peek(&self) -> Option<&Token<T>> {
        self.stream.get(self.position)
    }

    pub fn double_peek(&self) -> (Option<&Token<T>>, Option<&Token<T>>) {
        (
            self.stream.get(self.position),
            self.stream.get(self.position + 1),
        )
    }

    pub fn push(&mut self, token: Token<T>) {
        self.stream.push(token)
    }

    pub fn expect(&mut self, token: T) -> Result<(), Box<dyn std::error::Error>> {
        if let Some(next) = self.next() {
            if token == next.token {
                return Ok(());
            }

            return Err(Error::new(
                ErrorKind::UnexpectedToken,
                format!(
                    "Expected {token:?}, found {:?} ({} - {})",
                    next.token, next.span.start, next.span.end
                ),
            ));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedEndOfFile,
            format!("Expected {token:?}, but found End of File"),
        ));
    }

    pub fn check(&self, token: T) -> bool {
        if let Some(next) = self.peek() {
            return token == next.token;
        }

        false
    }

    pub fn skip_if(&mut self, token: T) {
        if self.check(token) {
            self.position = self.position + 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum BrainToken {
        Let,
        Identifier,
        Assign,
        Number,
        LeftParen,
    }

    #[test]
    fn new() {
        let stream = &mut TokenStream::<BrainToken>::new();

        assert_eq!(
            stream,
            &mut TokenStream {
                stream: vec![],
                position: 0,
            }
        )
    }

    #[test]
    fn push() {
        let stream = &mut TokenStream::<BrainToken>::new();

        stream.push(Token::new(0..2, BrainToken::Let, None));

        assert_eq!(
            stream,
            &mut TokenStream {
                stream: vec![Token::new(0..2, BrainToken::Let, None)],
                position: 0,
            }
        )
    }

    #[test]
    fn peek() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0..2, BrainToken::Let, None));
    }

    #[test]
    fn multiple_peek() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let mut peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0..2, BrainToken::Let, None));

        peek = stream.peek();

        assert_eq!(peek.unwrap(), &Token::new(0..2, BrainToken::Let, None));
    }

    #[test]
    fn next() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let next = stream.next();

        assert_eq!(next.unwrap(), &Token::new(0..2, BrainToken::Let, None));
    }

    #[test]
    fn next_consume_stream() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0..2, BrainToken::Let, None)
        );
        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0..2, BrainToken::Identifier, Some("x".to_string()))
        );
        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0..2, BrainToken::Assign, None)
        );
        assert_eq!(
            stream.next().unwrap(),
            &Token::new(0..2, BrainToken::Number, Some("0".to_string()))
        );
        assert!(stream.next().is_none());
    }

    #[test]
    fn skip() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        stream.skip();

        assert_eq!(
            stream.peek().unwrap(),
            &Token::new(0..2, BrainToken::Identifier, Some("x".to_string()))
        );
    }

    #[test]
    fn skip_consume_stream() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
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
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.expect(BrainToken::Let).is_ok())
    }

    #[test]
    fn expect_consume_stream() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.expect(BrainToken::Let).is_ok());
        assert!(stream.expect(BrainToken::Identifier).is_ok());
        assert!(stream.expect(BrainToken::Assign).is_ok());
        assert!(stream.expect(BrainToken::Number).is_ok());
    }

    #[test]
    fn expect_wrong_token() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = stream.expect(BrainToken::LeftParen);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected LeftParen, found Let (0 - 2)"
        );
    }

    #[test]
    fn expect_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = stream.expect(BrainToken::LeftParen);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected LeftParen, but found End of File"
        );
    }

    #[test]
    fn check() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(stream.check(BrainToken::Let));
        assert!(!stream.check(BrainToken::LeftParen));
    }

    #[test]
    fn skip_if() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        stream.skip_if(BrainToken::Let);

        assert_ne!(
            stream.peek().unwrap(),
            &Token::new(0..2, BrainToken::Let, None)
        );
    }

    #[test]
    fn double_peek() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Let, None),
            Token::new(0..2, BrainToken::Identifier, Some("x".to_string())),
            Token::new(0..2, BrainToken::Assign, None),
            Token::new(0..2, BrainToken::Number, Some("0".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.double_peek();

        assert_eq!(
            peek,
            (
                Some(&Token::new(0..2, BrainToken::Let, None)),
                Some(&Token::new(
                    0..2,
                    BrainToken::Identifier,
                    Some("x".to_string())
                ))
            )
        )
    }

    #[test]
    fn double_peek_none() {
        let tokens: Vec<Token<BrainToken>> = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let peek = stream.double_peek();

        assert_eq!(peek, (None, None))
    }
}
