use brain_token::{stream::TokenStream, tokenkind::TokenKind};

pub fn disambiguate_reassignment(stream: &mut TokenStream<TokenKind>) -> bool {
    if let (Some(next), Some(following)) = stream.double_peek() {
        return match (&next.token, &following.token) {
            (TokenKind::Identifier(_), TokenKind::Assign) => true,
            _ => false,
        };
    }

    false
}

#[cfg(test)]
mod test {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn reassignment() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("a".to_string())),
            Token::new(0, 0, TokenKind::Assign),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(disambiguate_reassignment(stream));
    }

    #[test]
    fn equality() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Identifier("a".to_string())),
            Token::new(0, 0, TokenKind::Equal),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(!disambiguate_reassignment(stream));
    }
}
