use brain_token::stream::TokenStream;

use super::token::BrainToken;

pub fn disambiguate_reassignment(stream: &mut TokenStream<BrainToken>) -> bool {
    if let (Some(next), Some(following)) = stream.double_peek() {
        return match (&next.token, &following.token) {
            (BrainToken::Identifier, BrainToken::Assign) => true,
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
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(2..3, BrainToken::Assign, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(disambiguate_reassignment(stream));
    }

    #[test]
    fn equality() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(2..3, BrainToken::Equal, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        assert!(!disambiguate_reassignment(stream));
    }
}
