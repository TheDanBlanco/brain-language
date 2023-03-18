use brain_attribute::Brain;

#[derive(Brain, Debug, PartialEq, Clone)]
enum BrainToken {
    #[token("let")]
    Let,

    #[regex("[a-zA-Z_-]+")]
    Identifier,

    #[regex("[0-9]+")]
    Number,

    #[token("=")]
    Assign,

    #[token(";")]
    Semicolon,

    #[regex(r#""[a-zA-Z0-9_-]+""#)]
    String
}

#[cfg(test)]
mod tests {
    use brain_token::{token::Token, stream::TokenStream};

    use super::*;

    #[test]
    fn assignment() {
        let input = "let first = 0".to_string();

        let expected_tokens = vec![
            Token::new(0..3, BrainToken::Let, Some("let".to_string())),
            Token::new(4..9, BrainToken::Identifier, Some("first".to_string())),
            Token::new(10..11, BrainToken::Assign, Some("=".to_string())),
            Token::new(12..13, BrainToken::Number, Some("0".to_string())),
        ];

        let expected_stream = TokenStream::from_vec(expected_tokens);

        let stream = BrainToken::lex(input).unwrap();

        assert_eq!(stream, expected_stream);
    }

    #[test]
    fn reassignment() {
        let input = "x = 0".to_string();

        let expected_tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("x".to_string())),
            Token::new(2..3, BrainToken::Assign, Some("=".to_string())),
            Token::new(4..5, BrainToken::Number, Some("0".to_string())),
        ];

        let expected_stream = TokenStream::from_vec(expected_tokens);

        let stream = BrainToken::lex(input).unwrap();

        assert_eq!(stream, expected_stream);
    }

    #[test]
    fn string() {
        let input = r#"let x = "hello""#.to_string();

        let expected_tokens = vec![
            Token::new(0..3, BrainToken::Let, Some("let".to_string())),
            Token::new(4..5, BrainToken::Identifier, Some("x".to_string())),
            Token::new(6..7, BrainToken::Assign, Some("=".to_string())),
            Token::new(8..15, BrainToken::String, Some(r#""hello""#.to_string())),
        ];

        let expected_stream = TokenStream::from_vec(expected_tokens);

        let stream = BrainToken::lex(input).unwrap();

        assert_eq!(stream, expected_stream);
    }
}
