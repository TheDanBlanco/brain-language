use core::fmt;

use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{token::BrainToken, Match, Parse};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum LiteralValue {
    String(String),
    Number(i64),
    Boolean(bool),
    Null,
}

impl Match for LiteralValue {
    fn matches(token: &BrainToken) -> bool {
        matches!(
            token,
            BrainToken::String
                | BrainToken::Number
                | BrainToken::True
                | BrainToken::False
                | BrainToken::Null
        )
    }
}

impl Parse for LiteralValue {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let token = stream.assert_peek("Expected token, found End of File".to_string())?;

        let val = match token.token {
            BrainToken::String => LiteralValue::String(String::parse(stream)?),
            BrainToken::Number => LiteralValue::Number(i64::parse(stream)?),
            BrainToken::True | BrainToken::False => LiteralValue::Boolean(bool::parse(stream)?),
            BrainToken::Null => {
                stream.skip();
                LiteralValue::Null
            }
            _ => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!(
                        "Expected a literal value, found {} ({} - {})",
                        token.token, token.span.start, token.span.end,
                    ),
                ))
            }
        };

        return Ok(val);
    }
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return match self {
            LiteralValue::String(s) => write!(f, "{}", s),
            LiteralValue::Number(n) => write!(f, "{}", n),
            LiteralValue::Boolean(b) => write!(f, "{}", b),
            LiteralValue::Null => write!(f, "null"),
        };
    }
}

impl Parse for String {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let token = stream.expect(BrainToken::String)?;

        return Ok(token.data.replace("\"", "").replace("\'", ""));
    }
}

impl Parse for i64 {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let token = stream.expect(BrainToken::Number)?;

        return Ok(token.data.parse::<i64>()?);
    }
}

impl Parse for bool {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let token = stream.assert_next("Expected token, found End of File".to_string())?;

        if token.token == BrainToken::True {
            return Ok(true);
        }

        if token.token == BrainToken::False {
            return Ok(false);
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!(
                "Expected a boolean value, found {} ({} - {})",
                token.token, token.span.start, token.span.end,
            ),
        ));
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn matches() {
        assert!(LiteralValue::matches(&BrainToken::String));
        assert!(LiteralValue::matches(&BrainToken::Number));
        assert!(LiteralValue::matches(&BrainToken::True));
        assert!(LiteralValue::matches(&BrainToken::False));
        assert!(LiteralValue::matches(&BrainToken::Null));
        assert!(!LiteralValue::matches(&BrainToken::Identifier));
    }

    #[test]
    fn parse_string() {
        let tokens = vec![Token::new(0..2, BrainToken::String, r#""oh""#.to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::String("oh".to_string()));
    }

    #[test]
    fn parse_number() {
        let tokens = vec![Token::new(0..1, BrainToken::Number, "1".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::Number(1));
    }

    #[test]
    fn parse_boolean_true() {
        let tokens = vec![Token::new(0..4, BrainToken::True, "true".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::Boolean(true));
    }

    #[test]
    fn parse_boolean_false() {
        let tokens = vec![Token::new(0..5, BrainToken::False, "false".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::Boolean(false));
    }

    #[test]
    fn parse_null() {
        let tokens = vec![Token::new(0..4, BrainToken::Null, "null".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::Null);
    }

    #[test]
    fn parse_eof() {
        let tokens = vec![];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert!(val.is_err());
    }

    #[test]
    fn parse_unexpected_token() {
        let tokens = vec![Token::new(0..1, BrainToken::Identifier, "oh".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert!(val.is_err());
    }

    #[test]
    fn display_string() {
        let val = LiteralValue::String("oh".to_string());

        assert_eq!(val.to_string(), "oh");
    }

    #[test]
    fn display_number() {
        let val = LiteralValue::Number(1);

        assert_eq!(val.to_string(), "1");
    }

    #[test]
    fn display_boolean_true() {
        let val = LiteralValue::Boolean(true);

        assert_eq!(val.to_string(), "true");
    }

    #[test]
    fn display_boolean_false() {
        let val = LiteralValue::Boolean(false);

        assert_eq!(val.to_string(), "false");
    }

    #[test]
    fn display_null() {
        let val = LiteralValue::Null;

        assert_eq!(val.to_string(), "null");
    }

    #[test]
    fn parse_string_value() {
        let tokens = vec![Token::new(0..2, BrainToken::String, r#""oh""#.to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = String::parse(&mut stream);

        assert_eq!(val.unwrap(), "oh".to_string());
    }

    #[test]
    fn parse_number_value() {
        let tokens = vec![Token::new(0..1, BrainToken::Number, "1".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = i64::parse(&mut stream);

        assert_eq!(val.unwrap(), 1);
    }

    #[test]
    fn parse_boolean_value_true() {
        let tokens = vec![Token::new(0..4, BrainToken::True, "true".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = bool::parse(&mut stream);

        assert_eq!(val.unwrap(), true);
    }

    #[test]
    fn parse_boolean_value_false() {
        let tokens = vec![Token::new(0..5, BrainToken::False, "false".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = bool::parse(&mut stream);

        assert_eq!(val.unwrap(), false);
    }

    #[test]
    fn parse_boolean_value_unexpected_token() {
        let tokens = vec![Token::new(0..1, BrainToken::Number, "1".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = bool::parse(&mut stream);

        assert!(val.is_err());
    }

    #[test]
    fn parse_null_value() {
        let tokens = vec![Token::new(0..4, BrainToken::Null, "null".to_string())];
        let mut stream = TokenStream::from_vec(tokens);

        let val = LiteralValue::parse(&mut stream);

        assert_eq!(val.unwrap(), LiteralValue::Null);
    }
}
