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
