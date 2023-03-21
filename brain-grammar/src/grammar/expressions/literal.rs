use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Match, Parse};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Literal {
    value: Value,
}

impl Literal {
    pub fn new(value: Value) -> Self {
        Literal { value }
    }
}

impl Match for Literal {
    fn matches(token: &BrainToken) -> bool {
        matches!(
            token,
            BrainToken::Number
                | BrainToken::String
                | BrainToken::True
                | BrainToken::False
                | BrainToken::Null,
        )
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        Ok(self.value.clone())
    }
}

impl Parse for Literal {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected literal, found End of File".to_string(),
            ));
        }

        let token = next.unwrap();

        let literal = match (&token.token, &token.data) {
            (BrainToken::Number, Some(data)) => Self::new(Value::Number(data.parse().unwrap())),
            (BrainToken::String, Some(data)) => {
                Self::new(Value::String(data.replace("\"", "").replace("\'", "")))
            }
            (BrainToken::True, _) => Self::new(Value::Boolean(true)),
            (BrainToken::False, _) => Self::new(Value::Boolean(false)),
            (BrainToken::Null, _) => Self::new(Value::Null),
            _ => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!(
                        "Expected literal, found {} ({} - {})",
                        token.token, token.span.start, token.span.end
                    ),
                ))
            }
        };

        Ok(literal)
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;
    use crate::grammar::{statements::Statement, value::Value};

    #[test]
    fn create_new_number_literal() {
        let literal = Literal::new(Value::Number(1));
        assert_eq!(literal.value, Value::Number(1));
    }

    #[test]
    fn eval_number_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Number(1));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn create_new_string_literal() {
        let literal = Literal::new(Value::String("hello".to_string()));
        assert_eq!(literal.value, Value::String("hello".to_string()));
    }

    #[test]
    fn eval_string_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::String("hello".to_string()));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::String("hello".to_string()));
    }

    #[test]
    fn create_new_boolean_literal() {
        let literal = Literal::new(Value::Boolean(true));
        assert_eq!(literal.value, Value::Boolean(true));
    }

    #[test]
    fn eval_boolean_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Boolean(true));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Boolean(true));
    }

    #[test]
    fn create_new_map_literal() {
        let literal = Literal::new(Value::Map(std::collections::BTreeMap::new()));
        assert_eq!(literal.value, Value::Map(std::collections::BTreeMap::new()));
    }

    #[test]
    fn eval_map_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Map(std::collections::BTreeMap::new()));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::Map(std::collections::BTreeMap::new())
        );
    }

    #[test]
    fn create_new_collection_literal() {
        let literal = Literal::new(Value::Collection(vec![]));
        assert_eq!(literal.value, Value::Collection(vec![]));
    }

    #[test]
    fn eval_collection_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::Collection(vec![]));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Collection(vec![]));
    }

    #[test]
    fn create_new_function_literal() {
        let literal = Literal::new(Value::new_function(vec![], Statement::new_break()));
        assert_eq!(
            literal.value,
            Value::new_function(vec![], Statement::new_break()),
        );
    }

    #[test]
    fn eval_function_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_function(vec![], Statement::new_break()));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_function(vec![], Statement::new_break()),
        );
    }

    #[test]
    fn parse_literal_string() {
        let tokens = vec![Token::new(
            0..3,
            BrainToken::String,
            Some(r#""hey""#.to_string()),
        )];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Literal::new(Value::String("hey".to_string()))
        );
    }

    #[test]
    fn parse_literal_number() {
        let tokens = vec![Token::new(0..3, BrainToken::Number, Some("0".to_string()))];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Number(0)));
    }

    #[test]
    fn parse_literal_null() {
        let tokens = vec![Token::new(0..3, BrainToken::Null, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Null));
    }

    #[test]
    fn parse_literal_bool_true() {
        let tokens = vec![Token::new(0..3, BrainToken::True, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Boolean(true)));
    }

    #[test]
    fn parse_literal_bool_false() {
        let tokens = vec![Token::new(0..4, BrainToken::False, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Boolean(false)));
    }

    #[test]
    fn parse_literal_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected literal, found End of File".to_string()
        );
    }

    #[test]
    fn parse_literal_not_primitive() {
        let tokens = vec![Token::new(0..1, BrainToken::Plus, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected literal, found Token::Plus (0 - 1)".to_string()
        );
    }
}
