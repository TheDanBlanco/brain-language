use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{context::Context, value::Value, Evaluate, Match, Parse};

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
    fn matches(token: &TokenKind) -> bool {
        matches!(
            token,
            TokenKind::Number(_)
                | TokenKind::String(_)
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Null,
        )
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        Ok(self.value.clone())
    }
}

impl Parse for Literal {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected literal, found End of File".to_string(),
            ));
        }

        let token = &next.unwrap().token;

        let literal = match token {
            TokenKind::Number(number) => Self::new(Value::Number(*number)),
            TokenKind::String(string) => Self::new(Value::String(string.to_string())),
            TokenKind::True => Self::new(Value::Boolean(true)),
            TokenKind::False => Self::new(Value::Boolean(false)),
            TokenKind::Null => Self::new(Value::Null),
            _ => {
                return Err(Error::new(
                    ErrorKind::UnexpectedToken,
                    format!("Expected literal, found {token}"),
                ))
            }
        };

        Ok(literal)
    }
}

// tests
#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;
    use crate::lang::grammar::{statements::Statement, value::Value};

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
        let tokens = vec![Token::new(0, 0, TokenKind::String("a".to_string()))];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Literal::new(Value::String("a".to_string()))
        );
    }

    #[test]
    fn parse_literal_number() {
        let tokens = vec![Token::new(0, 0, TokenKind::Number(0))];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Number(0)));
    }

    #[test]
    fn parse_literal_null() {
        let tokens = vec![Token::new(0, 0, TokenKind::Null)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Null));
    }

    #[test]
    fn parse_literal_bool_true() {
        let tokens = vec![Token::new(0, 0, TokenKind::True)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::Boolean(true)));
    }

    #[test]
    fn parse_literal_bool_false() {
        let tokens = vec![Token::new(0, 0, TokenKind::False)];

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
        let tokens = vec![Token::new(0, 0, TokenKind::Add)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected literal, found Token::Add".to_string()
        );
    }
}
