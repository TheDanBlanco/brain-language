use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context,
    token::BrainToken,
    value::{literal::LiteralValue, Value},
    Evaluate, Match, Parse,
};

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
        LiteralValue::matches(token)
    }
}

impl Evaluate for Literal {
    fn evaluate(&self, _context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        Ok(self.value.clone())
    }
}

impl Parse for Literal {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let value = Value::parse(stream)?;

        Ok(Self::new(value))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use brain_token::token::Token;

    use super::*;
    use crate::grammar::{statements::Statement, value::Value};

    #[test]
    fn create_new_number_literal() {
        let literal = Literal::new(Value::new_number(1));
        assert_eq!(literal.value, Value::new_number(1));
    }

    #[test]
    fn eval_number_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_number(1));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn create_new_string_literal() {
        let literal = Literal::new(Value::new_string("hello".to_string()));
        assert_eq!(literal.value, Value::new_string("hello".to_string()));
    }

    #[test]
    fn eval_string_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_string("hello".to_string()));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_string("hello".to_string()));
    }

    #[test]
    fn create_new_boolean_literal() {
        let literal = Literal::new(Value::new_boolean(true));
        assert_eq!(literal.value, Value::new_boolean(true));
    }

    #[test]
    fn eval_boolean_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_boolean(true));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_boolean(true));
    }

    #[test]
    fn create_new_map_literal() {
        let literal = Literal::new(Value::new_map(BTreeMap::new()));
        assert_eq!(
            literal.value,
            Value::new_map(std::collections::BTreeMap::new())
        );
    }

    #[test]
    fn eval_map_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_map(BTreeMap::new()));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_map(BTreeMap::new()));
    }

    #[test]
    fn create_new_collection_literal() {
        let literal = Literal::new(Value::new_collection(vec![]));
        assert_eq!(literal.value, Value::new_collection(vec![]));
    }

    #[test]
    fn eval_collection_literal() {
        let context = &mut Context::new();
        let literal = Literal::new(Value::new_collection(vec![]));
        let result = literal.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_collection(vec![]));
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
        let tokens = vec![Token::new(0..3, BrainToken::String, r#""hey""#.to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Literal::new(Value::new_string("hey".to_string()))
        );
    }

    #[test]
    fn parse_literal_number() {
        let tokens = vec![Token::new(0..3, BrainToken::Number, "0".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::new_number(0)));
    }

    #[test]
    fn parse_literal_null() {
        let tokens = vec![Token::new(0..3, BrainToken::Null, "null".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::new_null()));
    }

    #[test]
    fn parse_literal_bool_true() {
        let tokens = vec![Token::new(0..3, BrainToken::True, "true".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::new_boolean(true)));
    }

    #[test]
    fn parse_literal_bool_false() {
        let tokens = vec![Token::new(0..4, BrainToken::False, "false".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Literal::new(Value::new_boolean(false)));
    }

    #[test]
    fn parse_literal_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected token, found End of File".to_string()
        );
    }

    #[test]
    fn parse_literal_not_primitive() {
        let tokens = vec![Token::new(0..1, BrainToken::Plus, "+".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Literal::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected value, found Token::Plus (0 - 1)".to_string()
        );
    }
}
