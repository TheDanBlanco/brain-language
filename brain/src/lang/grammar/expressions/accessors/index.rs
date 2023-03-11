use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{
    context::Context, expressions::Expression, value::Value, Evaluate, Parse,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Index {
    index: Box<Expression>,
    target: Box<Expression>,
}

impl Index {
    pub fn new(index: Expression, target: Expression) -> Self {
        Index {
            index: Box::new(index),
            target: Box::new(target),
        }
    }

    pub fn parse(
        stream: &mut TokenStream,
        target: Expression,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::LeftBracket)?;

        let index = Expression::parse(stream)?;

        stream.expect(TokenKind::RightBracket)?;

        Ok(Index::new(index, target))
    }
}

impl Evaluate for Index {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let target = self.target.evaluate(context)?;

        match &target {
            Value::Collection(collection) => {
                let index = self.index.evaluate(context)?;

                match index {
                    Value::Number(index) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    collection.len()
                                ),
                            ));
                        }

                        if index >= (collection.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    collection.len()
                                ),
                            ));
                        }

                        Ok(collection[index as usize].clone())
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be a number, not '{index}'"),
                    )),
                }
            }
            Value::Map(map) => {
                let key = self.index.evaluate(context)?;

                match map.get(&key) {
                    Some(value) => Ok(value.clone()),
                    None => Err(Error::new(
                        ErrorKind::KeyNotFound,
                        format!("'{key}' in {target}"),
                    )),
                }
            }
            Value::String(string) => {
                let index = self.index.evaluate(context)?;

                match index {
                    Value::Number(index) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    string.len()
                                ),
                            ));
                        }

                        if index >= (string.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    string.len()
                                ),
                            ));
                        }

                        Ok(Value::String(
                            string.chars().nth(index as usize).unwrap().to_string(),
                        ))
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be a number, not '{index}'"),
                    )),
                }
            }
            _ => Err(Error::new(
                ErrorKind::InvalidType,
                format!("Cannot index into {target}"),
            )),
        }
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::expressions::map::Map;

    use super::*;

    #[test]
    fn new_index_accessor() {
        let index = Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
        );

        assert_eq!(
            index.index,
            Box::new(Expression::new_literal(Value::Number(0)))
        );

        assert_eq!(
            index.target,
            Box::new(Expression::new_literal(Value::Collection(vec![
                Value::Number(1),
                Value::Number(2),
            ])))
        );
    }

    #[test]
    fn index_accessor_number_of_collection() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn index_accessor_number_of_collection_out_of_bounds() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[IndexOutOfBounds]: Index 2 is out of bounds (length of 2)"
        );
    }

    #[test]
    fn index_accessor_number_of_collection_number_is_negative() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(-1)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[IndexOutOfBounds]: Index -1 is out of bounds (length of 2)"
        );
    }

    #[test]
    fn index_accessor_string_of_collection() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::Collection(vec![
                Value::String("a".to_string()),
                Value::String("b".to_string()),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Index must be a number, not 'a'"
        );
    }

    #[test]
    fn index_accessor_number_of_map() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::Number(0)),
                    Expression::new_literal(Value::Number(1)),
                ),
                (
                    Expression::new_literal(Value::Number(1)),
                    Expression::new_literal(Value::Number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn index_accessor_string_of_map() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::String("a".to_string())),
                    Expression::new_literal(Value::Number(1)),
                ),
                (
                    Expression::new_literal(Value::String("b".to_string())),
                    Expression::new_literal(Value::Number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn index_accessor_bool_of_map() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Boolean(true)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::Boolean(true)),
                    Expression::new_literal(Value::Number(1)),
                ),
                (
                    Expression::new_literal(Value::Boolean(false)),
                    Expression::new_literal(Value::Number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn index_accessor_map_key_not_found() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(2)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::Number(0)),
                    Expression::new_literal(Value::Number(1)),
                ),
                (
                    Expression::new_literal(Value::Number(1)),
                    Expression::new_literal(Value::Number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[KeyNotFound]: '2' in { 0: 1, 1: 2 }"
        );
    }

    #[test]
    fn index_accessor_number_of_string() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::String("abc".to_string())),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::String("a".to_string()));
    }

    #[test]
    fn index_accessor_number_of_string_out_of_bounds() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(4)),
            Expression::new_literal(Value::String("abc".to_string())),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[IndexOutOfBounds]: Index 4 is out of bounds (length of 3)"
        );
    }

    #[test]
    fn index_accessor_number_of_string_number_is_negative() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(-1)),
            Expression::new_literal(Value::String("abc".to_string())),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[IndexOutOfBounds]: Index -1 is out of bounds (length of 3)"
        );
    }

    #[test]
    fn index_accessor_string_of_string() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::String("abc".to_string())),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Index must be a number, not 'a'"
        );
    }

    #[test]
    fn index_accessor_invalid_type() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::Null),
        );

        let result = index.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Cannot index into null"
        );
    }

    #[test]
    fn parse_index_accessor() {
        let expression = Expression::new_identifier("a".to_string());

        let tokens = vec![
            Token::new(0, 0, TokenKind::LeftBracket),
            Token::new(0, 0, TokenKind::Number(0)),
            Token::new(0, 0, TokenKind::RightBracket),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Index::parse(stream, expression.clone());

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Index::new(Expression::new_literal(Value::Number(0)), expression)
        );
    }
}
