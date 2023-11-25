use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context,
    expressions::Expression,
    token::BrainToken,
    value::{complex::ComplexValue, literal::LiteralValue, Value},
    Evaluate, Parse,
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
        stream: &mut TokenStream<BrainToken>,
        target: Expression,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::LeftBracket)?;

        let index = Expression::parse(stream)?;

        stream.expect(BrainToken::RightBracket)?;

        Ok(Index::new(index, target))
    }
}

impl Evaluate for Index {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let target = self.target.evaluate(context)?;

        match &target {
            Value::Complex(ComplexValue::Collection(collection)) => {
                let index = self.index.evaluate(context)?;

                match index {
                    Value::Literal(LiteralValue::Number(index)) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    collection.value.len()
                                ),
                            ));
                        }

                        if index >= (collection.value.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    collection.value.len()
                                ),
                            ));
                        }

                        Ok(collection.value[index as usize].clone())
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be a number, not '{index}'"),
                    )),
                }
            }
            Value::Complex(ComplexValue::Tuple(tuple)) => {
                let index = self.index.evaluate(context)?;
                let values = tuple.values.clone();

                match index {
                    Value::Literal(LiteralValue::Number(index)) => {
                        if index < 0 {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    values.len()
                                ),
                            ));
                        }

                        if index >= (values.len() as i64) {
                            return Err(Error::new(
                                ErrorKind::IndexOutOfBounds,
                                format!(
                                    "Index {} is out of bounds (length of {})",
                                    index,
                                    values.len()
                                ),
                            ));
                        }

                        Ok(values[index as usize].clone())
                    }
                    _ => Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("Index must be a number, not '{index}'"),
                    )),
                }
            }
            Value::Complex(ComplexValue::Map(map)) => {
                let key = self.index.evaluate(context)?;

                match map.value.get(&key) {
                    Some(value) => Ok(value.clone()),
                    None => Err(Error::new(
                        ErrorKind::KeyNotFound,
                        format!("'{key}' in {target}"),
                    )),
                }
            }
            Value::Literal(LiteralValue::String(string)) => {
                let index = self.index.evaluate(context)?;

                match index {
                    Value::Literal(LiteralValue::Number(index)) => {
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

                        Ok(Value::new_string(
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

    use crate::grammar::expressions::map::Map;

    use super::*;

    #[test]
    fn new_index_accessor() {
        let index = Index::new(
            Expression::new_literal(Value::new_number(0)),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])),
        );

        assert_eq!(
            index.index,
            Box::new(Expression::new_literal(Value::new_number(0)))
        );

        assert_eq!(
            index.target,
            Box::new(Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])))
        );
    }

    #[test]
    fn index_accessor_number_of_collection() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_number(0)),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn index_accessor_number_of_collection_out_of_bounds() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])),
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
            Expression::new_literal(Value::new_number(-1)),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])),
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
            Expression::new_literal(Value::new_string("a".to_string())),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_string("a".to_string()),
                Value::new_string("b".to_string()),
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
            Expression::new_literal(Value::new_number(0)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::new_number(0)),
                    Expression::new_literal(Value::new_number(1)),
                ),
                (
                    Expression::new_literal(Value::new_number(1)),
                    Expression::new_literal(Value::new_number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn index_accessor_string_of_map() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_string("a".to_string())),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::new_string("a".to_string())),
                    Expression::new_literal(Value::new_number(1)),
                ),
                (
                    Expression::new_literal(Value::new_string("b".to_string())),
                    Expression::new_literal(Value::new_number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn index_accessor_bool_of_map() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_boolean(true)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::new_boolean(true)),
                    Expression::new_literal(Value::new_number(1)),
                ),
                (
                    Expression::new_literal(Value::new_boolean(false)),
                    Expression::new_literal(Value::new_number(2)),
                ),
            ])),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_number(1));
    }

    #[test]
    fn index_accessor_map_key_not_found() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_number(2)),
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::new_number(0)),
                    Expression::new_literal(Value::new_number(1)),
                ),
                (
                    Expression::new_literal(Value::new_number(1)),
                    Expression::new_literal(Value::new_number(2)),
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
            Expression::new_literal(Value::new_number(0)),
            Expression::new_literal(Value::new_string("abc".to_string())),
        );

        let result = index.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::new_string("a".to_string()));
    }

    #[test]
    fn index_accessor_number_of_string_out_of_bounds() {
        let context = &mut Context::new();
        let index = Index::new(
            Expression::new_literal(Value::new_number(4)),
            Expression::new_literal(Value::new_string("abc".to_string())),
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
            Expression::new_literal(Value::new_number(-1)),
            Expression::new_literal(Value::new_string("abc".to_string())),
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
            Expression::new_literal(Value::new_string("a".to_string())),
            Expression::new_literal(Value::new_string("abc".to_string())),
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
            Expression::new_literal(Value::new_number(0)),
            Expression::new_literal(Value::new_null()),
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
            Token::new(0..1, BrainToken::LeftBracket, "[".to_string()),
            Token::new(1..2, BrainToken::Number, "0".to_string()),
            Token::new(2..3, BrainToken::RightBracket, "]".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Index::parse(stream, expression.clone());

        // assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Index::new(Expression::new_literal(Value::new_number(0)), expression)
        );
    }
}
