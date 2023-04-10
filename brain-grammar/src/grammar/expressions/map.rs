use std::collections::BTreeMap;

use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

use super::Expression;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Map {
    pub pairs: Vec<(Expression, Expression)>,
}

impl Map {
    pub fn new(pairs: Vec<(Expression, Expression)>) -> Self {
        Map { pairs }
    }
}

impl Evaluate for Map {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut map = BTreeMap::new();

        for (key, value) in &self.pairs {
            let key = key.evaluate(context)?;

            match key {
                Value::Number(_) | Value::String(_) | Value::Boolean(_) | Value::Null => {}
                _ => return Err(Error::new(ErrorKind::InvalidMapKey, format!("{key}"))),
            }

            let value = value.evaluate(context)?;
            map.insert(key.clone(), value.clone());
        }

        Ok(Value::Map(map))
    }
}

impl Parse for Map {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::LeftBrace)?;

        let mut entries = vec![];

        while !stream.check(BrainToken::RightBrace) {
            let mut key = Expression::parse(stream)?;

            key = match key {
                Expression::Identifier(identifier) => {
                    Expression::new_literal(Value::String(identifier.name))
                }
                Expression::Literal(_) => key,
                _ => {
                    return Err(Error::new(
                        ErrorKind::InvalidMapKey,
                        format!("cannot use {key} for key in map"),
                    ))
                }
            };

            stream.expect(BrainToken::Colon)?;
            let value = Expression::parse(stream)?;
            stream.skip_if(BrainToken::Comma);
            entries.push((key, value));
        }

        stream.expect(BrainToken::RightBrace)?;
        stream.skip_if(BrainToken::Semicolon);
        Ok(Self::new(entries))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;
    use crate::grammar::{statements::Statement, value::Value};

    #[test]
    fn create_new_map() {
        let pairs = vec![(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        assert_eq!(map.pairs.len(), 1);
        assert_eq!(
            map.pairs[0].0,
            Expression::new_literal(Value::String("a".to_string()))
        );
    }

    #[test]
    fn eval_map_string_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::String("a".to_string()), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::String("a".to_string())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_number_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::Number(1), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_boolean_key() {
        let mut tree = BTreeMap::new();
        tree.insert(Value::Boolean(true), Value::Number(2));

        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Boolean(true)),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Map(tree));
    }

    #[test]
    fn eval_map_with_invalid_key_collection() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Collection(vec![])),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "[InvalidMapKey]: []",);
    }

    #[test]
    fn eval_map_with_invalid_key_function() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::new_function(vec![], Statement::new_break())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMapKey]: [function]",
        );
    }

    #[test]
    fn eval_map_with_invalid_key_map() {
        let context = &mut Context::new();
        let pairs = vec![(
            Expression::new_literal(Value::Map(BTreeMap::new())),
            Expression::new_literal(Value::Number(2)),
        )];
        let map = Map::new(pairs);

        let result = map.evaluate(context);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().to_string(), "[InvalidMapKey]: {}",);
    }

    #[test]
    fn parse_map() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(0..1, BrainToken::Colon, None),
            Token::new(0..1, BrainToken::Number, Some("0".to_string())),
            Token::new(0..1, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Map::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(0))
            )])
        );
    }

    #[test]
    fn parse_map_invalid_key_collection() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(0..1, BrainToken::LeftBracket, None),
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(0..1, BrainToken::RightBracket, None),
            Token::new(0..1, BrainToken::Colon, None),
            Token::new(0..1, BrainToken::Number, Some("0".to_string())),
            Token::new(0..1, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Map::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMapKey]: cannot use collection for key in map",
        );
    }

    #[test]
    fn parse_map_invalid_key_map() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(0..1, BrainToken::RightBrace, None),
            Token::new(0..1, BrainToken::Colon, None),
            Token::new(0..1, BrainToken::Number, Some("0".to_string())),
            Token::new(0..1, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Map::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMapKey]: cannot use map for key in map",
        );
    }

    #[test]
    fn parse_map_invalid_key_function() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(0..1, BrainToken::Identifier, Some("test".to_string())),
            Token::new(0..1, BrainToken::LeftParen, None),
            Token::new(0..1, BrainToken::RightParen, None),
            Token::new(0..1, BrainToken::RightBrace, None),
            Token::new(0..1, BrainToken::Colon, None),
            Token::new(0..1, BrainToken::Number, Some("0".to_string())),
            Token::new(0..1, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Map::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidMapKey]: cannot use function for key in map",
        );
    }
}
