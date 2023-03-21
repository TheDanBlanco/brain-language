use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context, expressions::Expression, token::BrainToken, value::Value, Evaluate,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    field: Value,
    target: Box<Expression>,
}

impl Field {
    pub fn new(field: String, target: Expression) -> Self {
        Field {
            field: Value::String(field),
            target: Box::new(target),
        }
    }

    pub fn parse(
        stream: &mut TokenStream<BrainToken>,
        target: Expression,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::Dot)?;
        let next = stream.next();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected identifier, found End of File".to_string(),
            ));
        }

        let token = &next.unwrap();

        if let BrainToken::Identifier = token.token {
            return Ok(Self::new(token.data.clone().unwrap(), target));
        }

        Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!(
                "Expected identifier, found {} ({} - {})",
                token.token, token.span.start, token.span.end
            ),
        ))
    }
}

impl Evaluate for Field {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let target = self.target.evaluate(context)?;

        if let Value::String(_) = &self.field {
        } else {
            return Err(Error::new(
                ErrorKind::InvalidType,
                format!("Field accessor '{}' must be of type String", self.field),
            ));
        }

        match &target {
            Value::Map(map) => match map.get(&self.field) {
                Some(value) => Ok(value.clone()),
                None => Err(Error::new(
                    ErrorKind::KeyNotFound,
                    format!("'{}' in {target}", self.field),
                )),
            },
            _ => Err(Error::new(
                ErrorKind::InvalidType,
                format!("Cannot access field {} of {target}", self.field),
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
    fn create_new_field() {
        let field = Field::new(
            "a".to_string(),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );
        assert_eq!(field.field, Value::String("a".to_string()));
        assert_eq!(
            field.target,
            Box::new(Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )],)))
        );
    }

    #[test]
    fn field_access_map() {
        let context = &mut Context::new();
        let field = Field::new(
            "a".to_string(),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Number(1));
    }

    #[test]
    fn field_access_map_key_not_found() {
        let context = &mut Context::new();
        let field = Field::new(
            "b".to_string(),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        );

        let result = field.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[KeyNotFound]: 'b' in { a: 1 }".to_string()
        );
    }

    #[test]
    fn field_access_target_not_map() {
        let context = &mut Context::new();
        let field = Field::new("a".to_string(), Expression::new_literal(Value::Number(1)));

        let result = field.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Cannot access field a of 1".to_string()
        );
    }

    #[test]
    fn field_accessor_type_not_string() {
        let context = &mut Context::new();
        let field = Field {
            field: Value::Null,
            target: Box::new(Expression::new_literal(Value::Number(1))),
        };

        let result = field.evaluate(context);
        assert!(result.is_err());
        assert_eq!(
            result.unwrap_err().to_string(),
            "[InvalidType]: Field accessor 'null' must be of type String".to_string()
        );
    }

    #[test]
    fn parse_field_accessor() {
        let expression = Expression::new_map(vec![]);

        let tokens = vec![
            Token::new(0..1, BrainToken::Dot, None),
            Token::new(1..2, BrainToken::Identifier, Some("a".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Field::parse(stream, expression.clone());

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Field::new("a".to_string(), expression));
    }

    #[test]
    fn parse_field_accessor_eof() {
        let expression = Expression::new_map(vec![]);

        let tokens = vec![Token::new(0..1, BrainToken::Dot, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Field::parse(stream, expression.clone());

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected identifier, found End of File".to_string()
        )
    }

    #[test]
    fn parse_field_accessor_not_identifier() {
        let expression = Expression::new_map(vec![]);

        let tokens = vec![
            Token::new(0..1, BrainToken::Dot, None),
            Token::new(1..2, BrainToken::Null, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Field::parse(stream, expression.clone());

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected identifier, found Token::Null (1 - 2)".to_string()
        )
    }
}
