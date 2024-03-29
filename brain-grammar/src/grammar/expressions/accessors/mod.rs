use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Match, Parse};

use self::{field::Field, index::Index};

use super::{collection::Collection, Expression};

pub mod field;
pub mod index;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Accessor {
    Index(Index),
    Field(Field),
}

impl Accessor {
    pub fn new_index(index: Expression, target: Expression) -> Self {
        Accessor::Index(Index::new(index, target))
    }

    pub fn new_field(field: String, target: Expression) -> Self {
        Accessor::Field(Field::new(field, target))
    }

    pub fn parse(
        stream: &mut TokenStream<BrainToken>,
        initial: Option<Expression>,
    ) -> Result<Expression, Box<dyn std::error::Error>> {
        let mut expression;

        if let Some(identifier) = initial {
            expression = identifier;

            while let Some(next) = stream.peek() {
                let token = &next.token;

                if !Self::matches(token) || stream.check(BrainToken::LeftParen) {
                    break;
                }

                if stream.check(BrainToken::LeftBracket) {
                    let index = Index::parse(stream, expression)?;
                    expression = Expression::Accessor(Accessor::Index(index));
                    continue;
                }

                if stream.check(BrainToken::LeftParen) {}

                let field = Field::parse(stream, expression)?;
                expression = Expression::Accessor(Accessor::Field(field))
            }

            return Ok(expression);
        }

        let collection = Collection::parse(stream)?;
        Ok(Expression::Collection(collection))
    }
}

impl Evaluate for Accessor {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Accessor::Index(index) => index.evaluate(context),
            Accessor::Field(field) => field.evaluate(context),
        }
    }
}

impl Match for Accessor {
    fn matches(token: &BrainToken) -> bool {
        matches!(token, BrainToken::Dot | BrainToken::LeftBracket)
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::grammar::expressions::{map::Map, Expression};

    use super::*;

    #[test]
    fn new_index() {
        let index = Expression::new_literal(Value::new_number(0));
        let target = Expression::new_collection(vec![]);

        let accessor = Accessor::new_index(index.clone(), target.clone());

        assert_eq!(accessor, Accessor::Index(Index::new(index, target)));
    }

    #[test]
    fn new_field() {
        let field = "a".to_string();
        let target = Expression::new_collection(vec![]);

        let accessor = Accessor::new_field(field.clone(), target.clone());

        assert_eq!(accessor, Accessor::Field(Field::new(field, target)));
    }

    #[test]
    fn eval_index_accessor() {
        let context = &mut Context::new();
        let accessor = Accessor::Index(Index::new(
            Expression::new_literal(Value::new_number(0)),
            Expression::new_literal(Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
            ])),
        ));

        let result = accessor.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_field_accessor() {
        let context = &mut Context::new();
        let accessor = Accessor::Field(Field::new(
            "a".to_string(),
            Expression::Map(Map::new(vec![(
                Expression::new_literal(Value::new_string("a".to_string())),
                Expression::new_literal(Value::new_number(1)),
            )])),
        ));

        let result = accessor.evaluate(context);

        assert!(result.is_ok());
    }

    #[test]
    fn parse_accessor_is_collection() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBracket, "[".to_string()),
            Token::new(1..2, BrainToken::Number, "0".to_string()),
            Token::new(2..3, BrainToken::RightBracket, "]".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Accessor::parse(stream, None);

        // assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::new_collection(vec![Expression::new_literal(Value::new_number(0))])
        );
    }

    #[test]
    fn parse_accessor_is_index_accessor() {
        let expression =
            Expression::new_collection(vec![Expression::new_literal(Value::new_number(0))]);

        let tokens = vec![
            Token::new(7..8, BrainToken::LeftBracket, "[".to_string()),
            Token::new(8..9, BrainToken::Number, "0".to_string()),
            Token::new(9..10, BrainToken::RightBracket, "]".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Accessor::parse(stream, Some(expression.clone()));

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::Accessor(Accessor::Index(Index::new(
                Expression::new_literal(Value::new_number(0)),
                expression
            )))
        );
    }

    #[test]
    fn parse_accessor_is_index_accessor_with_function_call() {
        let expression =
            Expression::new_collection(vec![Expression::new_literal(Value::new_number(0))]);

        let tokens = vec![
            Token::new(7..8, BrainToken::LeftBracket, "[".to_string()),
            Token::new(8..11, BrainToken::Identifier, "func".to_string()),
            Token::new(11..12, BrainToken::LeftParen, "(".to_string()),
            Token::new(12..13, BrainToken::RightParen, ")".to_string()),
            Token::new(9..10, BrainToken::RightBracket, "]".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Accessor::parse(stream, Some(expression.clone()));

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::Accessor(Accessor::Index(Index::new(
                Expression::new_function_call(
                    Expression::new_identifier("func".to_string()),
                    vec![]
                ),
                expression
            )))
        );
    }

    #[test]
    fn parse_accessor_is_field_accessor() {
        let expression = Expression::new_map(vec![]);

        let tokens = vec![
            Token::new(7..8, BrainToken::Dot, ".".to_string()),
            Token::new(8..9, BrainToken::Identifier, "a".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Accessor::parse(stream, Some(expression.clone()));

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::Accessor(Accessor::Field(Field::new("a".to_string(), expression)))
        );
    }

    #[test]
    fn parse_accessor_is_not_accessor() {
        let expression = Expression::new_map(vec![]);

        let tokens = vec![Token::new(0..1, BrainToken::Semicolon, ";".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Accessor::parse(stream, Some(expression.clone()));

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expression);
    }
}
