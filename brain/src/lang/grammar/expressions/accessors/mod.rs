use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{context::Context, value::Value, Evaluate, Match, Parse};

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
        stream: &mut TokenStream,
        initial: Option<Expression>,
    ) -> Result<Expression, Box<dyn std::error::Error>> {
        let mut expression;

        if let Some(identifier) = initial {
            expression = identifier;

            while let Some(next) = stream.peek() {
                let token = &next.token;

                if !Self::matches(token) || stream.check(TokenKind::LeftParen) {
                    break;
                }

                if stream.check(TokenKind::LeftBracket) {
                    let index = Index::parse(stream, expression)?;
                    expression = Expression::Accessor(Accessor::Index(index));
                    continue;
                }

                if stream.check(TokenKind::LeftParen) {}

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
    fn matches(token: &TokenKind) -> bool {
        matches!(token, TokenKind::Dot | TokenKind::LeftBracket)
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::expressions::{map::Map, Expression};

    use super::*;

    #[test]
    fn new_index() {
        let index = Expression::new_literal(Value::Number(0));
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
            Expression::new_literal(Value::Number(0)),
            Expression::new_literal(Value::Collection(vec![Value::Number(1), Value::Number(2)])),
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
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(1)),
            )])),
        ));

        let result = accessor.evaluate(context);

        assert!(result.is_ok());
    }

    // #[test]
    // fn parse_accessor_is_collection() {
    //     let tokens = vec![
    //         Token::new(0, 0, TokenKind::LeftBracket),
    //         Token::new(0, 0, TokenKind::Number(0)),
    //         Token::new(0, 0, TokenKind::RightBracket),
    //     ];

    //     let stream = &mut TokenStream::from_vec(tokens);

    //     let result = Accessor::parse(stream);

    //     assert!(result.is_ok());
    //     assert_eq!(
    //         result.unwrap(),
    //         Expression::new_collection(vec![Expression::new_literal(Value::Number(0))])
    //     );
    // }

    // #[test]
    // fn parse_accessor_is_index_accessor() {
    //     let expression =
    //         Expression::new_collection(vec![Expression::new_literal(Value::Number(0))]);

    //     let tokens = vec![
    //         Token::new(0, 0, TokenKind::LeftBracket),
    //         Token::new(0, 0, TokenKind::Number(0)),
    //         Token::new(0, 0, TokenKind::RightBracket),
    //     ];

    //     let stream = &mut TokenStream::from_vec(tokens);

    //     let result = Accessor::parse(stream);

    //     assert!(result.is_ok());
    //     assert_eq!(
    //         result.unwrap(),
    //         Expression::Accessor(Accessor::Index(Index::new(
    //             Expression::new_literal(Value::Number(0)),
    //             expression
    //         )))
    //     );
    // }

    // #[test]
    // fn parse_accessor_is_index_accessor_with_function_call() {
    //     let expression =
    //         Expression::new_collection(vec![Expression::new_literal(Value::Number(0))]);

    //     let tokens = vec![
    //         Token::new(0, 0, TokenKind::LeftBracket),
    //         Token::new(0, 0, TokenKind::Identifier("a".to_string())),
    //         Token::new(0, 0, TokenKind::LeftParen),
    //         Token::new(0, 0, TokenKind::RightParen),
    //         Token::new(0, 0, TokenKind::RightBracket),
    //     ];

    //     let stream = &mut TokenStream::from_vec(tokens);

    //     let result = Accessor::parse(stream, Some(expression.clone()));

    //     assert!(result.is_ok());
    //     assert_eq!(
    //         result.unwrap(),
    //         Expression::Accessor(Accessor::Index(Index::new(
    //             Expression::new_function_call(Expression::new_identifier("a".to_string()), vec![]),
    //             expression
    //         )))
    //     );
    // }

    // #[test]
    // fn parse_accessor_is_field_accessor() {
    //     let expression = Expression::new_map(vec![]);

    //     let tokens = vec![
    //         Token::new(0, 0, TokenKind::Dot),
    //         Token::new(0, 0, TokenKind::Identifier("a".to_string())),
    //     ];

    //     let stream = &mut TokenStream::from_vec(tokens);

    //     let result = Accessor::parse(stream, Some(expression.clone()));

    //     assert!(result.is_ok());
    //     assert_eq!(
    //         result.unwrap(),
    //         Expression::Accessor(Accessor::Field(Field::new("a".to_string(), expression)))
    //     );
    // }

    // #[test]
    // fn parse_accessor_is_not_accessor() {
    //     let expression = Expression::new_map(vec![]);

    //     let tokens = vec![Token::new(0, 0, TokenKind::Semicolon)];

    //     let stream = &mut TokenStream::from_vec(tokens);

    //     let result = Accessor::parse(stream, Some(expression.clone()));

    //     assert!(result.is_ok());
    //     assert_eq!(result.unwrap(), expression);
    // }
}
