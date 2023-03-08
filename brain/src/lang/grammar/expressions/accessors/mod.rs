use crate::lang::{
    grammar::{context::Context, value::Value, Evaluate, Match, Parse},
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

use self::{field::Field, index::Index};

use super::{collection::Collection, functioncall::FunctionCall, Expression};

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
        let mut expression: Expression;

        if let Some(identifier) = initial {
            expression = identifier;

            while let Some(next) = stream.next() {
                let token = &next.token;

                if !Self::matches(token) {
                    break;
                }

                if stream.check(TokenKind::LeftParen) {
                    let function_call = FunctionCall::parse(stream, expression)?;
                    expression = Expression::FunctionCall(function_call);
                    continue;
                }

                if stream.check(TokenKind::LeftBracket) {
                    let index = Index::parse(stream, expression)?;
                    expression = Expression::Accessor(Accessor::Index(index));
                    continue;
                }

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
        matches!(
            token,
            TokenKind::LeftParen | TokenKind::Dot | TokenKind::LeftBracket
        )
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
}
