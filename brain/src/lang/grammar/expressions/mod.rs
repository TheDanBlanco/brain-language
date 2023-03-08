use crate::lang::{
    grammar::{context::Context, value::Value},
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

use self::{
    accessors::Accessor, binary::Binary, collection::Collection, functioncall::FunctionCall,
    identifier::Identifier, literal::Literal, map::Map, operator::Operator,
};

use super::{
    error::{Error, ErrorKind},
    Evaluate, Match, Parse,
};

pub mod accessors;
pub mod binary;
pub mod collection;
pub mod functioncall;
pub mod identifier;
pub mod literal;
pub mod map;
pub mod operator;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Binary(Binary),
    Collection(Collection),
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
    Accessor(Accessor),
    Map(Map),
}

impl Expression {
    pub fn new_binary(lhs: Expression, operator: Operator, rhs: Expression) -> Self {
        Expression::Binary(Binary::new(lhs, operator, rhs))
    }

    pub fn new_collection(elements: Vec<Expression>) -> Self {
        Expression::Collection(Collection::new(elements))
    }

    pub fn new_literal(value: Value) -> Self {
        Expression::Literal(Literal::new(value))
    }

    pub fn new_identifier(identifier: String) -> Self {
        Expression::Identifier(Identifier::new(identifier))
    }

    pub fn new_function_call(name: Expression, args: Vec<Expression>) -> Self {
        Expression::FunctionCall(FunctionCall::new(name, args))
    }

    pub fn new_map(pairs: Vec<(Expression, Expression)>) -> Self {
        Expression::Map(Map::new(pairs))
    }
}

impl Evaluate for Expression {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Expression::Binary(binary) => binary.evaluate(context),
            Expression::Collection(collection) => collection.evaluate(context),
            Expression::Literal(literal) => literal.evaluate(context),
            Expression::FunctionCall(function_call) => function_call.evaluate(context),
            Expression::Identifier(identifier) => identifier.evaluate(context),
            Expression::Map(map) => map.evaluate(context),
            Expression::Accessor(accessor) => accessor.evaluate(context),
        }
    }
}

impl Parse for Expression {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.peek();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected literal, identifier, left brace, or left bracket, found End of File"
                    .to_string(),
            ));
        }

        let token = &next.unwrap().token;

        if let TokenKind::Identifier(identifier) = token {
            let identifier = Self::Identifier(Identifier::new(identifier.to_string()));

            if next.is_some() && Operator::matches(&next.unwrap().token) {
                return Ok(Self::Binary(Binary::parse(stream, identifier)?));
            }

            if next.is_some() && Accessor::matches(&&next.unwrap().token) {
                return Ok(Accessor::parse(stream, Some(identifier))?);
            }

            return Ok(identifier);
        }

        if Literal::matches(&token) {
            let literal = Self::Literal(Literal::parse(stream)?);

            let next = stream.peek();

            if next.is_some() && !Operator::matches(&next.unwrap().token) {
                return Ok(Self::Binary(Binary::parse(stream, literal)?));
            }

            return Ok(literal);
        }

        if &TokenKind::LeftBracket == token {
            return Ok(Accessor::parse(stream, None)?);
        }

        if &TokenKind::LeftBrace == token {
            return Ok(Self::Map(Map::parse(stream)?));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!("Unexpected token {token}"),
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::statements::Statement;

    use super::*;

    #[test]
    fn new_expression_literal() {
        let expression = Expression::new_literal(Value::Number(1));
        assert_eq!(
            expression,
            Expression::Literal(Literal::new(Value::Number(1)))
        );
    }

    #[test]
    fn eval_expression_literal() {
        let context = &mut Context::new();
        let expression = Expression::new_literal(Value::Number(1));

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_identifier() {
        let expression = Expression::new_identifier("foo".to_string());
        assert_eq!(
            expression,
            Expression::Identifier(Identifier::new("foo".to_string()))
        );
    }

    #[test]
    fn eval_expression_identifier() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));
        let expression = Expression::new_identifier("foo".to_string());

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_function_call() {
        let expression = Expression::new_function_call(
            Expression::new_identifier("foo".to_string()),
            vec![Expression::new_literal(Value::Number(1))],
        );
        assert_eq!(
            expression,
            Expression::FunctionCall(FunctionCall::new(
                Expression::new_identifier("foo".to_string()),
                vec![Expression::new_literal(Value::Number(1))]
            ))
        );
    }

    #[test]
    fn eval_expression_function_call() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::new_function(vec![], Statement::new_break()),
        );
        let expression = Expression::new_function_call(
            Expression::new_identifier("foo".to_string()),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_binary() {
        let expression = Expression::new_binary(
            Expression::new_literal(Value::Number(1)),
            Operator::new_addition(),
            Expression::new_literal(Value::Number(2)),
        );
        assert_eq!(
            expression,
            Expression::Binary(Binary::new(
                Expression::new_literal(Value::Number(1)),
                Operator::new_addition(),
                Expression::new_literal(Value::Number(2)),
            ))
        );
    }

    #[test]
    fn eval_expression_binary() {
        let context = &mut Context::new();
        let expression = Expression::new_binary(
            Expression::new_literal(Value::Number(1)),
            Operator::new_addition(),
            Expression::new_literal(Value::Number(2)),
        );

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_collection() {
        let expression = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
        ]);
        assert_eq!(
            expression,
            Expression::Collection(Collection::new(vec![
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
            ]))
        );
    }

    #[test]
    fn eval_expression_collection() {
        let context = &mut Context::new();
        let expression = Expression::new_collection(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
        ]);

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_map() {
        let expression = Expression::new_map(vec![
            (
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
            ),
            (
                Expression::new_literal(Value::Number(3)),
                Expression::new_literal(Value::Number(4)),
            ),
        ]);
        assert_eq!(
            expression,
            Expression::Map(Map::new(vec![
                (
                    Expression::new_literal(Value::Number(1)),
                    Expression::new_literal(Value::Number(2)),
                ),
                (
                    Expression::new_literal(Value::Number(3)),
                    Expression::new_literal(Value::Number(4)),
                ),
            ]))
        );
    }

    #[test]
    fn eval_expression_map() {
        let context = &mut Context::new();
        let expression = Expression::new_map(vec![
            (
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
            ),
            (
                Expression::new_literal(Value::Number(3)),
                Expression::new_literal(Value::Number(4)),
            ),
        ]);

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }
}
