use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::lang::grammar::{context::Context, value::Value};

use self::{
    accessors::Accessor, binary::Binary, collection::Collection, functioncall::FunctionCall,
    identifier::Identifier, literal::Literal, map::Map, operator::Operator,
};

use super::{token::BrainToken, Evaluate, Match, Parse};

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

    pub fn new_field_accessor(field: String, target: Expression) -> Self {
        Expression::Accessor(Accessor::new_field(field, target))
    }

    pub fn new_index_accessor(index: Expression, target: Expression) -> Self {
        Expression::Accessor(Accessor::new_index(index, target))
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
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.peek();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected literal, identifier, left brace, or left bracket, found End of File"
                    .to_string(),
            ));
        }

        let token = &next.unwrap().token;

        if Literal::matches(token) {
            return Ok(Self::Literal(Literal::parse(stream)?));
        }

        if stream.check(BrainToken::LeftBracket) {
            return Ok(Accessor::parse(stream, None)?);
        }

        if stream.check(BrainToken::LeftBrace) {
            return Ok(Self::Map(Map::parse(stream)?));
        }

        if let &BrainToken::Identifier = token {
            let mut expression = Self::Identifier(Identifier::parse(stream)?);
            while let Some(next) = stream.peek() {
                if Operator::matches(&next.token) {
                    expression = Self::Binary(Binary::parse(stream, Some(expression))?);
                    continue;
                }

                if Accessor::matches(&next.token) {
                    expression = Accessor::parse(stream, Some(expression))?;
                    continue;
                }

                if BrainToken::LeftParen == next.token {
                    expression =
                        Expression::FunctionCall(FunctionCall::parse(stream, Some(expression))?);
                    continue;
                }

                break;
            }

            return Ok(expression);
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!(
                "Expected literal, identifier, left brace, or left bracket, found {}",
                stream.peek().unwrap().token
            ),
        ));
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::{
        expressions::accessors::{field::Field, index::Index},
        statements::Statement,
    };

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
    fn parse_literal() {
        let tokens = vec![Token::new(0..1, BrainToken::Number, Some("1".to_string()))];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

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
    fn parse_identifier() {
        let tokens = vec![Token::new(
            0..2,
            BrainToken::Identifier,
            Some("foo".to_string()),
        )];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

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
    fn parse_function_call() {
        let tokens = vec![
            Token::new(0..2, BrainToken::Identifier, Some("foo".to_string())),
            Token::new(2..3, BrainToken::LeftParen, None),
            Token::new(3..4, BrainToken::Number, Some("1".to_string())),
            Token::new(4..5, BrainToken::RightParen, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

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
    fn parse_binary() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Number, Some("1".to_string())),
            Token::new(1..2, BrainToken::Plus, None),
            Token::new(2..3, BrainToken::Number, Some("2".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

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
    fn parse_collection() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBracket, None),
            Token::new(1..2, BrainToken::Number, Some("1".to_string())),
            Token::new(2..3, BrainToken::Comma, None),
            Token::new(3..4, BrainToken::Number, Some("2".to_string())),
            Token::new(4..5, BrainToken::RightBracket, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

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

    #[test]
    fn parse_map() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBrace, None),
            Token::new(1..2, BrainToken::Identifier, Some("a".to_string())),
            Token::new(2..3, BrainToken::Colon, None),
            Token::new(3..4, BrainToken::Number, Some("1".to_string())),
            Token::new(4..5, BrainToken::Comma, None),
            Token::new(5..6, BrainToken::Identifier, Some("b".to_string())),
            Token::new(6..7, BrainToken::Colon, None),
            Token::new(7..8, BrainToken::Number, Some("2".to_string())),
            Token::new(8..9, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Map::parse(stream);

        assert!(result.is_ok());
    }

    #[test]
    fn new_index_accessor() {
        let expression = Expression::new_index_accessor(
            Expression::new_literal(Value::Number(0)),
            Expression::new_collection(vec![Expression::new_literal(Value::Null)]),
        );
        assert_eq!(
            expression,
            Expression::Accessor(Accessor::Index(Index::new(
                Expression::new_literal(Value::Number(0)),
                Expression::new_collection(vec![Expression::new_literal(Value::Null)])
            )))
        );
    }

    #[test]
    fn eval_index_accessor() {
        let context = &mut Context::new();
        let expression = Expression::Accessor(Accessor::Index(Index::new(
            Expression::new_literal(Value::Number(0)),
            Expression::new_collection(vec![Expression::new_literal(Value::Number(0))]),
        )));

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_index_accessor() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(1..2, BrainToken::LeftBracket, None),
            Token::new(2..3, BrainToken::Number, Some("0".to_string())),
            Token::new(3..4, BrainToken::RightBracket, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
    }

    #[test]
    fn new_field_accessor() {
        let expression = Expression::new_field_accessor(
            "a".to_string(),
            Expression::new_map(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(0)),
            )]),
        );
        assert_eq!(
            expression,
            Expression::Accessor(Accessor::Field(Field::new(
                "a".to_string(),
                Expression::new_map(vec![(
                    Expression::new_literal(Value::String("a".to_string())),
                    Expression::new_literal(Value::Number(0))
                )])
            )))
        );
    }

    #[test]
    fn eval_field_accessor() {
        let context = &mut Context::new();
        let expression = Expression::new_field_accessor(
            "a".to_string(),
            Expression::new_map(vec![(
                Expression::new_literal(Value::String("a".to_string())),
                Expression::new_literal(Value::Number(0)),
            )]),
        );

        let result = expression.evaluate(context);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_field_accessor() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(1..2, BrainToken::Dot, None),
            Token::new(2..3, BrainToken::Identifier, Some("b".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
    }

    #[test]
    fn parse_double_function_call() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(1..2, BrainToken::LeftParen, None),
            Token::new(2..3, BrainToken::RightParen, None),
            Token::new(3..4, BrainToken::LeftParen, None),
            Token::new(4..5, BrainToken::RightParen, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::new_function_call(
                Expression::new_function_call(Expression::new_identifier("a".to_string()), vec![]),
                vec![]
            ),
        )
    }

    #[test]
    fn parse_triple_function_call() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(1..2, BrainToken::LeftParen, None),
            Token::new(2..3, BrainToken::RightParen, None),
            Token::new(3..4, BrainToken::LeftParen, None),
            Token::new(4..5, BrainToken::RightParen, None),
            Token::new(5..6, BrainToken::LeftParen, None),
            Token::new(6..7, BrainToken::RightParen, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::new_function_call(
                Expression::new_function_call(
                    Expression::new_function_call(
                        Expression::new_identifier("a".to_string()),
                        vec![]
                    ),
                    vec![]
                ),
                vec![]
            ),
        )
    }

    #[test]
    fn parse_function_call_accessor() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(3..4, BrainToken::LeftBracket, None),
            Token::new(4..5, BrainToken::Number, Some("0".to_string())),
            Token::new(5..6, BrainToken::RightBracket, None),
            Token::new(1..2, BrainToken::LeftParen, None),
            Token::new(2..3, BrainToken::RightParen, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::new_function_call(
                Expression::new_index_accessor(
                    Expression::new_literal(Value::Number(0)),
                    Expression::new_identifier("a".to_string())
                ),
                vec![]
            )
        )
    }

    #[test]
    fn parse_accessor_binary() {
        let tokens = vec![
            Token::new(0..1, BrainToken::Identifier, Some("a".to_string())),
            Token::new(1..2, BrainToken::LeftBracket, None),
            Token::new(2..3, BrainToken::Number, Some("0".to_string())),
            Token::new(3..4, BrainToken::RightBracket, None),
            Token::new(4..5, BrainToken::Plus, None),
            Token::new(5..6, BrainToken::Identifier, Some("b".to_string())),
            Token::new(6..7, BrainToken::LeftBracket, None),
            Token::new(7..8, BrainToken::Number, Some("0".to_string())),
            Token::new(8..9, BrainToken::RightBracket, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Expression::new_binary(
                Expression::new_index_accessor(
                    Expression::new_literal(Value::Number(0)),
                    Expression::new_identifier("a".to_string())
                ),
                Operator::new_addition(),
                Expression::new_index_accessor(
                    Expression::new_literal(Value::Number(0)),
                    Expression::new_identifier("b".to_string())
                ),
            )
        )
    }

    #[test]
    fn parse_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected literal, identifier, left brace, or left bracket, found End of File".to_string()
        );
    }

    #[test]
    fn parse_eof_unexpected_token() {
        let tokens = vec![Token::new(0..1, BrainToken::Let, None)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Expression::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected literal, identifier, left brace, or left bracket, found Token::Let".to_string()
        );
    }
}
