use crate::lang::parser_new::{context::Context, value::Value};

use super::{
    binary::Binary, collection::Collection, functioncall::FunctionCall, identifier::Identifier,
    literal::Literal, map::Map, operator::Operator,
};

pub trait Evaluatable {
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>>;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression {
    Binary(Binary),
    Collection(Collection),
    Literal(Literal),
    Identifier(Identifier),
    FunctionCall(FunctionCall),
    Map(Map),
}

impl Expression {
    pub fn new_binary(lhs: Expression, rhs: Expression, operator: Operator) -> Self {
        Expression::Binary(Binary::new(lhs, rhs, operator))
    }

    pub fn new_collection(elements: Vec<Expression>) -> Self {
        Expression::Collection(Collection::new(elements))
    }

    pub fn new_literal(value: Value) -> Self {
        Expression::Literal(Literal::new(value))
    }

    pub fn new_identifier(identifier: Identifier) -> Self {
        Expression::Identifier(identifier)
    }

    pub fn new_function_call(name: Expression, args: Vec<Expression>) -> Self {
        Expression::FunctionCall(FunctionCall::new(name, args))
    }

    pub fn new_map(pairs: Vec<(Expression, Expression)>) -> Self {
        Expression::Map(Map::new(pairs))
    }
}

impl Evaluatable for Expression {
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Expression::Binary(binary) => binary.eval(context),
            Expression::Collection(collection) => collection.eval(context),
            Expression::Literal(literal) => literal.eval(context),
            Expression::FunctionCall(function_call) => function_call.eval(context),
            Expression::Identifier(identifier) => identifier.eval(context),
            Expression::Map(map) => map.eval(context),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::parser_new::expressions::operator::mathematical::Mathematical;

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

        let result = expression.eval(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_identifier() {
        let expression = Expression::new_identifier(Identifier::new("foo".to_string()));
        assert_eq!(
            expression,
            Expression::Identifier(Identifier::new("foo".to_string()))
        );
    }

    #[test]
    fn eval_expression_identifier() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));
        let expression = Expression::new_identifier(Identifier::new("foo".to_string()));

        let result = expression.eval(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_function_call() {
        let expression = Expression::new_function_call(
            Expression::new_identifier(Identifier::new("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );
        assert_eq!(
            expression,
            Expression::FunctionCall(FunctionCall::new(
                Expression::new_identifier(Identifier::new("foo".to_string())),
                vec![Expression::new_literal(Value::Number(1))]
            ))
        );
    }

    #[test]
    fn eval_expression_function_call() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::Function(
                Box::new(Expression::new_literal(Value::String("bar".to_string()))),
                vec![],
            ),
        );
        let expression = Expression::new_function_call(
            Expression::new_identifier(Identifier::new("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = expression.eval(context);
        assert!(result.is_ok());
    }

    #[test]
    fn new_expression_binary() {
        let expression = Expression::new_binary(
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Operator::Mathematical(Mathematical::Add),
        );
        assert_eq!(
            expression,
            Expression::Binary(Binary::new(
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
                Operator::Mathematical(Mathematical::Add)
            ))
        );
    }

    #[test]
    fn eval_expression_binary() {
        let context = &mut Context::new();
        let expression = Expression::new_binary(
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Operator::Mathematical(Mathematical::Add),
        );

        let result = expression.eval(context);
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

        let result = expression.eval(context);
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

        let result = expression.eval(context);
        assert!(result.is_ok());
    }
}
