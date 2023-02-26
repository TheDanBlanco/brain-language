use crate::lang::{parser_new::{value::Value, context::Context}};

use super::{binary::Binary, collection::Collection, map::Map, functioncall::FunctionCall, identifier::Identifier, literal::Literal, operator::Operator};

pub trait Evaluatable {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<&Value, Box<dyn std::error::Error>>;
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
    fn eval<'a>(&'a self, context: &mut Context) -> Result<&Value, Box<dyn std::error::Error>> {
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