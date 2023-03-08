use self::{
    context::Context, expressions::Expression, output::Output, statements::Statement, value::Value,
};

use super::tokens::{stream::TokenStream, tokenkind::TokenKind};

pub mod context;
pub mod error;
pub mod expressions;
pub mod output;
pub mod statements;
pub mod value;

pub trait Resolve {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>>;
}

pub trait Evaluate {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>>;
}

pub trait Parse
where
    Self: Sized,
{
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>>;
}

pub trait Match {
    fn matches(token: &TokenKind) -> bool;
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

impl Node {
    pub fn from_statement(statement: Statement) -> Self {
        Node::Statement(statement)
    }

    pub fn from_expression(expression: Expression) -> Self {
        Node::Expression(expression)
    }
}

impl Parse for Node {
    fn parse(_stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        todo!()
    }
}

impl Resolve for Node {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        match self {
            Node::Expression(expression) => {
                expression.evaluate(context)?;

                return Ok(Output::None);
            }
            Node::Statement(statement) => statement.resolve(context),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Nodes {
    nodes: Vec<Node>,
}

impl Nodes {
    pub fn new(nodes: Vec<Node>) -> Self {
        Nodes { nodes: nodes }
    }
}
