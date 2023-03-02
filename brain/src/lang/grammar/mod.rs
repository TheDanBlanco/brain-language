use self::{
    context::Context,
    expressions::{Evaluatable, Expression},
    output::Output,
    statements::Statement,
};

pub mod context;
pub mod error;
pub mod expressions;
pub mod output;
pub mod statements;
pub mod value;

pub trait Resolveable {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>>;
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

impl Resolveable for Node {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        match self {
            Node::Expression(expression) => {
                expression.eval(context)?;

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
