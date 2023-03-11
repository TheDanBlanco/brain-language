use brain_errors::{Error, ErrorKind};
use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use self::{
    context::Context, expressions::Expression, output::Output, statements::Statement,
    util::disambiguate_reassignment, value::Value,
};

pub mod context;
pub mod expressions;
pub mod output;
pub mod statements;
pub mod util;
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
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.peek();

        if next.is_none() {
            return Err(Error::new(
                ErrorKind::UnexpectedEndOfFile,
                "Expected any token, found End of File".into(),
            ));
        }

        let token = &next.unwrap().token;

        if Statement::matches(&token) || disambiguate_reassignment(stream) {
            return Ok(Node::from_statement(Statement::parse(stream)?));
        }

        Ok(Node::from_expression(Expression::parse(stream)?))
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

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn new_node_from_statement() {
        let statement = Statement::new_break();
        let node = Node::from_statement(statement.clone());

        assert_eq!(node, Node::Statement(statement))
    }

    #[test]
    fn new_node_from_expression() {
        let expression = Expression::new_literal(Value::Null);
        let node = Node::from_expression(expression.clone());

        assert_eq!(node, Node::Expression(expression))
    }

    #[test]
    fn parse_node_statement() {
        let tokens = vec![Token::new(0, 0, TokenKind::Break)];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Node::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Node::Statement(Statement::new_break()));
    }

    #[test]
    fn parse_node_expression() {
        let tokens = vec![Token::new(0, 0, TokenKind::String("a".to_string()))];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Node::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Node::Expression(Expression::new_literal(Value::String("a".to_string())))
        );
    }

    #[test]
    fn parse_node_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Node::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected any token, found End of File".to_string(),
        );
    }
}
