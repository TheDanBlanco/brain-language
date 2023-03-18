use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{context::Context, output::Output, Node, Nodes, Parse, Resolve};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block {
    block: Nodes,
}

impl Block {
    pub fn new(nodes: Vec<Node>) -> Self {
        Block {
            block: Nodes::new(nodes),
        }
    }
}

impl Resolve for Block {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        for node in &self.block.nodes {
            let out = node.resolve(context)?;

            if matches!(out, Output::Break | Output::Continue) {
                return Ok(out);
            }
        }
        return Ok(Output::None);
    }
}

impl Parse for Block {
    fn parse(stream: &mut TokenStream<TokenKind>) -> Result<Self, Box<dyn std::error::Error>> {
        let mut nodes = vec![];

        stream.expect(TokenKind::LeftBrace)?;

        loop {
            if stream.check(TokenKind::RightBrace) {
                break;
            }

            let node = Node::parse(stream)?;
            nodes.push(node);
        }

        stream.expect(TokenKind::RightBrace)?;

        Ok(Self::new(nodes))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::{
        expressions::Expression, statements::Statement, value::Value, Node,
    };

    use super::*;

    #[test]
    fn new_block() {
        let expression = Expression::new_literal(Value::Number(1));
        let node = Node::from_expression(expression);
        let block = Block::new(vec![node.clone()]);

        assert_eq!(
            block,
            Block {
                block: Nodes::new(vec![node])
            }
        );
    }

    #[test]
    fn resolve_block_expression() {
        let context = &mut Context::new();

        let expression = Expression::new_literal(Value::Number(1));
        let node = Node::from_expression(expression);
        let block = Block::new(vec![node.clone()]);

        let result = block.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
    }

    #[test]
    fn resolve_block_statement_break() {
        let context = &mut Context::new();

        let statement = Statement::new_break();
        let node = Node::from_statement(statement);
        let block = Block::new(vec![node.clone()]);

        let result = block.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Break);
    }

    #[test]
    fn resolve_block_statement_continue() {
        let context = &mut Context::new();

        let statement = Statement::new_continue();
        let node = Node::from_statement(statement);
        let block = Block::new(vec![node.clone()]);

        let result = block.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Continue);
    }

    #[test]
    fn resolve_block_statement_none() {
        let context = &mut Context::new();

        let target = "foo".to_string();
        let expression = Expression::new_literal(Value::Number(1));
        let statement = Statement::new_assignment(target, expression);
        let node = Node::from_statement(statement);
        let block = Block::new(vec![node.clone()]);

        let result = block.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
    }

    #[test]
    fn parse_block() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::LeftBrace),
            Token::new(0, 0, TokenKind::RightBrace),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Block::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Block::new(vec![]));
    }
}
