use brain_token::stream::TokenStream;

use crate::grammar::{
    context::Context,
    expressions::Expression,
    output::Output,
    token::BrainToken,
    value::{complex::ComplexValue, Value},
    Evaluate, Parse, Resolve,
};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct For {
    identifier: String,
    iterable: Expression,
    block: Box<Statement>,
}

impl For {
    pub fn new(identifier: String, iterable: Expression, block: Statement) -> Self {
        For {
            identifier,
            iterable,
            block: Box::new(block),
        }
    }
}

impl Resolve for For {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let iterable = self.iterable.evaluate(context)?;

        if let Value::Complex(ComplexValue::Collection(collection)) = iterable {
            for value in collection.value {
                context.symbols.insert(self.identifier.clone(), value);
                let out = self.block.resolve(context)?;

                if matches!(out, Output::Break) {
                    return Ok(out);
                }
            }
        }

        Ok(Output::None)
    }
}

impl Parse for For {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::For)?;

        let identifier = stream.expect(BrainToken::Identifier)?.clone();

        stream.expect(BrainToken::In)?;

        let collection = Expression::parse(stream)?;

        let block = Statement::parse(stream)?;

        Ok(Self::new(identifier.data, collection, block))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::grammar::{expressions::operator::Operator, Node};

    use super::*;

    #[test]
    fn new_for() {
        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);
        let block =
            Statement::new_block(vec![Node::from_expression(Expression::new_function_call(
                Expression::new_identifier("print".to_string()),
                vec![Expression::new_identifier("item".to_string())],
            ))]);

        let r#for = For::new(identifier.clone(), iterable.clone(), block.clone());

        assert_eq!(
            r#for,
            For {
                identifier: identifier,
                iterable: iterable,
                block: Box::new(block),
            }
        )
    }

    #[test]
    fn resolve_for() {
        let context = &mut Context::new();

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);
        let block = Statement::new_block(vec![]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
    }

    #[test]
    fn resolve_for_loop_count() {
        let context = &mut Context::new();
        context
            .symbols
            .insert("x".to_string(), Value::new_number(0));

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);

        let block = Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
            "x".to_string(),
            Expression::new_binary(
                Expression::new_identifier("x".to_string()),
                Operator::new_addition(),
                Expression::new_identifier("item".to_string()),
            ),
        ))]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);

        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::new_number(6),
        )
    }

    #[test]
    fn resolve_early_break() {
        let context = &mut Context::new();
        context
            .symbols
            .insert("x".to_string(), Value::new_number(0));

        let identifier = "item".to_string();
        let iterable = Expression::new_collection(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);

        let block = Statement::new_block(vec![
            Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_identifier("item".to_string()),
                ),
            )),
            Node::from_statement(Statement::new_break()),
        ]);

        let r#for = For::new(identifier, iterable, block);
        let result = r#for.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::Break);

        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::new_number(1),
        )
    }

    #[test]
    fn parse_loop() {
        let tokens = vec![
            Token::new(0..3, BrainToken::For, "for".to_string()),
            Token::new(4..8, BrainToken::Identifier, "item".to_string()),
            Token::new(9..11, BrainToken::In, "in".to_string()),
            Token::new(12..22, BrainToken::Identifier, "collection".to_string()),
            Token::new(23..24, BrainToken::LeftBrace, "{".to_string()),
            Token::new(25..26, BrainToken::RightBrace, "}".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = For::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            For::new(
                "item".to_string(),
                Expression::new_identifier("collection".to_string()),
                Statement::new_block(vec![])
            )
        );
    }

    #[test]
    fn parse_loop_eof() {
        let tokens = vec![Token::new(0..3, BrainToken::For, "for".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = For::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected Identifier, but found End of File".to_string()
        );
    }
}
