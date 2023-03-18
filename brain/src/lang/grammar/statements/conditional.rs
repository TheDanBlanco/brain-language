use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::lang::grammar::{
    context::Context, expressions::Expression, output::Output, token::BrainToken, value::Value,
    Evaluate, Parse, Resolve,
};

use super::Statement;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Conditional {
    condition: Expression,
    consequence: Box<Statement>,
    alternative: Box<Option<Statement>>,
}

impl Conditional {
    pub fn new(
        condition: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Self {
        Conditional {
            condition: condition,
            consequence: Box::new(consequence),
            alternative: Box::new(alternative),
        }
    }
}

impl Resolve for Conditional {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        let condition = self.condition.evaluate(context)?;

        if !matches!(
            condition,
            Value::Null | Value::Boolean(_) | Value::Number(_) | Value::String(_)
        ) {
            return Err(Error::new(
                ErrorKind::InvalidType,
                format!("'{condition}' cannot be used as a boolean."),
            ));
        }

        let truthy = match condition {
            Value::Boolean(true) => true,
            Value::String(string) if string.len() != 0 => true,
            Value::Number(number) if number != 0 => true,
            _ => false,
        };

        if truthy {
            return self.consequence.resolve(context);
        }

        if let Some(alternative) = *self.alternative.clone() {
            return alternative.resolve(context);
        }

        Ok(Output::None)
    }
}

impl Parse for Conditional {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::If)?;

        let condition = Expression::parse(stream)?;

        let consequence = Statement::parse(stream)?;

        let mut alternative = None;

        if stream.check(BrainToken::Else) {
            stream.skip();
            alternative = Some(Statement::parse(stream)?)
        }

        Ok(Self::new(condition, consequence, alternative))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::{expressions::operator::Operator, Node};

    use super::*;

    #[test]
    fn new_conditional() {
        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);

        assert_eq!(
            conditional,
            Conditional {
                condition,
                consequence: Box::new(consequence),
                alternative: Box::new(None),
            }
        )
    }

    #[test]
    fn new_conditional_with_alternate() {
        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_subtraction(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(
            condition.clone(),
            consequence.clone(),
            Some(alternate.clone()),
        );

        assert_eq!(
            conditional,
            Conditional {
                condition,
                consequence: Box::new(consequence),
                alternative: Box::new(Some(alternate)),
            }
        )
    }

    #[test]
    fn resolve_conditional_null() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Null);
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_subtraction(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_true_no_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_false_no_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(false));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(0)
        )
    }

    #[test]
    fn resolve_conditional_true_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(true));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_subtraction(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_false_alternate() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Boolean(false));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_subtraction(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_string_not_empty() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::String("test".to_string()));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_string_empty() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::String("".to_string()));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let alternate =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_subtraction(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), Some(alternate));
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(-1)
        )
    }

    #[test]
    fn resolve_conditional_number_not_zero() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Number(1));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(1)
        )
    }

    #[test]
    fn resolve_conditional_number_zero() {
        let context = &mut Context::new();
        context.symbols.insert("x".to_string(), Value::Number(0));

        let condition = Expression::new_literal(Value::Number(0));
        let consequence =
            Statement::new_block(vec![Node::from_statement(Statement::new_reassignment(
                "x".to_string(),
                Expression::new_binary(
                    Expression::new_identifier("x".to_string()),
                    Operator::new_addition(),
                    Expression::new_literal(Value::Number(1)),
                ),
            ))]);

        let conditional = Conditional::new(condition.clone(), consequence.clone(), None);
        let result = conditional.resolve(context);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Output::None);
        assert_eq!(
            context.symbols.get(&"x".to_string()).unwrap(),
            &Value::Number(0)
        )
    }

    #[test]
    fn resolve_conditional_invalid_conditional_collection() {
        let context = &mut Context::new();
        let condition = Expression::new_collection(vec![]);
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '[]' cannot be used as a boolean."
        );
    }

    #[test]
    fn resolve_conditional_invalid_conditional_map() {
        let context = &mut Context::new();
        let condition = Expression::new_map(vec![]);
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '{}' cannot be used as a boolean."
        );
    }

    #[test]
    fn resolve_conditional_invalid_conditional_function() {
        let context = &mut Context::new();
        let condition =
            Expression::new_literal(Value::new_function(vec![], Statement::new_break()));
        let consequence = Statement::new_block(vec![]);

        let conditional = Conditional::new(condition, consequence, None);
        let result = conditional.resolve(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '[function]' cannot be used as a boolean."
        );
    }

    #[test]
    fn parse_conditional() {
        let tokens = vec![
            Token::new(0..2, BrainToken::If, None),
            Token::new(3..4, BrainToken::True, None),
            Token::new(5..6, BrainToken::LeftBrace, None),
            Token::new(6..7, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Conditional::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Conditional::new(
                Expression::new_literal(Value::Boolean(true)),
                Statement::new_block(vec![]),
                None
            )
        );
    }

    #[test]
    fn parse_conditional_with_alternate() {
        let tokens = vec![
            Token::new(0..2, BrainToken::If, None),
            Token::new(3..4, BrainToken::True, None),
            Token::new(5..6, BrainToken::LeftBrace, None),
            Token::new(6..7, BrainToken::RightBrace, None),
            Token::new(8..10, BrainToken::Else, None),
            Token::new(11..12, BrainToken::LeftBrace, None),
            Token::new(12..13, BrainToken::RightBrace, None),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Conditional::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Conditional::new(
                Expression::new_literal(Value::Boolean(true)),
                Statement::new_block(vec![]),
                Some(Statement::new_block(vec![]))
            )
        );
    }
}
