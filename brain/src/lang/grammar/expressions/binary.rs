use crate::lang::{
    grammar::{context::Context, value::Value, Evaluate, Parse},
    tokens::stream::TokenStream,
};

use super::{operator::Operator, Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    operator: Operator,
}

impl Binary {
    pub fn new(lhs: Expression, operator: Operator, rhs: Expression) -> Self {
        Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator,
        }
    }

    pub fn parse(
        stream: &mut TokenStream,
        initial: Option<Expression>,
    ) -> Result<Self, Box<dyn std::error::Error>> {
        let lhs = match initial {
            Some(expression) => expression,
            None => Expression::parse(stream)?,
        };

        println!("{lhs:#?}");

        let operator = Operator::parse(stream)?;
        let rhs = Expression::parse(stream)?;

        Ok(Self::new(lhs, operator, rhs))
    }
}

impl Evaluate for Binary {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let left = self.lhs.evaluate(context)?;
        let right = self.rhs.evaluate(context)?;

        self.operator.evaluate(left, right, context)
    }
}

#[cfg(test)]
mod test {
    use crate::lang::tokens::{token::Token, tokenkind::TokenKind};

    use super::*;

    #[test]
    fn new_binary() {
        let lhs = Expression::new_literal(Value::Number(1));
        let rhs = Expression::new_literal(Value::Number(2));
        let operator = Operator::new_addition();

        let binary = Binary::new(lhs, operator, rhs);
        assert_eq!(
            binary.lhs,
            Box::new(Expression::new_literal(Value::Number(1)))
        );
        assert_eq!(
            binary.rhs,
            Box::new(Expression::new_literal(Value::Number(2)))
        );
        assert_eq!(binary.operator, Operator::new_addition());
    }

    #[test]
    fn eval_binary() {
        let context = &mut Context::new();
        let lhs = Expression::new_literal(Value::Number(1));
        let rhs = Expression::new_literal(Value::Number(2));
        let operator = Operator::new_addition();
        let binary = Binary::new(lhs, operator, rhs);

        let result = binary.evaluate(context);

        assert!(result.is_ok());
    }

    #[test]
    fn parse_binary() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Number(0)),
            Token::new(0, 0, TokenKind::LessThan),
            Token::new(0, 0, TokenKind::Number(1)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Binary::parse(stream, None);

        // assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Binary::new(
                Expression::new_literal(Value::Number(0)),
                Operator::new_lt(),
                Expression::new_literal(Value::Number(1))
            )
        );
    }

    #[test]
    fn parse_binary_with_initial() {
        let expression = Expression::new_literal(Value::Number(0));

        let tokens = vec![
            Token::new(0, 0, TokenKind::LessThan),
            Token::new(0, 0, TokenKind::Number(1)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Binary::parse(stream, Some(expression));

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Binary::new(
                Expression::new_literal(Value::Number(0)),
                Operator::new_lt(),
                Expression::new_literal(Value::Number(1))
            )
        );
    }
}
