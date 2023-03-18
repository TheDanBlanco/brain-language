use brain_token::stream::TokenStream;

use crate::lang::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

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
        stream: &mut TokenStream<BrainToken>,
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
    use brain_token::token::Token;

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
            Token::new(0..1, BrainToken::Number, Some("0".to_string())),
            Token::new(0..1, BrainToken::LessThan, None),
            Token::new(0..1, BrainToken::Number, Some("1".to_string())),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Binary::parse(stream, None);

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

    #[test]
    fn parse_binary_with_initial() {
        let expression = Expression::new_literal(Value::Number(0));

        let tokens = vec![
            Token::new(0..1, BrainToken::LessThan, None),
            Token::new(0..1, BrainToken::Number, Some("1".to_string())),
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
