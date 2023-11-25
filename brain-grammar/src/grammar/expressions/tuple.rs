use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

use super::Expression;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Tuple {
    pub values: Vec<Expression>,
}

impl Tuple {
    pub fn new(values: Vec<Expression>) -> Self {
        Self { values }
    }
}

impl Parse for Tuple {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::LeftParen)?;

        let mut values = Vec::new();

        while !stream.check(BrainToken::RightParen) {
            values.push(Expression::parse(stream)?);

            stream.skip_if(BrainToken::Comma);
        }

        stream.expect(BrainToken::RightParen)?;

        stream.skip_if(BrainToken::Semicolon);

        Ok(Self::new(values))
    }
}

impl Evaluate for Tuple {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut values = Vec::new();

        for value in &self.values {
            values.push(value.evaluate(context)?);
        }

        Ok(Value::new_tuple(values))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn create_new_tuple() {
        let tuple = Tuple::new(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);
        assert_eq!(
            tuple.values,
            vec![
                Expression::new_literal(Value::new_number(1)),
                Expression::new_literal(Value::new_number(2)),
                Expression::new_literal(Value::new_number(3)),
            ]
        );
    }

    #[test]
    fn eval_tuple() {
        let context = &mut Context::new();
        let collection = Tuple::new(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);

        let result = collection.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_tuple(vec![
                Value::new_number(1),
                Value::new_number(2),
                Value::new_number(3),
            ])
        );
    }

    #[test]
    fn parse_tuple() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftParen, "(".to_string()),
            Token::new(1..2, BrainToken::Number, "0".to_string()),
            Token::new(2..3, BrainToken::RightParen, ")".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Tuple::parse(stream);

        assert_eq!(
            result.unwrap(),
            Tuple::new(vec![Expression::new_literal(Value::new_number(0))])
        );
    }
}
