use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

use super::Expression;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Tuple {
    pub value: Vec<Expression>,
}

impl Tuple {
    pub fn new(values: Vec<Expression>) -> Self {
        Self { value: values }
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

        for value in &self.value {
            values.push(value.evaluate(context)?);
        }

        Ok(Value::Tuple(values))
    }
}
