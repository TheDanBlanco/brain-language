use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Evaluate, Parse};

use super::Expression;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Collection {
    elements: Vec<Expression>,
}

impl Collection {
    pub fn new(elements: Vec<Expression>) -> Self {
        Collection { elements }
    }
}

impl Evaluate for Collection {
    fn evaluate(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let mut values = Vec::new();

        for element in &self.elements {
            let value = element.evaluate(context)?;
            values.push(value.clone());
        }

        Ok(Value::new_collection(values))
    }
}

impl Parse for Collection {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(BrainToken::LeftBracket)?;

        let mut elements = vec![];

        while !stream.check(BrainToken::RightBracket) {
            let expression = Expression::parse(stream)?;

            elements.push(expression);

            stream.skip_if(BrainToken::Comma);
        }

        stream.expect(BrainToken::RightBracket)?;

        stream.skip_if(BrainToken::Semicolon);

        Ok(Self::new(elements))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use super::*;

    #[test]
    fn create_new_collection() {
        let collection = Collection::new(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);
        assert_eq!(
            collection.elements,
            vec![
                Expression::new_literal(Value::new_number(1)),
                Expression::new_literal(Value::new_number(2)),
                Expression::new_literal(Value::new_number(3)),
            ]
        );
    }

    #[test]
    fn eval_collection() {
        let context = &mut Context::new();
        let collection = Collection::new(vec![
            Expression::new_literal(Value::new_number(1)),
            Expression::new_literal(Value::new_number(2)),
            Expression::new_literal(Value::new_number(3)),
        ]);

        let result = collection.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
                Value::new_number(3),
            ])
        );
    }

    #[test]
    fn parse_collection() {
        let tokens = vec![
            Token::new(0..1, BrainToken::LeftBracket, "[".to_string()),
            Token::new(1..2, BrainToken::Number, "0".to_string()),
            Token::new(2..3, BrainToken::RightBracket, "]".to_string()),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Collection::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Collection::new(vec![Expression::new_literal(Value::new_number(0))])
        );
    }
}
