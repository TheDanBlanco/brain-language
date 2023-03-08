use crate::lang::{
    grammar::{context::Context, value::Value, Evaluate, Parse},
    tokens::{stream::TokenStream, tokenkind::TokenKind},
};

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

        Ok(Value::Collection(values))
    }
}

impl Parse for Collection {
    fn parse(stream: &mut TokenStream) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::LeftBracket)?;

        let mut elements = vec![];

        while !stream.check(TokenKind::RightBracket) {
            let expression = Expression::parse(stream)?;

            elements.push(expression);

            stream.skip_if(TokenKind::Comma);
        }

        stream.expect(TokenKind::RightBracket)?;

        stream.skip_if(TokenKind::Semicolon);

        Ok(Self::new(elements))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn create_new_collection() {
        let collection = Collection::new(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);
        assert_eq!(
            collection.elements,
            vec![
                Expression::new_literal(Value::Number(1)),
                Expression::new_literal(Value::Number(2)),
                Expression::new_literal(Value::Number(3)),
            ]
        );
    }

    #[test]
    fn eval_collection() {
        let context = &mut Context::new();
        let collection = Collection::new(vec![
            Expression::new_literal(Value::Number(1)),
            Expression::new_literal(Value::Number(2)),
            Expression::new_literal(Value::Number(3)),
        ]);

        let result = collection.evaluate(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::Collection(vec![Value::Number(1), Value::Number(2), Value::Number(3),])
        );
    }
}
