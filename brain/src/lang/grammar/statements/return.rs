use brain_token::{stream::TokenStream, tokenkind::TokenKind};

use crate::lang::grammar::{
    context::Context, expressions::Expression, output::Output, Evaluate, Parse, Resolve,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Return {
    value: Expression,
}

impl Return {
    pub fn new(value: Expression) -> Self {
        Return { value }
    }
}

impl Resolve for Return {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Value(self.value.evaluate(context)?))
    }
}

impl Parse for Return {
    fn parse(stream: &mut TokenStream<TokenKind>) -> Result<Self, Box<dyn std::error::Error>> {
        stream.expect(TokenKind::Return)?;

        let expression = Expression::parse(stream)?;

        stream.skip_if(TokenKind::Semicolon);

        Ok(Self::new(expression))
    }
}

#[cfg(test)]
mod tests {
    use brain_token::token::Token;

    use crate::lang::grammar::value::Value;

    use super::*;

    #[test]
    fn new_return() {
        let expression = Expression::new_literal(Value::String("hello".to_string()));
        let statement = Return::new(expression.clone());

        assert_eq!(statement, Return { value: expression })
    }

    #[test]
    fn resolve_return() {
        let context = &mut Context::new();
        let expression = Expression::new_literal(Value::String("hello".to_string()));
        let statement = Return::new(expression.clone());

        let result = statement.resolve(context);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Output::Value(Value::String("hello".to_string()))
        );
    }

    #[test]
    fn parse_return() {
        let tokens = vec![
            Token::new(0, 0, TokenKind::Return),
            Token::new(0, 0, TokenKind::Number(0)),
        ];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Return::parse(stream);

        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Return::new(Expression::new_literal(Value::Number(0)))
        );
    }
}
