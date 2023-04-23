use brain_error::{Error, ErrorKind};
use brain_token::stream::TokenStream;

use crate::grammar::{context::Context, token::BrainToken, value::Value, Match, Parse};

use self::{
    bitwise::Bitwise, comparison::Comparison, logical::Logical, mathematical::Mathematical,
};

pub mod bitwise;
pub mod comparison;
pub mod logical;
pub mod mathematical;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Mathematical(Mathematical),
    Logical(Logical),
    Comparison(Comparison),
    Bitwise(Bitwise),
}

impl Operator {
    pub fn new_addition() -> Self {
        Operator::Mathematical(Mathematical::Add)
    }

    pub fn new_subtraction() -> Self {
        Operator::Mathematical(Mathematical::Subtract)
    }

    pub fn new_multiplication() -> Self {
        Operator::Mathematical(Mathematical::Multiply)
    }

    pub fn new_division() -> Self {
        Operator::Mathematical(Mathematical::Divide)
    }

    pub fn new_modulo() -> Self {
        Operator::Mathematical(Mathematical::Modulo)
    }

    pub fn new_and() -> Self {
        Operator::Logical(Logical::And)
    }

    pub fn new_or() -> Self {
        Operator::Logical(Logical::Or)
    }

    pub fn new_equal() -> Self {
        Operator::Comparison(Comparison::Equal)
    }

    pub fn new_not_equal() -> Self {
        Operator::Comparison(Comparison::NotEqual)
    }

    pub fn new_gt() -> Self {
        Operator::Comparison(Comparison::GreaterThan)
    }

    pub fn new_gte() -> Self {
        Operator::Comparison(Comparison::GreaterThanEqual)
    }

    pub fn new_lt() -> Self {
        Operator::Comparison(Comparison::LessThan)
    }

    pub fn new_lte() -> Self {
        Operator::Comparison(Comparison::LessThanEqual)
    }

    pub fn new_bitwise_and() -> Self {
        Operator::Bitwise(Bitwise::And)
    }

    pub fn new_bitwise_or() -> Self {
        Operator::Bitwise(Bitwise::Or)
    }

    pub fn evaluate(
        &self,
        left: Value,
        right: Value,
        _context: &mut Context,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Operator::Mathematical(mathematical) => mathematical.evaluate(left, right),
            Operator::Logical(logical) => logical.evaluate(left, right),
            Operator::Comparison(comparison) => comparison.evaluate(left, right),
            Operator::Bitwise(bitwise) => bitwise.evaluate(left, right),
        }
    }
}

impl Parse for Operator {
    fn parse(stream: &mut TokenStream<BrainToken>) -> Result<Self, Box<dyn std::error::Error>> {
        let next = stream.assert_next("Expected operator, found End of File".to_string())?;

        let token = &next.token;

        if Mathematical::matches(token) {
            return Ok(Operator::Mathematical(Mathematical::parse(token)));
        }

        if Logical::matches(token) {
            return Ok(Operator::Logical(Logical::parse(token)));
        }

        if Comparison::matches(token) {
            return Ok(Operator::Comparison(Comparison::parse(token)));
        }

        if Bitwise::matches(token) {
            return Ok(Operator::Bitwise(Bitwise::parse(token)));
        }

        return Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!(
                "Expected operator, found {token} ({} - {})",
                &next.span.start, &next.span.end
            ),
        ));
    }
}

impl Match for Operator {
    fn matches(token: &BrainToken) -> bool {
        Comparison::matches(token)
            | Logical::matches(token)
            | Mathematical::matches(token)
            | Bitwise::matches(token)
    }
}

#[cfg(test)]
mod tests {

    use brain_token::token::Token;

    use super::*;

    #[test]
    fn new_addition() {
        let operator = Operator::new_addition();

        assert_eq!(operator, Operator::Mathematical(Mathematical::Add))
    }

    #[test]
    fn new_subtraction() {
        let operator = Operator::new_subtraction();

        assert_eq!(operator, Operator::Mathematical(Mathematical::Subtract))
    }

    #[test]
    fn new_multiplication() {
        let operator = Operator::new_multiplication();

        assert_eq!(operator, Operator::Mathematical(Mathematical::Multiply))
    }

    #[test]
    fn new_division() {
        let operator = Operator::new_division();

        assert_eq!(operator, Operator::Mathematical(Mathematical::Divide))
    }

    #[test]
    fn new_modulo() {
        let operator = Operator::new_modulo();

        assert_eq!(operator, Operator::Mathematical(Mathematical::Modulo))
    }

    #[test]
    fn new_and() {
        let operator = Operator::new_and();

        assert_eq!(operator, Operator::Logical(Logical::And))
    }

    #[test]
    fn new_or() {
        let operator = Operator::new_or();

        assert_eq!(operator, Operator::Logical(Logical::Or))
    }

    #[test]
    fn new_equal() {
        let operator = Operator::new_equal();

        assert_eq!(operator, Operator::Comparison(Comparison::Equal))
    }

    #[test]
    fn new_not_equal() {
        let operator = Operator::new_not_equal();

        assert_eq!(operator, Operator::Comparison(Comparison::NotEqual))
    }

    #[test]
    fn new_lt() {
        let operator = Operator::new_lt();

        assert_eq!(operator, Operator::Comparison(Comparison::LessThan))
    }

    #[test]
    fn new_lte() {
        let operator = Operator::new_lte();

        assert_eq!(operator, Operator::Comparison(Comparison::LessThanEqual))
    }

    #[test]
    fn new_gt() {
        let operator = Operator::new_gt();

        assert_eq!(operator, Operator::Comparison(Comparison::GreaterThan))
    }

    #[test]
    fn new_gte() {
        let operator = Operator::new_gte();

        assert_eq!(operator, Operator::Comparison(Comparison::GreaterThanEqual))
    }

    #[test]
    fn eval_mathematical_operator() {
        let context = &mut Context::new();
        let left = Value::Number(1);
        let right = Value::Number(2);
        let operator = Operator::new_addition();

        let result = operator.evaluate(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_logical_operator() {
        let context = &mut Context::new();
        let left = Value::Boolean(true);
        let right = Value::Boolean(false);
        let operator = Operator::Logical(Logical::And);

        let result = operator.evaluate(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_comparison_operator() {
        let context = &mut Context::new();
        let left = Value::Number(1);
        let right = Value::Number(2);
        let operator = Operator::Comparison(Comparison::Equal);

        let result = operator.evaluate(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_bitwise_operator() {
        let context = &mut Context::new();
        let left = Value::Number(1);
        let right = Value::Number(2);
        let operator = Operator::Bitwise(Bitwise::And);

        let result = operator.evaluate(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_operator_mathematical() {
        let tokens = vec![Token::new(0..1, BrainToken::Plus, "+".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Operator::new_addition());
    }

    #[test]
    fn parse_operator_logical() {
        let tokens = vec![Token::new(0..2, BrainToken::And, "and".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Operator::new_and());
    }

    #[test]
    fn parse_operator_comparison() {
        let tokens = vec![Token::new(0..1, BrainToken::GreaterThan, ">".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Operator::new_gt());
    }

    #[test]
    fn parse_operator_bitwise_and() {
        let tokens = vec![Token::new(0..1, BrainToken::BitwiseAnd, "&".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Operator::new_bitwise_and());
    }

    #[test]
    fn parse_operator_bitwise_or() {
        let tokens = vec![Token::new(0..1, BrainToken::BitwiseOr, "|".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Operator::new_bitwise_or());
    }

    #[test]
    fn parse_operator_eof() {
        let tokens = vec![];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedEndOfFile]: Expected operator, found End of File".to_string()
        );
    }

    #[test]
    fn parse_operator_not_operator() {
        let tokens = vec![Token::new(0..1, BrainToken::LeftBrace, "{".to_string())];

        let stream = &mut TokenStream::from_vec(tokens);

        let result = Operator::parse(stream);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected operator, found Token::LeftBrace (0 - 1)".to_string()
        );
    }
}
