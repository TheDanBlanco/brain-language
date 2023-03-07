use crate::lang::grammar::{context::Context, value::Value};

use self::{comparison::Comparison, logical::Logical, mathematical::Mathematical};

pub mod comparison;
pub mod logical;
pub mod mathematical;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Operator {
    Mathematical(Mathematical),
    Logical(Logical),
    Comparison(Comparison),
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
        }
    }
}

#[cfg(test)]
mod tests {
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
}
