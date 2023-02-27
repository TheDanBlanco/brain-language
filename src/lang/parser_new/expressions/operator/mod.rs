use crate::lang::parser_new::{context::Context, value::Value};

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
    pub fn eval(
        &self,
        left: Value,
        right: Value,
        _context: &mut Context,
    ) -> Result<Value, Box<dyn std::error::Error>> {
        match self {
            Operator::Mathematical(mathematical) => mathematical.eval(left, right),
            Operator::Logical(logical) => logical.eval(left, right),
            Operator::Comparison(comparison) => comparison.eval(left, right),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn eval_mathematical_operator() {
        let context = &mut Context::new();
        let left = Value::Number(1);
        let right = Value::Number(2);
        let operator = Operator::Mathematical(Mathematical::Add);

        let result = operator.eval(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_logical_operator() {
        let context = &mut Context::new();
        let left = Value::Boolean(true);
        let right = Value::Boolean(false);
        let operator = Operator::Logical(Logical::And);

        let result = operator.eval(left, right, context);
        assert!(result.is_ok());
    }

    #[test]
    fn eval_comparison_operator() {
        let context = &mut Context::new();
        let left = Value::Number(1);
        let right = Value::Number(2);
        let operator = Operator::Comparison(Comparison::Equal);

        let result = operator.eval(left, right, context);
        assert!(result.is_ok());
    }
}
