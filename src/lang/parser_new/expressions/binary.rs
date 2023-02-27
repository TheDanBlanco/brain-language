use crate::lang::{parser_new::{value::Value, context::Context}};

use super::{expression::{Expression, Evaluatable}, operator::Operator};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Binary {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
    operator: Operator,
}

impl Binary {
    pub fn new(lhs: Expression, rhs: Expression, operator: Operator) -> Self {
        Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            operator,
        }
    }
}

impl Evaluatable for Binary {
    fn eval<'a, 'b>(&'a self, context: &'b mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let left = self.lhs.eval(context)?;
        let right = self.rhs.eval(context)?;

        self.operator.eval(left, right, context)
    }
}