use crate::lang::grammar::{context::Context, value::Value, Evaluate};

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
}
