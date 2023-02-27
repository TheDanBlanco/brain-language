use crate::lang::parser_new::{context::Context, value::Value};

use super::{
    expression::{Evaluatable, Expression},
    operator::Operator,
};

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
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let left = self.lhs.eval(context)?;
        let right = self.rhs.eval(context)?;

        self.operator.eval(left, right, context)
    }
}

#[cfg(test)]
mod test {
    use crate::lang::parser_new::expressions::operator::mathematical::Mathematical;

    use super::*;

    #[test]
    fn new_binary() {
        let lhs = Expression::new_literal(Value::Number(1));
        let rhs = Expression::new_literal(Value::Number(2));
        let operator = Operator::Mathematical(Mathematical::Add);

        let binary = Binary::new(lhs, rhs, operator);
        assert_eq!(
            binary.lhs,
            Box::new(Expression::new_literal(Value::Number(1)))
        );
        assert_eq!(
            binary.rhs,
            Box::new(Expression::new_literal(Value::Number(2)))
        );
        assert_eq!(binary.operator, Operator::Mathematical(Mathematical::Add));
    }

    #[test]
    fn eval_binary() {
        let context = &mut Context::new();
        let lhs = Expression::new_literal(Value::Number(1));
        let rhs = Expression::new_literal(Value::Number(2));
        let operator = Operator::Mathematical(Mathematical::Add);
        let binary = Binary::new(lhs, rhs, operator);

        let result = binary.eval(context);

        assert!(result.is_ok());
    }
}
