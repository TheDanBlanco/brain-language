pub struct BinaryExpression {
    lhs: Expression,
    rhs: Expression,
    operator: Operator,
}

impl BinaryExpression {
    pub fn new(lhs: Expression, rhs: Expression, operator: Operator) -> Self {
        BinaryExpression {
            lhs,
            rhs,
            operator,
        }
    }

    pub fn eval() -> Result<Value, Error> {
        let left = lhs.eval()?;
        let right = rhs.eval()?;

        return operator.eval(left, right);
    }
}