pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Power,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or,
    Not,
}

impl Operator {
    pub fn new() -> Self {
        todo!()
    }

    pub fn eval(left: Value, right: Value) -> Result<Value, Error> {
        match self {
            Operator::Add => todo!(),
            Operator::Subtract => todo!(),
            Operator::Multiply => todo!(),
            Operator::Divide => todo!(),
            Operator::Modulo => todo!(),
            Operator::Power => todo!(),
            Operator::Equal => todo!(),
            Operator::NotEqual => todo!(),
            Operator::GreaterThan => todo!(),
            Operator::GreaterThanOrEqual => todo!(),
            Operator::LessThan => todo!(),
            Operator::LessThanOrEqual => todo!(),
            Operator::And => todo!(),
            Operator::Or => todo!(),
            Operator::Not => todo!(),
        }
    }
}