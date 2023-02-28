use crate::lang::grammar::{
    context::Context,
    expressions::{Evaluatable, Expression},
    output::Output,
    Resolveable,
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

impl Resolveable for Return {
    fn resolve(&self, context: &mut Context) -> Result<Output, Box<dyn std::error::Error>> {
        Ok(Output::Value(self.value.eval(context)?))
    }
}

#[cfg(test)]
mod tests {
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
}
