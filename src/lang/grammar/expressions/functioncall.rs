use crate::lang::grammar::{
    context::Context,
    error::{Error, ErrorKind},
    value::Value,
};

use super::{Evaluatable, Expression};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall {
    pub identifier: Box<Expression>,
    pub args: Vec<Expression>,
}

impl FunctionCall {
    pub fn new(identifier: Expression, args: Vec<Expression>) -> Self {
        FunctionCall {
            identifier: Box::new(identifier),
            args,
        }
    }
}

impl Evaluatable for FunctionCall {
    fn eval(&self, context: &mut Context) -> Result<Value, Box<dyn std::error::Error>> {
        let identifier = self.identifier.eval(context)?;

        if let Value::Function(function, args) = identifier {
            return Ok(Value::Function(function.clone(), args.clone()));
        }

        if let Value::String(fn_identifier) = identifier {
            match context.symbols.get(&fn_identifier) {
                Some(Value::Function(function, args)) => {
                    return Ok(Value::Function(function.clone(), args.clone()));
                }
                Some(_) => {
                    return Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("'{}' is not a function", fn_identifier),
                    ))
                }
                None => {
                    return Err(Error::new(
                        ErrorKind::UnknownIdentifier,
                        format!("'{fn_identifier}'"),
                    ))
                }
            };
        }

        return Err(Error::new(
            ErrorKind::InvalidType,
            format!("'{identifier}' is not a neither a function nor an identifier"),
        ));
    }
}

#[cfg(test)]
mod tests {
    use crate::lang::grammar::statements::Statement;

    use super::*;

    #[test]
    fn create_new_function_call() {
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );
        assert_eq!(
            function_call.identifier,
            Box::new(Expression::new_literal(Value::String("foo".to_string())))
        );
        assert_eq!(
            function_call.args,
            vec![Expression::new_literal(Value::Number(1))]
        );
    }

    #[test]
    fn eval_function_call_identifier_is_string() {
        let context = &mut Context::new();
        context.symbols.insert(
            "foo".to_string(),
            Value::new_function(vec![], Statement::new_break()),
        );

        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![],
        );

        let result = function_call.eval(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_function(vec![], Statement::new_break()),
        );
    }

    #[test]
    fn eval_function_call_identifier_is_function() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::new_function(vec![], Statement::new_break())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.eval(context);
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap(),
            Value::new_function(vec![], Statement::new_break()),
        );
    }

    #[test]
    fn eval_function_call_unknown_identifier() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.eval(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[UnknownIdentifier]: 'foo'"
        )
    }

    #[test]
    fn eval_function_call_invalid_type() {
        let context = &mut Context::new();
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::Number(1)),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.eval(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: '1' is not a neither a function nor an identifier"
        )
    }

    #[test]
    fn eval_function_type_in_symbol_table_not_a_function() {
        let context = &mut Context::new();
        context.symbols.insert("foo".to_string(), Value::Number(1));
        let function_call = FunctionCall::new(
            Expression::new_literal(Value::String("foo".to_string())),
            vec![Expression::new_literal(Value::Number(1))],
        );

        let result = function_call.eval(context);

        assert!(result.is_err());
        assert_eq!(
            result.err().unwrap().to_string(),
            "[InvalidType]: 'foo' is not a function"
        )
    }
}
