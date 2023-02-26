use crate::lang::parser_new::{value::Value, error::{ErrorKind, Error}, context::Context};

use super::expression::{Expression, Evaluatable};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionCall {
    pub name: Box<Expression>,
    pub args: Vec<Expression>,
}

impl FunctionCall {
    pub fn new(name: Expression, args: Vec<Expression>) -> Self {
        FunctionCall {
            name: Box::new(name),
            args,
        }
    }
}

impl Evaluatable for FunctionCall {
    fn eval<'a>(&'a self, context: &mut Context) -> Result<&Value, Box<dyn std::error::Error>> {
        let unboxed_name = &self.name;
        let name = unboxed_name.eval(context)?;

        let function = match name {
            Value::Function(function, args) => Value::Function(function.clone(), args.clone()),
            Value::String(identifier) => {
                match context.symbols.get(identifier) {
                    Some(Value::Function(function, args)) => Value::Function(function.clone(), args.clone()),
                    Some(_) => return Err(Error::new(
                        ErrorKind::InvalidType,
                        format!("{} is not a function", identifier),
                    )),
                    None => 
                         return Err(Error::new(
                            ErrorKind::UnknownIdentifier,
                            format!("Unknown identifier: {identifier}"),
                        ))
                    
                };
                
                return Err(Error::new(
                    ErrorKind::UnknownIdentifier,
                    format!("Unknown identifier: {identifier}"),
                ))
            }
            _ => return Err(Error::new(
                ErrorKind::InvalidType,
                format!("{name} is not a function"),
            )),
        };
    
        return Ok(&function.clone())
    }
}