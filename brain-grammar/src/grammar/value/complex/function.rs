use core::fmt;

use crate::grammar::statements::Statement;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct Function {
    pub arguments: Vec<String>,
    pub body: Box<Statement>,
}

impl Function {
    pub fn new(arguments: Vec<String>, body: Statement) -> Self {
        Self {
            arguments,
            body: Box::new(body),
        }
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[function]")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_function() {
        let function = Function::new(
            vec!["a".to_string(), "b".to_string()],
            Statement::new_break(),
        );

        assert_eq!(
            function,
            Function {
                arguments: vec!["a".to_string(), "b".to_string()],
                body: Box::new(Statement::new_break()),
            }
        );
    }

    #[test]
    fn display_function() {
        let function = Function::new(
            vec!["a".to_string(), "b".to_string()],
            Statement::new_break(),
        );

        assert_eq!(format!("{}", function), "[function]");
    }
}
