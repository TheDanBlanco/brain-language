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
