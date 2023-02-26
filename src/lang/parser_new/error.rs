use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    KeyNotFound,
    InvalidType,
    IndexOutOfBounds,
    InvalidOperator,
    InvalidFunction,
    InvalidArgument,
    InvalidExpression,
    InvalidStatement,
    InvalidIdentifier,
    InvalidAssignment,
    InvalidVariable,
    InvalidFunctionCall,
    UnknownIdentifier,
    InvalidMapKey,
    InvalidLogicalOperation,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::KeyNotFound => write!(f, "Key not found"),
            ErrorKind::InvalidType => write!(f, "Invalid type"),
            ErrorKind::IndexOutOfBounds => write!(f, "Index out of bounds"),
            ErrorKind::InvalidOperator => write!(f, "Invalid operator"),
            ErrorKind::InvalidFunction => write!(f, "Invalid function"),
            ErrorKind::InvalidArgument => write!(f, "Invalid argument"),
            ErrorKind::InvalidExpression => write!(f, "Invalid expression"),
            ErrorKind::InvalidStatement => write!(f, "Invalid statement"),
            ErrorKind::InvalidIdentifier => write!(f, "Invalid identifier"),
            ErrorKind::InvalidAssignment => write!(f, "Invalid assignment"),
            ErrorKind::InvalidVariable => write!(f, "Invalid variable"),
            ErrorKind::InvalidFunctionCall => write!(f, "Invalid function call"),
            ErrorKind::UnknownIdentifier => write!(f, "Unknown identifier"),
            ErrorKind::InvalidMapKey => write!(f, "Invalid map key"),
            ErrorKind::InvalidLogicalOperation => write!(f, "Invalid logical operation"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String,
}

impl Error {
    pub fn new(kind: ErrorKind, message: String) -> Box<Self> {
        Box::new(Error { kind, message })
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}: {}", self.kind, self.message)
    }
}

impl std::error::Error for Error {}