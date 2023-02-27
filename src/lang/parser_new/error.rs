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
    InvalidMathematicalOperation,
    InvalidComparisonOperation,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::KeyNotFound => write!(f, "KeyNotFound"),
            ErrorKind::InvalidType => write!(f, "InvalidType"),
            ErrorKind::IndexOutOfBounds => write!(f, "IndexOutOfBounds"),
            ErrorKind::InvalidOperator => write!(f, "InvalidOperator"),
            ErrorKind::InvalidFunction => write!(f, "InvalidFunction"),
            ErrorKind::InvalidArgument => write!(f, "InvalidArgument"),
            ErrorKind::InvalidExpression => write!(f, "InvalidExpression"),
            ErrorKind::InvalidStatement => write!(f, "InvalidStatement"),
            ErrorKind::InvalidIdentifier => write!(f, "InvalidIdentifier"),
            ErrorKind::InvalidAssignment => write!(f, "InvalidAssignment"),
            ErrorKind::InvalidVariable => write!(f, "InvalidVariable"),
            ErrorKind::InvalidFunctionCall => write!(f, "InvalidFunctionCall"),
            ErrorKind::UnknownIdentifier => write!(f, "UnknownIdentifier"),
            ErrorKind::InvalidMapKey => write!(f, "InvalidMapKey"),
            ErrorKind::InvalidLogicalOperation => write!(f, "InvalidLogicalOperation"),
            ErrorKind::InvalidMathematicalOperation => write!(f, "InvalidMathematicalOperation"),
            ErrorKind::InvalidComparisonOperation => write!(f, "InvalidComparisonOperation"),
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
        write!(f, "[{}]: {}", self.kind, self.message)
    }
}

impl std::error::Error for Error {}
