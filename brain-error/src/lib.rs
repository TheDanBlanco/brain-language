use core::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    KeyNotFound,
    InvalidType,
    IndexOutOfBounds,
    UnknownIdentifier,
    UnexpectedExpression,
    UnknownEnum,
    UnknownEnumVariant,
    InvalidMapKey,
    InvalidLogicalOperation,
    InvalidMathematicalOperation,
    InvalidComparisonOperation,
    InvalidBitwiseOperation,
    IdentifierAlreadyExists,
    UnexpectedToken,
    UnexpectedEndOfFile,
    ParseError,
    ImmutableValue,
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorKind::KeyNotFound => write!(f, "KeyNotFound"),
            ErrorKind::InvalidType => write!(f, "InvalidType"),
            ErrorKind::IndexOutOfBounds => write!(f, "IndexOutOfBounds"),
            ErrorKind::UnknownIdentifier => write!(f, "UnknownIdentifier"),
            ErrorKind::InvalidMapKey => write!(f, "InvalidMapKey"),
            ErrorKind::InvalidLogicalOperation => write!(f, "InvalidLogicalOperation"),
            ErrorKind::UnknownEnum => write!(f, "UnknownEnum"),
            ErrorKind::UnknownEnumVariant => write!(f, "UnknownEnumVariant"),
            ErrorKind::InvalidMathematicalOperation => write!(f, "InvalidMathematicalOperation"),
            ErrorKind::InvalidComparisonOperation => write!(f, "InvalidComparisonOperation"),
            ErrorKind::InvalidBitwiseOperation => write!(f, "InvalidBitwiseOperation"),
            ErrorKind::IdentifierAlreadyExists => write!(f, "IdentifierAlreadyExists"),
            ErrorKind::UnexpectedToken => write!(f, "UnexpectedToken"),
            ErrorKind::UnexpectedEndOfFile => write!(f, "UnexpectedEndOfFile"),
            ErrorKind::UnexpectedExpression => write!(f, "UnexpectedExpression"),
            ErrorKind::ParseError => write!(f, "ParseError"),
            ErrorKind::ImmutableValue => write!(f, "ImmutableValue"),
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
