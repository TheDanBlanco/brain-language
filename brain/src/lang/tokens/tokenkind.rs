use core::fmt;

use crate::lang::grammar::error::{Error, ErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    None,
    Continue,
    Semicolon,
    Comma,
    Colon,
    Dot,
    Break,
    Return,
    Eof,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Identifier(String),
    Let,
    If,
    For,
    In,
    Else,
    Function,
    Loop,
    Number(i64),
    String(String),
    True,
    False,
    Null,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Assign,
}

impl TokenKind {
    pub fn get_identifier(&self) -> Result<String, Box<dyn std::error::Error>> {
        if let Self::Identifier(identifier) = self {
            return Ok(identifier.to_string());
        }

        Err(Error::new(
            ErrorKind::UnexpectedToken,
            format!("Expected Token::Identifier, found {self}"),
        ))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, "Token::None"),
            Self::Continue => write!(f, "Token::Continue"),
            Self::Semicolon => write!(f, "Token::Semicolon"),
            Self::Comma => write!(f, "Token::Comma"),
            Self::Colon => write!(f, "Token::Colon"),
            Self::Dot => write!(f, "Token::Dot"),
            Self::Break => write!(f, "Token::Break"),
            Self::Eof => write!(f, "Token::EoF"),
            Self::Return => write!(f, "Token::Return"),
            Self::Let => write!(f, "Token::Let"),
            Self::Assign => write!(f, "Token::Assign"),
            Self::For => write!(f, "Token::For"),
            Self::In => write!(f, "Token::In"),
            Self::Function => write!(f, "Token::Function"),
            Self::Loop => write!(f, "Token::Loop"),
            Self::True => write!(f, "Token::True"),
            Self::False => write!(f, "Token::False"),
            Self::Null => write!(f, "Token::Null"),
            Self::LeftBrace => write!(f, "Token::LeftBrace"),
            Self::RightBrace => write!(f, "Token::RightBrace"),
            Self::LeftParen => write!(f, "Token::LeftParen"),
            Self::RightParen => write!(f, "Token::RightParen"),
            Self::LeftBracket => write!(f, "Token::LeftBracket"),
            Self::RightBracket => write!(f, "Token::RightBracket"),
            Self::Add => write!(f, "Token::Add"),
            Self::Subtract => write!(f, "Token::Subtract"),
            Self::Multiply => write!(f, "Token::Multiply"),
            Self::Divide => write!(f, "Token::Divide"),
            Self::Modulo => write!(f, "Token::Modulo"),
            Self::And => write!(f, "Token::And"),
            Self::Or => write!(f, "Token::Or"),
            Self::Equal => write!(f, "Token::Equal"),
            Self::NotEqual => write!(f, "Token::NotEqual"),
            Self::GreaterThan => write!(f, "Token::GreaterThan"),
            Self::GreaterThanEqual => write!(f, "Token::GreaterThanEqual"),
            Self::LessThan => write!(f, "Token::LessThan"),
            Self::LessThanEqual => write!(f, "Token::LessThanEqual"),
            Self::If => write!(f, "Token::If"),
            Self::Else => write!(f, "Token::Else"),
            Self::Identifier(string) => write!(f, "Token::Identifier({string})"),
            Self::String(string) => write!(f, "Token::String({string})"),
            Self::Number(number) => write!(f, "Token::Number({number})"),
        }
    }
}
