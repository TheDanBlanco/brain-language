use core::fmt;

use crate::lang::grammar::error::{Error, ErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    None,
    Continue,
    Semicolon,
    Comma,
    Break,
    Return,
    Eof,
    Identifier(String),
    Let,
    If,
    For,
    In,
    Else,
    Function,
    Loop,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
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
            Self::Break => write!(f, "Token::Break"),
            Self::Eof => write!(f, "Token::EoF"),
            Self::Return => write!(f, "Token::Return"),
            Self::Let => write!(f, "Token::Let"),
            Self::Assign => write!(f, "Token::Assign"),
            Self::For => write!(f, "Token::For"),
            Self::In => write!(f, "Token::In"),
            Self::Function => write!(f, "Token::Function"),
            Self::Loop => write!(f, "Token::Loop"),
            Self::LeftBrace => write!(f, "Token::LeftBrace"),
            Self::RightBrace => write!(f, "Token::RightBrace"),
            Self::LeftParen => write!(f, "Token::LeftParen"),
            Self::RightParen => write!(f, "Token::RightParen"),
            Self::If => write!(f, "Token::If"),
            Self::Else => write!(f, "Token::Else"),
            Self::Identifier(string) => write!(f, "Token::Identifier({string})"),
        }
    }
}
