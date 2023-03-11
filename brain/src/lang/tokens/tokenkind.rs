use core::fmt;

use crate::lang::grammar::error::{Error, ErrorKind};

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    None,
    Illegal,
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
            Self::Illegal => write!(f, "Token::Illegal"),
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn get_identifier() {
        let token = TokenKind::Identifier("test".to_string());

        let identifier = token.get_identifier();

        assert!(identifier.is_ok());
        assert_eq!(identifier.unwrap(), "test".to_string())
    }

    #[test]
    fn get_identifier_not_identifier() {
        let token = TokenKind::Let;

        let identifier = token.get_identifier();

        assert!(identifier.is_err());
        assert_eq!(
            identifier.err().unwrap().to_string(),
            "[UnexpectedToken]: Expected Token::Identifier, found Token::Let".to_string(),
        )
    }

    #[test]
    fn display() {
        assert_eq!(format!("{}", TokenKind::None), "Token::None".to_string());
        assert_eq!(
            format!("{}", TokenKind::Illegal),
            "Token::Illegal".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Continue),
            "Token::Continue".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Semicolon),
            "Token::Semicolon".to_string()
        );
        assert_eq!(format!("{}", TokenKind::Comma), "Token::Comma".to_string());
        assert_eq!(format!("{}", TokenKind::Colon), "Token::Colon".to_string());
        assert_eq!(format!("{}", TokenKind::Dot), "Token::Dot".to_string());
        assert_eq!(format!("{}", TokenKind::Break), "Token::Break".to_string());
        assert_eq!(format!("{}", TokenKind::Eof), "Token::EoF".to_string());
        assert_eq!(
            format!("{}", TokenKind::Return),
            "Token::Return".to_string()
        );
        assert_eq!(format!("{}", TokenKind::Let), "Token::Let".to_string());
        assert_eq!(
            format!("{}", TokenKind::Assign),
            "Token::Assign".to_string()
        );
        assert_eq!(format!("{}", TokenKind::For), "Token::For".to_string());
        assert_eq!(format!("{}", TokenKind::In), "Token::In".to_string());
        assert_eq!(
            format!("{}", TokenKind::Function),
            "Token::Function".to_string()
        );
        assert_eq!(format!("{}", TokenKind::Loop), "Token::Loop".to_string());
        assert_eq!(format!("{}", TokenKind::True), "Token::True".to_string());
        assert_eq!(format!("{}", TokenKind::False), "Token::False".to_string());
        assert_eq!(format!("{}", TokenKind::Null), "Token::Null".to_string());
        assert_eq!(
            format!("{}", TokenKind::LeftBrace),
            "Token::LeftBrace".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::RightBrace),
            "Token::RightBrace".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::LeftParen),
            "Token::LeftParen".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::RightParen),
            "Token::RightParen".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::LeftBracket),
            "Token::LeftBracket".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::RightBracket),
            "Token::RightBracket".to_string()
        );
        assert_eq!(format!("{}", TokenKind::Add), "Token::Add".to_string());
        assert_eq!(
            format!("{}", TokenKind::Subtract),
            "Token::Subtract".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Multiply),
            "Token::Multiply".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Divide),
            "Token::Divide".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Modulo),
            "Token::Modulo".to_string()
        );
        assert_eq!(format!("{}", TokenKind::And), "Token::And".to_string());
        assert_eq!(format!("{}", TokenKind::Or), "Token::Or".to_string());
        assert_eq!(format!("{}", TokenKind::Equal), "Token::Equal".to_string());
        assert_eq!(
            format!("{}", TokenKind::NotEqual),
            "Token::NotEqual".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::GreaterThan),
            "Token::GreaterThan".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::GreaterThanEqual),
            "Token::GreaterThanEqual".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::LessThan),
            "Token::LessThan".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::LessThanEqual),
            "Token::LessThanEqual".to_string()
        );
        assert_eq!(format!("{}", TokenKind::If), "Token::If".to_string());
        assert_eq!(format!("{}", TokenKind::Else), "Token::Else".to_string());
        assert_eq!(
            format!("{}", TokenKind::Identifier("a".to_string())),
            "Token::Identifier(a)".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::String("b".to_string())),
            "Token::String(b)".to_string()
        );
        assert_eq!(
            format!("{}", TokenKind::Number(0)),
            "Token::Number(0)".to_string()
        );
    }
}
