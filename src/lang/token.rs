#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Token {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Divide,
    Times,
    Comment,
    
    // One or two character tokens.
    Bang,
    NotEqual,
    Equal,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Assign,

    // Literals.
    Identifier(String),
    String(String),
    Number(String),
    Char(String),

    // Keywords.
    And,
    Else,
    True,
    False,
    Function,
    For,
    If,
    Null,
    Or,
    Print,
    Return,
    Let,
    Loop,

    // End of file.
    Eof,
    Illegal,
    End,
}

impl Token {
    pub fn is_builtin_function(&self) -> bool {
        match self {
            Token::Print | Token::Function => true,
            _ => false,
        }
    }

    pub fn skip_readchar(&self) -> bool {
        match self {
            Token::Number(_) |
            Token::Identifier(_) => true,
            _ => false,
        }
    }

    pub fn is_bool_literal(&self) -> bool {
        match self {
            Token::True | Token::False => true,
            _ => false,
        }
    }

    pub fn is_statement(&self) -> bool {
        match self {
            Token::Assign | Token::Else | Token::If | Token::Function | Token::Return | Token::Let => true,
            _ => false,
        }
    }

    pub fn is_comparator(&self) -> bool {
        match self {
            Token::Less | Token::Greater | Token::Equal | Token:: NotEqual | Token::GreaterEqual | Token::LessEqual => true,
            _ => false
        }
    }

    pub fn is_operator(&self) -> bool {
        match self {
            Token::Plus | Token::Minus | Token::Divide | Token::Times => true,
            _ => false
        }
    }
}