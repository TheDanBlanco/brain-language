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
    // GreaterEqual,
    Less,
    // LessEqual,
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
    Nil,
    Or,
    Print,
    Return,
    Let,

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
}