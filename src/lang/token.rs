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
    Colon,
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
    In,
    Null,
    Or,
    Print,
    Len,
    Return,
    Let,
    Loop,
    Break,
    Continue,

    // End of file.
    Eof,
    Illegal,
    End,
}

impl Token {
    pub fn is_builtin_function(&self) -> bool {
        match self {
            Token::Print | Token::Function | Token::Len => true,
            _ => false,
        }
    }

    pub fn skip_readchar(&self) -> bool {
        match self {
            Token::Number(_) | Token::Identifier(_) => true,
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
            Token::Assign
            | Token::Else
            | Token::If
            | Token::Function
            | Token::Let
            | Token::Loop
            | Token::For
            | Token::LeftBrace
            | Token::Return
            | Token::Break
            | Token::Continue => true,
            _ => false,
        }
    }

    pub fn is_comparator(&self) -> bool {
        match self {
            Token::Less
            | Token::Greater
            | Token::Equal
            | Token::NotEqual
            | Token::GreaterEqual
            | Token::LessEqual => true,
            _ => false,
        }
    }

    pub fn is_mathematical(&self) -> bool {
        match self {
            Token::Plus | Token::Minus | Token::Divide | Token::Times => true,
            _ => false,
        }
    }

    pub fn is_logical(&self) -> bool {
        match self {
            Token::And | Token::Or => true,
            _ => false,
        }
    }

    pub fn is_operator(&self) -> bool {
        self.is_logical() || self.is_comparator() || self.is_mathematical()
    }

    pub fn is_accessor_indicator(&self) -> bool {
        match self {
            Token::LeftParen | Token::Dot | Token::LeftBracket => true,
            _ => false,
        }
    }
}

// tests for the token module
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_builtin_function() {
        assert!(Token::Print.is_builtin_function());
        assert!(Token::Function.is_builtin_function());
        assert!(Token::Len.is_builtin_function());
        assert!(!Token::Plus.is_builtin_function());
    }

    #[test]
    fn test_skip_readchar() {
        assert!(Token::Number("123".into()).skip_readchar());
        assert!(Token::Identifier("abc".into()).skip_readchar());
        assert!(!Token::Plus.skip_readchar());
    }

    #[test]
    fn test_is_bool_literal() {
        assert!(Token::True.is_bool_literal());
        assert!(Token::False.is_bool_literal());
        assert!(!Token::Plus.is_bool_literal());
    }

    #[test]
    fn test_is_statement() {
        assert!(Token::Assign.is_statement());
        assert!(Token::Else.is_statement());
        assert!(Token::If.is_statement());
        assert!(Token::Function.is_statement());
        assert!(Token::Let.is_statement());
        assert!(Token::Loop.is_statement());
        assert!(Token::LeftBrace.is_statement());
        assert!(Token::Return.is_statement());
        assert!(Token::Break.is_statement());
        assert!(Token::Continue.is_statement());
        assert!(!Token::Plus.is_statement());
    }

    #[test]
    fn test_is_comparator() {
        assert!(Token::Less.is_comparator());
        assert!(Token::Greater.is_comparator());
        assert!(Token::Equal.is_comparator());
        assert!(Token::NotEqual.is_comparator());
        assert!(Token::GreaterEqual.is_comparator());
        assert!(Token::LessEqual.is_comparator());
        assert!(!Token::Plus.is_comparator());
    }

    #[test]
    fn test_is_mathematical() {
        assert!(Token::Plus.is_mathematical());
        assert!(Token::Minus.is_mathematical());
        assert!(Token::Divide.is_mathematical());
        assert!(Token::Times.is_mathematical());
        assert!(!Token::Less.is_mathematical());
    }

    #[test]
    fn test_is_logical() {
        assert!(Token::And.is_logical());
        assert!(Token::Or.is_logical());
        assert!(!Token::Less.is_logical());
    }

    #[test]
    fn test_is_operator() {
        assert!(Token::Plus.is_operator());
        assert!(Token::Minus.is_operator());
        assert!(Token::Divide.is_operator());
        assert!(Token::Times.is_operator());
        assert!(Token::Less.is_operator());
        assert!(Token::Greater.is_operator());
        assert!(Token::Equal.is_operator());
        assert!(Token::NotEqual.is_operator());
        assert!(Token::GreaterEqual.is_operator());
        assert!(Token::LessEqual.is_operator());
        assert!(Token::And.is_operator());
        assert!(Token::Or.is_operator());
        assert!(!Token::LeftBrace.is_operator());
    }

    #[test]
    fn test_is_accessor_indicator() {
        assert!(Token::LeftParen.is_accessor_indicator());
        assert!(Token::Dot.is_accessor_indicator());
        assert!(Token::LeftBracket.is_accessor_indicator());
        assert!(!Token::Plus.is_accessor_indicator());
    }
}
