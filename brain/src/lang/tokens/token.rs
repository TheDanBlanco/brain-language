use super::tokenkind::TokenKind;

#[derive(Debug, Clone)]
pub struct Token {
    pub start: usize,
    pub end: usize,
    pub token: TokenKind,
}

impl Token {
    pub fn new(start: usize, end: usize, token: TokenKind) -> Self {
        Token { start, end, token }
    }
}
