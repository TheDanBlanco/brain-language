pub type Span = std::ops::Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub struct Token<T> {
    pub span: Span,
    pub token: T,
    pub data: Option<String>,
}

impl<T> Token<T>
where
    T: Clone + std::fmt::Debug + PartialEq,
{
    pub fn new(span: Span, token: T, data: Option<String>) -> Self {
        Token { span, token, data }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, PartialEq)]
    enum Test {
        A,
        B,
    }

    #[test]
    fn new() {
        let token = Token::new(0..2, Test::A, None);

        assert_eq!(token.span, 0..2);
        assert_eq!(token.token, Test::A);
        assert_eq!(token.data, None);
    }

    #[test]
    fn new_with_data() {
        let token = Token::new(0..2, Test::B, Some("test".to_string()));

        assert_eq!(token.span, 0..2);
        assert_eq!(token.token, Test::B);
        assert_eq!(token.data, Some("test".to_string()));
    }
}
