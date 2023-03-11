use brain_attribute::Brain;

#[derive(Brain)]
enum Token {
    #[token("let")]
    Let,

    #[token("assign")]
    Assign,

    #[regex(["a-zA-z"])]
    Identifier(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let token = Token::Let;
        token.lex().unwrap();
    }
}
