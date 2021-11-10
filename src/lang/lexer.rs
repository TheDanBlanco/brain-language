use super::token::Token;

pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: None,
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input.chars().nth(self.read_position).unwrap());
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&self) -> Option<char> {
        if self.read_position >= self.input.len() {
            None
        } else {
            Some(self.input.chars().nth(self.read_position).unwrap())
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch {
            if c.is_whitespace() {
                self.read_char();
            } else {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while let Some(c) = self.ch {
            if c.is_alphanumeric() || c == '_' {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while let Some(c) = self.ch {
            if c.is_digit(10) {
                self.read_char();
            } else {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;

        if let Some(c) = self.ch {
            if c == '"' {
                self.read_char();
            }
        }

        while let Some(c) = self.ch {
            if c == '"' {
                break;
            }
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_char_literal(&mut self) -> String {
        let position = self.position + 1;

        if let Some(c) = self.ch {
            if c == '\'' {
                self.read_char();
            }
        }

        while let Some(c) = self.ch {
            if c == '\'' {
                break;
            }
            self.read_char();
        }
        self.input[position..self.position].to_string()
    }

    fn read_token(&mut self) -> Token {
        self.skip_whitespace();
        let tok = match self.ch {
            Some('=') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            }
            Some('+') => Token::Plus,
            Some('-') => Token::Minus,
            Some('!') => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Token::NotEqual
                } else {
                    Token::Bang
                }
            }
            Some('*') => Token::Times,
            Some('/') => Token::Divide,
            Some('.') => Token::Dot,
            Some('<') => Token::Less,
            Some('>') => Token::Greater,
            Some(';') => Token::Semicolon,
            Some(',') => Token::Comma,
            Some('{') => Token::LeftBrace,
            Some('}') => Token::RightBrace,
            Some('(') => Token::LeftParen,
            Some(')') => Token::RightParen,
            Some('[') => Token::LeftBracket,
            Some(']') => Token::RightBracket,
            Some('#') => Token::Comment,
            Some('"') => Token::String(self.read_string()),
            Some('\'') => Token::Char(self.read_char_literal()),
            Some('0'..='9') => Token::Number(self.read_number()),
            Some('_') | Some('a'..='z') | Some('A'..='Z') => {
                let identifier = self.read_identifier();
                match &*identifier {
                    "let" => Token::Let,
                    "and" => Token::And,
                    "else" => Token::Else,
                    "false" => Token::False,
                    "for" => Token::For,
                    "fn" => Token::Function,
                    "if" => Token::If,
                    "nil" => Token::Nil,
                    "or" => Token::Or,
                    "print" => Token::Print,
                    "return" => Token::Return,
                    "true" => Token::True,
                    _ => Token::Identifier(identifier),
                }
            },
            None => Token::Eof,
            _ => Token::Illegal,
        };

        if tok.skip_readchar() || tok.is_builtin_function() || tok.is_bool_literal() {
            return tok;
        }

        self.read_char();

        return tok;
    }

    pub fn next_token(&mut self) -> Token {
        self.read_token()
    }


    pub fn get_all_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            if tok == Token::Eof {
                break;
            }
            tokens.push(tok);
        }
        tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_identifier() {
        let input = "foobar";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_identifier(), "foobar");
    }

    #[test]
    fn test_read_number() {
        let input = "123";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_number(), "123");
    }

    #[test]
    fn test_read_string() {
        let input = r#""foo bar""#;
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_string(), "foo bar");
    }

    #[test]
    fn test_read_char_literal() {
        let input = r#"'f'"#;
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_char_literal(), "f");
    }

    #[test]
    fn test_read_token() {
        let input = "=+(){},;";
        let mut l = Lexer::new(input.to_string());

        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::LeftBrace);
        assert_eq!(l.next_token(), Token::RightBrace);
        assert_eq!(l.next_token(), Token::Comma);
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::Eof);
    }

    #[test]
    fn test_read_identifier_with_underscore() {
        let input = "foobar_";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_identifier(), "foobar_");
    }

    #[test]
    fn test_read_identifier_with_number() {
        let input = "foobar1";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_identifier(), "foobar1");
    }

    #[test]
    fn test_read_identifier_with_number_and_underscore() {
        let input = "foobar1_";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_identifier(), "foobar1_");
    }

    #[test]
    fn test_read_identifier_with_number_and_underscore_and_letter() {
        let input = "foobar1_a";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.read_identifier(), "foobar1_a");
    }

    #[test]
    fn test_print() {
        let input = "print";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Print);
    }

    #[test]
    fn test_print_and_parens() {
        let input = "print(a);";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Print);
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_number_to_identifier() {
        let input = "let a = 1;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_string_to_identifier() {
        let input = "let a = \"foo bar\";";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::String("foo bar".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_char_to_identifier() {
        let input = "let a = 'f';";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Char("f".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_identifier_to_identifier() {
        let input = "let a = b;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Identifier("b".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_string_plus_string_to_identifier() {
        let input = "let a = \"foo\" + \"bar\";";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::String("foo".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::String("bar".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_string_plus_number_to_identifier() {
        let input = "let a = \"foo\" + 1;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::String("foo".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_number_plus_string_to_identifier() {
        let input = "let a = 1 + \"foo\";";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::String("foo".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_number_plus_number_to_identifier() {
        let input = "let a = 1 + 1;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_identifier_with_dot_to_identifier() {
        let input = "let a = b.c;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Identifier("b".to_string()));
        assert_eq!(l.next_token(), Token::Dot);
        assert_eq!(l.next_token(), Token::Identifier("c".to_string()));
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn assign_function_to_identifier() {
        let input = "let a = fn(x) { return x + 1 };";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Function);
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::LeftBrace);
        assert_eq!(l.next_token(), Token::Return);
        assert_eq!(l.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::RightBrace);
        assert_eq!(l.next_token(), Token::Semicolon);
    }    

    #[test]
    fn call_function_with_identifier() {
        let input = "let a = fn(x) { return x + 1 }; a(1);";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::Function);
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::LeftBrace);
        assert_eq!(l.next_token(), Token::Return);
        assert_eq!(l.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(l.next_token(), Token::Plus);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::RightBrace);
        assert_eq!(l.next_token(), Token::Semicolon);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Number("1".to_string()));
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn call_function_inside_function() {
        let input = "some_fn(another_fn(x));";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Identifier("some_fn".to_string()));
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Identifier("another_fn".to_string()));
        assert_eq!(l.next_token(), Token::LeftParen);
        assert_eq!(l.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::RightParen);
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn comment_block() {
        let input = "# this is a comment block";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Comment);
        
    }

    #[test]
    fn true_assignment() {
        let input = "let a = true;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::True);
        assert_eq!(l.next_token(), Token::Semicolon);
    }

    #[test]
    fn false_assignment() {
        let input = "let a = false;";
        let mut l = Lexer::new(input.to_string());
        assert_eq!(l.next_token(), Token::Let);
        assert_eq!(l.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(l.next_token(), Token::Assign);
        assert_eq!(l.next_token(), Token::False);
        assert_eq!(l.next_token(), Token::Semicolon);
    }
}
