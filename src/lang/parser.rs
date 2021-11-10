use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Divide,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Number(f64),
    String(String),
    Operator(Operator),
    Parenthesis(Box<Expression>),
}

#[derive(Debug, PartialEq)]
pub enum Node {
    Expression(Expression),
    Operator(Operator),
    Identifier(String),
    Block(Vec<Node>),
    Parenthesis(Box<Node>),
    Binary(Box<Node>, Operator, Box<Node>),
    Conditional(Box<Node>, Box<Node>, Box<Option<Node>>),
    Assignment(String, Box<Node>),
    Function(String, Vec<Node>),
    Return(Box<Node>),
    FunctionDefinition(String, Vec<Node>, Box<Node>),
}

#[derive(Debug, PartialEq)]
pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, pos: 0 }
    }

    pub fn parse(&mut self) -> Node {
        self.parse_statement()
    }

    fn parse_statement(&mut self) -> Node {
        match self.peek() {
            Token::Let => self.parse_new_assignment(),
            Token::Print => self.parse_print(),
            Token::Identifier(ident) => self.parse_identifier(ident),
            Token::Return => self.parse_return(),
            Token::Function => self.parse_function_definition(),
            Token::If => self.parse_conditional(),
            _ => self.parse_expression(),
        }
    }

    fn parse_new_assignment(&mut self) -> Node {
        self.expect(Token::Let);
        let token = self.next();
        let identifier = self.get_value_from_identifier_token(token);
        self.expect(Token::Assign);
        Node::Assignment(identifier, Box::new(self.parse_expression()))
    }

    fn parse_return(&mut self) -> Node {
        self.expect(Token::Return);
        Node::Return(Box::new(self.parse_expression()))
    }

    fn parse_function_definition(&mut self) -> Node {
        self.expect(Token::Function);
        let token = self.next();
        let identifier = self.get_value_from_identifier_token(token);
        self.expect(Token::LeftParen);
        let mut parameters = Vec::new();
        while self.peek() != Token::RightParen {
            let param = self.next();
            parameters.push(Node::Identifier(self.get_value_from_identifier_token(param)));
            if self.peek() == Token::Comma {
                self.next();
            }
        }
        self.expect(Token::RightParen);
        self.expect(Token::LeftBrace);
        let body = self.parse_statement();
        self.expect(Token::RightBrace);
        Node::FunctionDefinition(identifier, parameters, Box::new(Node::Block(vec![body])))
    }

    fn parse_conditional(&mut self) -> Node {
        self.expect(Token::If);
        self.expect(Token::LeftParen);
        let condition = self.parse_expression();
        self.expect(Token::RightParen);
        let consequence = self.parse_statement();
        let alternative = if self.peek() == Token::Else {
            self.next();
            Some(self.parse_statement())
        } else {
            None
        };
        Node::Conditional(Box::new(condition), Box::new(consequence), Box::new(alternative))
    }

    fn parse_print(&mut self) -> Node {
        self.parse_identifier("print".to_string())
    }

    fn parse_expression(&mut self) -> Node {
        let mut node = self.parse_term();
        loop {
            if !self.can_peek() {
                break;
            }

            let op = match self.peek() {
                Token::Plus => Operator::Plus,
                Token::Minus => Operator::Minus,
                Token::Times => Operator::Times,
                Token::Divide => Operator::Divide,
                _ => break,
            };

            self.next();
            node = Node::Binary(Box::new(node), op, Box::new(self.parse_term()));
        }
        node
    }

    fn parse_term(&mut self) -> Node {
        match self.peek() {
            Token::Number(n) => { 
                self.next();
                Node::Expression(Expression::Number(n.parse().unwrap()))
            }
            Token::String(ident) => {
                self.next();
                Node::Expression(Expression::String(ident))
            }
            Token::Identifier(ident) => {
                self.next();
                Node::Identifier(ident)
            }
            Token::LeftParen => {
                self.next();
                let node = self.parse_expression();
                self.expect(Token::RightParen);
                node
            }

            _ => panic!("Unexpected token {:?}", self.peek()),
        }
    }

    fn parse_identifier(&mut self, identifier: String) -> Node {
        self.skip();
        match self.peek() {
            Token::LeftParen => {
                self.skip();
                let mut args = Vec::new();
                while self.peek() != Token::RightParen {
                    let token = self.peek();
                    let mut next = self.parse_expression();

                    if self.peek() == Token::Comma {
                        self.next();
                    }

                    if self.peek() == Token::LeftParen {
                        let identifier = self.get_value_from_identifier_token(token);
                        self.reverse();
                        next = self.parse_identifier(identifier);
                    }

                    args.push(next)
                }
                self.expect(Token::RightParen);
                Node::Function(identifier, args)
            }
            Token::Assign => {
                self.skip();
                Node::Assignment(identifier, Box::new(self.parse_expression()))
            },
            _ => panic!("{:?} used as expression", self.peek()),
        }
    }

    fn expect(&mut self, token_type: Token) {
        if self.peek() == token_type {
            self.next();
        } else {
            panic!("Expected {:?} but found {:?}", token_type, self.peek());
        }
    }

    fn get_value_from_identifier_token(&self, token: Token) -> String {
        match token {
            Token::Identifier(ident) => ident,
            _ => panic!("Expected identifier but found {:?}", token),
        }
    }

    fn can_peek(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn peek(&self) -> Token {
        self.tokens[self.pos].clone()
    }

    fn next(&mut self) -> Token {
        let token = self.tokens[self.pos].clone();
        self.pos += 1;
        token
    }

    fn skip(&mut self) {
        self.pos += 1;
    }

    fn reverse(&mut self) {
        self.pos -= 1;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_parse_assignment() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Number("1".to_string()),
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Assignment(
                "x".to_string(),
                Box::new(Node::Expression(Expression::Number(1.0)))
            )
        );
    }

    #[test]
    fn test_parser_print_no_arguments() {
        let tokens = vec![
            Token::Print,
            Token::LeftParen,
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function("print".to_string(), vec![])
        );
    }

    #[test]
    fn test_parser_parse_print() {
        let tokens = vec![
            Token::Print,
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "print".to_string(),
                vec![Node::Identifier("x".to_string())]
            )
        );
    }

    #[test]
    fn test_parser_print_two_identifiers() {
        let tokens = vec![
            Token::Print,
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "print".to_string(),
                vec![
                    Node::Binary(
                        Box::new(Node::Identifier("x".to_string())),
                        Operator::Plus,
                        Box::new(Node::Identifier("y".to_string()))
                    )
                ]
            )
        );
    }

    #[test]
    fn test_nested_call_with_print() {
        let tokens = vec![
            Token::Print,
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::LeftParen,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::RightParen,
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "print".to_string(),
                vec![
                    Node::Function(
                        "x".to_string(),
                        vec![Node::Identifier("y".to_string())]
                    )
                ]
            )
        );
    }

    #[test]
    fn test_parser_add_two_numbers() {
        let tokens = vec![
            Token::Number("1".to_string()),
            Token::Plus,
            Token::Number("2".to_string()),
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Binary(
                Box::new(Node::Expression(Expression::Number(1.0))),
                Operator::Plus,
                Box::new(Node::Expression(Expression::Number(2.0)))
            )
        );
    }

    #[test]
    fn test_parser_parse_function_call() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LeftParen,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "foo".to_string(),
                vec![
                    Node::Expression(Expression::Number(1.0)),
                    Node::Expression(Expression::Number(2.0)),
                ],
            )
        );
    }

    #[test]
    fn test_assign_function() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("adder".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::LeftBrace,
            Token::Return,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::RightBrace,
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        
        assert_eq!(
            node,
            Node::FunctionDefinition(
                "adder".to_string(),
                vec![
                    Node::Identifier("x".to_string()),
                    Node::Identifier("y".to_string()),
                ],
                Box::new(
                    Node::Block(
                        vec![
                            Node::Return(
                                Box::new(
                                    Node::Binary(
                                        Box::new(Node::Identifier("x".to_string())),
                                        Operator::Plus,
                                        Box::new(Node::Identifier("y".to_string())),
                                    )
                                )
                            )
                        ]
                    )
                )
            )
        );
    }

    #[test]
    fn test_parser_parse_function_call_with_args() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LeftParen,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "foo".to_string(),
                vec![
                    Node::Expression(Expression::Number(1.0)),
                    Node::Expression(Expression::Number(2.0)),
                ],
            )
        );
    }

    #[test]
    fn test_nested_function_call() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LeftParen,
            Token::Identifier("bar".to_string()),
            Token::LeftParen,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightParen,
            Token::RightParen,
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "foo".to_string(),
                vec![
                    Node::Function(
                        "bar".to_string(),
                        vec![
                            Node::Expression(Expression::Number(1.0)),
                            Node::Expression(Expression::Number(2.0)),
                        ],
                    ),
                ],
            )
        );
    }

    #[test]
    fn test_parser_multiple_binary_expressions_in_function() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LeftParen,
            Token::Number("1".to_string()),
            Token::Plus,
            Token::Number("2".to_string()),
            Token::Plus,
            Token::Number("3".to_string()),
            Token::Minus,
            Token::Number("4".to_string()),
            Token::RightParen,
            Token::Semicolon,
        ];

        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "foo".to_string(),
                vec![
                    Node::Binary(
                        Box::new(Node::Binary(
                            Box::new(Node::Binary(
                                Box::new(Node::Expression(Expression::Number(1.0))),
                                Operator::Plus,
                                Box::new(Node::Expression(Expression::Number(2.0))),
                            )),
                            Operator::Plus,
                            Box::new(Node::Expression(Expression::Number(3.0))),
                        )),
                        Operator::Minus,
                        Box::new(Node::Expression(Expression::Number(4.0))),
                    )
                ],
            )
        );
    }

    #[test]
    fn test_nested_binary_calls() {
        let tokens = vec![
            Token::Identifier("foo".to_string()),
            Token::LeftParen,
            Token::Identifier("bar".to_string()),
            Token::LeftParen,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightParen,
            Token::Plus,
            Token::Identifier("baz".to_string()),
            Token::LeftParen,
            Token::Number("3".to_string()),
            Token::Comma,
            Token::Number("4".to_string()),
            Token::RightParen,
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            Node::Function(
                "foo".to_string(),
                vec![
                    Node::Function(
                        "bar".to_string(),
                        vec![
                            Node::Expression(Expression::Number(1.0)),
                            Node::Expression(Expression::Number(2.0)),
                        ],
                    ),
                    Node::Function(
                        "baz".to_string(),
                        vec![
                            Node::Expression(Expression::Number(3.0)),
                            Node::Expression(Expression::Number(4.0)),
                        ],
                    ),
                ],
            )
        );
    }
}