use std::{fmt::Debug};

use super::token::Token;

#[derive(Debug, PartialEq, Clone)]
pub enum MathematicalOperator {
    Plus,
    Minus,
    Times,
    Divide,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum ComparisonOperator {
    Equal,
    NotEqual,
    GreaterThan,
    GreatherThanEqual,
    LessThan,
    LessThanEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum LogicalOperator {
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    String(String),
    Number(f64),
    Boolean(bool),
    Function(Vec<String>, Box<Statement>),
    Null,
}


#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Literal(Value),
    Identifier(String),
    FunctionCall(String, Vec<Expression>),
    Return(Box<Expression>),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(String, Box<Expression>),
    Block(Vec<StatementExpression>),
    Conditional(Expression, Box<Statement>, Box<Option<Statement>>),
    Loop(Expression, Box<Statement>),
    FunctionDefinition(String, Vec<String>, Box<Statement>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementExpression {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    MathematicalOperator(MathematicalOperator),
    LogicalOperator(LogicalOperator),
    ComparisonOperator(ComparisonOperator),
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

    pub fn parse(&mut self) -> StatementExpression {
        if self.peek().is_statement() {
            return StatementExpression::Statement(self.parse_statement())
        }

        StatementExpression::Expression(self.parse_expression())
    }

    fn parse_statement(&mut self) -> Statement {
        match self.peek() {
            Token::Let => self.parse_new_assignment(),
            Token::Function => self.parse_function_definition(),
            Token::If => self.parse_conditional(),
            // Token::Loop => self.parse_loop(),
            _ => todo!()
        }
    }

    fn parse_new_assignment(&mut self) -> Statement {
        self.expect(Token::Let);
        let token = self.next();
        let identifier = self.get_value_from_identifier_token(token);
        self.expect(Token::Assign);
        Statement::Assignment(identifier, Box::new(self.parse_expression()))
    }

    fn parse_function_definition(&mut self) -> Statement {
        self.expect(Token::Function);
        let token = self.next();
        let identifier = self.get_value_from_identifier_token(token);
        self.expect(Token::LeftParen);
        let mut parameters = Vec::new();
        while self.peek() != Token::RightParen {
            let param = self.next();
            parameters.push(self.get_value_from_identifier_token(param));
            if self.peek() == Token::Comma {
                self.skip();
            }
        }
        self.expect(Token::RightParen);
        self.expect(Token::LeftBrace);
        let body = self.parse();
        self.expect(Token::RightBrace);
        self.expect(Token::Semicolon);
        Statement::FunctionDefinition(
            identifier,
            parameters,
            Box::new(Statement::Block(vec![body])),
        )
    }

    fn parse_conditional(&mut self) -> Statement {
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
        Statement::Conditional(condition, Box::new(consequence), Box::new(alternative))
    }

    fn parse_literal(&mut self) -> Expression {
        match self.peek() {
            Token::Number(number) => {
                self.skip();
                Expression::Literal(Value::Number(number.parse().unwrap()))
            }
            Token::String(string) => {
                self.skip();
                Expression::Literal(Value::String(string))
            }
            Token::True => {
                self.skip();
                Expression::Literal(Value::Boolean(true))
            }
            Token::False => {
                self.skip();
                Expression::Literal(Value::Boolean(false))
            }
            Token::Null => {
                self.skip();
                Expression::Literal(Value::Null)
            }
            _ => panic!("{:#?} is not a literal", self.peek())
        }
    }

    fn parse_expression(&mut self) -> Expression {
        match self.peek() {
            Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
                let literal = self.parse_literal();

                if !self.can_peek() || !self.peek().is_operator() {
                    return literal
                }

                self.parse_binary_expression(literal)
            }
            Token::Identifier(identifier) => {
                self.skip();

                let next = self.peek();

                if next == Token::LeftParen {
                    return self.parse_fn_call(identifier)
                }

                if next.is_operator() {
                    let initial = Expression::Identifier(identifier);

                    return self.parse_binary_expression(initial);
                }

                Expression::Identifier(identifier)
            }
            Token::If => {
                todo!()
            }
            Token::Loop => {
                todo!()
            }
            Token::Return => {
                self.skip();
                return Expression::Return(Box::new(self.parse_expression()))
            }

            _ => panic!("Unexpected token {:?}", self.peek()),
        }
    }

    fn parse_binary_expression(&mut self, initial: Expression) -> Expression {
        let node = initial.clone();
        loop {
            if !self.can_peek() {
                break;
            }

            if self.peek().is_mathematical() {
                return self.parse_mathematical_operator(initial.clone());
            }

            if self.peek().is_comparator() {
                return self.parse_comparison_operator(initial.clone())
            }

            return self.parse_logical_operator(initial.clone());
        }
        node
    }

    fn parse_mathematical_operator(&mut self, initial: Expression) -> Expression {
        let op = match self.peek() {
            Token::Plus => MathematicalOperator::Plus,
            Token::Minus => MathematicalOperator::Minus,
            Token::Times => MathematicalOperator::Times,
            Token::Divide => MathematicalOperator::Divide,
            _ => panic!("node is not an operator")
        };

        self.skip();
        return Expression::Binary(Box::new(initial), Operator::MathematicalOperator(op), Box::new(self.parse_expression()));
    }

    fn parse_comparison_operator(&mut self, initial: Expression) -> Expression {
        let comparator = match self.peek() {
            Token::Less => ComparisonOperator::LessThan,
            Token::LessEqual => ComparisonOperator::LessThanEqual,
            Token::Equal => ComparisonOperator::Equal,
            Token::NotEqual => ComparisonOperator::NotEqual,
            Token::Greater => ComparisonOperator::GreaterThan,
            Token::GreaterEqual => ComparisonOperator::GreatherThanEqual,
            _ => panic!("node is not a comparator")
        };

        self.skip();
        return Expression::Binary(Box::new(initial), Operator::ComparisonOperator(comparator), Box::new(self.parse_expression()));
    }

    fn parse_logical_operator(&mut self, initial: Expression) -> Expression {
        let logical = match self.peek() {
            Token::And => LogicalOperator::And,
            Token::Or => LogicalOperator::Or,
            _ => panic!("node is not a comparator")
        };

        self.skip();
        return Expression::Binary(Box::new(initial), Operator::LogicalOperator(logical), Box::new(self.parse_expression()));
    }

    fn parse_fn_call(&mut self, identifier: String) -> Expression {
        self.expect(Token::LeftParen);
        let mut args = Vec::new();

        while self.peek() != Token::RightParen {
            let token = self.peek();
            let mut next = self.parse_expression();
            let peek = self.peek();

            if peek == Token::Comma {
                self.skip();
            }

            if peek == Token::LeftParen {
                let identifier = self.get_value_from_identifier_token(token);
                next = self.parse_fn_call(identifier);
            }

            if peek.is_operator() {
                next = self.parse_binary_expression(next);
            }

            args.push(next)
        }

        self.expect(Token::RightParen);
        Expression::FunctionCall(identifier, args)
    }

    fn expect(&mut self, token_type: Token) {
        if self.peek() == token_type {
            self.skip();
            return
        }

        panic!("Expected {:?} but found {:?}", token_type, self.peek());
    }

    fn get_value_from_identifier_token(&self, token: Token) -> String {
        if let Token::Identifier(ident) = token {
            return ident;
        }

        panic!("Expected identifier but found {:?}", token);
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

    #[allow(dead_code)]
    fn parse_unary_expression(&self, _literal: Expression) -> Expression {
        todo!()
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
            StatementExpression::Statement(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::Literal(Value::Number(1.0)))
            )
        ));
    }

    #[test]
    fn test_parser_print_no_arguments() {
        let tokens = vec![Token::Identifier("print".to_string()), Token::LeftParen, Token::RightParen];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::FunctionCall(
                "print".to_string(),
                vec![]
            ))
        );
    }

    #[test]
    fn test_parser_parse_print() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::FunctionCall(
                "print".to_string(),
                vec![Expression::Identifier("x".to_string())]
            ))
        );
    }

    #[test]
    fn test_parser_print_two_identifiers() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
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
            StatementExpression::Expression(Expression::FunctionCall(
                "print".to_string(),
                vec![Expression::Binary(
                    Box::new(Expression::Identifier("x".to_string())),
                    Operator::MathematicalOperator(MathematicalOperator::Plus),
                    Box::new(Expression::Identifier("y".to_string()))
                )]
            ))
        );
    }

    #[test]
    fn test_nested_call_with_print() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
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
            StatementExpression::Expression(Expression::FunctionCall(
                "print".to_string(),
                vec![Expression::FunctionCall(
                    "x".to_string(),
                    vec![Expression::Identifier("y".to_string())]
                )]
            )
        ));
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
            StatementExpression::Expression(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::MathematicalOperator(MathematicalOperator::Plus),
                Box::new(Expression::Literal(Value::Number(2.0)))
            )
        ));
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![
                    Expression::Literal(Value::Number(1.0)),
                    Expression::Literal(Value::Number(2.0)),
                ],
            )
        ));
    }

    #[test]
    fn test_assign_function() {
        // fn adder(x, y) { return x + y };
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
            StatementExpression::Statement(
                Statement::FunctionDefinition(
                    "adder".to_string(), 
                    vec![
                        "x".to_string(),
                        "y".to_string(),
                    ],
                    Box::new(
                        Statement::Block(
                            vec![
                                StatementExpression::Expression(
                                    Expression::Return(
                                        Box::new(
                                            Expression::Binary(
                                                Box::new(
                                                    Expression::Identifier("x".to_string()),
                                                ),
                                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                                Box::new(
                                                    Expression::Identifier("y".to_string()),
                                                )
                                            )
                                        )
                                    )
                                )
                            ]
                        )
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![
                    Expression::Literal(Value::Number(1.0)),
                    Expression::Literal(Value::Number(2.0)),
                ],
            )
        ));
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![Expression::FunctionCall(
                    "bar".to_string(),
                    vec![
                        Expression::Literal(Value::Number(1.0)),
                        Expression::Literal(Value::Number(2.0)),
                    ],
                ),],
            )
        ));
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![
                    Expression::Binary(
                        Box::new(Expression::Literal(Value::Number(1.0))),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Literal(Value::Number(2.0))),
                            Operator::MathematicalOperator(MathematicalOperator::Plus),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Literal(Value::Number(3.0))),
                                Operator::MathematicalOperator(MathematicalOperator::Minus),
                                Box::new(Expression::Literal(Value::Number(4.0)))
                            ))
                        ))
                    )
                ],
            )
        ));
    }

    #[test]
    fn test_nested_binary_calls() {
        // foo(bar(1, 2) + baz(3, 4))
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![Expression::Binary(
                    Box::new(Expression::FunctionCall(
                        "bar".to_string(),
                        vec![
                            Expression::Literal(Value::Number(1.0)),
                            Expression::Literal(Value::Number(2.0)),
                        ],
                    )),
                    Operator::MathematicalOperator(MathematicalOperator::Plus),
                    Box::new(Expression::FunctionCall(
                        "baz".to_string(),
                        vec![
                            Expression::Literal(Value::Number(3.0)),
                            Expression::Literal(Value::Number(4.0)),
                        ],
                    )),
                )],
            )
        ));
    }
}
