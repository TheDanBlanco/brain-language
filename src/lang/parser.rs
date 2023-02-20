use std::fmt::{self, Debug};

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
    Collection(Vec<Value>),
    Null,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::String(string) => write!(f, "{string}"),
            Value::Number(number) => write!(f, "{number}"),
            Value::Boolean(bool) => write!(f, "{bool}"),
            Value::Null => write!(f, "null"),
            Value::Function(_, _) => write!(f, "[function]"),
            Value::Collection(collection) => {
                let mut output = String::new();
                for (i, value) in collection.iter().enumerate() {
                    output.push_str(&value.to_string());
                    if i != collection.len() - 1 {
                        output.push_str(", ");
                    }
                }
                write!(f, "[{}]", output)
            }
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Binary(Box<Expression>, Operator, Box<Expression>),
    Unary(Operator, Box<Expression>),
    Literal(Value),
    Collection(Vec<Expression>),
    Identifier(String),
    FunctionCall(String, Vec<Expression>),
    IndexAccess(Box<Expression>),
}

#[allow(dead_code)]
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Assignment(String, Box<Expression>),
    Reassignment(String, Box<Expression>),
    Block(Vec<StatementExpression>),
    Conditional(Expression, Box<Statement>, Box<Option<Statement>>),
    Loop(Box<Statement>),
    FunctionDefinition(String, Vec<String>, Box<Statement>),
    Return(Box<Expression>),
    For(String, Box<Expression>, Box<Statement>),
    Break,
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

    pub fn create_ast(&mut self) -> Vec<StatementExpression> {
        let mut output = Vec::new();

        loop {
            output.push(self.parse());

            if !self.can_peek() {
                break;
            }
        }

        output
    }

    pub fn parse(&mut self) -> StatementExpression {
        if let Some(statement) = self.handle_identifier_statement_case() {
            return statement;
        }

        if self.peek().is_statement() {
            return StatementExpression::Statement(self.parse_statement());
        }

        StatementExpression::Expression(self.parse_expression())
    }

    fn parse_statement(&mut self) -> Statement {
        match self.peek() {
            Token::Let => self.parse_new_assignment(),
            Token::Function => self.parse_function_definition(),
            Token::If => self.parse_conditional(),
            Token::Identifier(identifier) => self.parse_reassignment(identifier),
            Token::Loop => self.parse_loop(),
            Token::For => self.parse_for(),
            Token::LeftBrace => self.parse_block(),
            Token::Break => self.parse_break(),
            Token::Return => self.parse_return(),
            _ => todo!(),
        }
    }

    fn parse_new_assignment(&mut self) -> Statement {
        self.expect(Token::Let);
        let token = self.next();
        let identifier = self.get_value_from_identifier_token(token);
        self.expect(Token::Assign);
        let statement = Statement::Assignment(identifier, Box::new(self.parse_expression()));
        self.check_and_skip(Token::Semicolon);

        statement
    }

    // can these be combined?
    fn parse_reassignment(&mut self, identifier: String) -> Statement {
        self.skip();
        self.expect(Token::Assign);
        let statement = Statement::Reassignment(identifier, Box::new(self.parse_expression()));
        self.check_and_skip(Token::Semicolon);

        statement
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
        let block = self.parse_block();
        let statement = Statement::FunctionDefinition(identifier, parameters, Box::new(block));

        self.check_and_skip(Token::Semicolon);
        return statement;
    }

    fn parse_block(&mut self) -> Statement {
        let mut body = vec![];
        self.expect(Token::LeftBrace);
        loop {
            let statement_expression = self.parse();
            body.push(statement_expression);

            if self.peek() == Token::RightBrace {
                break;
            }
        }
        self.expect(Token::RightBrace);
        Statement::Block(body)
    }

    fn handle_identifier_statement_case(&mut self) -> Option<StatementExpression> {
        if let Some((next, following)) = self.double_peek() {
            return match (next, following) {
                (Token::Identifier(_), Token::Assign) => {
                    Some(StatementExpression::Statement(self.parse_statement()))
                }
                _ => None,
            };
        }

        None
    }

    fn parse_conditional(&mut self) -> Statement {
        self.expect(Token::If);
        let condition = self.parse_expression();
        let consequence = self.parse_block();
        let alternative = if self.check(Token::Else) {
            self.next();
            Some(self.parse_block())
        } else {
            None
        };
        Statement::Conditional(condition, Box::new(consequence), Box::new(alternative))
    }

    fn parse_loop(&mut self) -> Statement {
        self.expect(Token::Loop);
        Statement::Loop(Box::new(self.parse_block()))
    }

    fn parse_return(&mut self) -> Statement {
        self.expect(Token::Return);
        Statement::Return(Box::new(self.parse_expression()))
    }

    fn parse_break(&mut self) -> Statement {
        self.expect(Token::Break);
        self.check_and_skip(Token::Semicolon);
        Statement::Break
    }

    fn parse_literal(&mut self, next: Token) -> Expression {
        match next {
            Token::Number(number) => Expression::Literal(Value::Number(number.parse().unwrap())),
            Token::String(string) => Expression::Literal(Value::String(string)),
            Token::True => Expression::Literal(Value::Boolean(true)),
            Token::False => Expression::Literal(Value::Boolean(false)),
            Token::Null => Expression::Literal(Value::Null),
            _ => panic!("{next:#?} is not a literal"),
        }
    }

    fn parse_expression(&mut self) -> Expression {
        let next = self.next();

        let expression = match next {
            Token::Number(_) | Token::String(_) | Token::True | Token::False | Token::Null => {
                let literal = self.parse_literal(next);

                if !self.can_peek() || !self.peek().is_operator() {
                    return literal;
                }

                self.parse_binary_expression(literal)
            }
            Token::Identifier(identifier) => {
                let next = self.peek();

                if next == Token::LeftParen {
                    return self.parse_fn_call(identifier);
                }

                if next.is_operator() {
                    let initial = Expression::Identifier(identifier);

                    return self.parse_binary_expression(initial);
                }

                Expression::Identifier(identifier)
            },
            Token::LeftBracket => self.parse_collection(),
            _ => panic!("Unexpected token {:?}", self.peek()),
        };

        self.check_and_skip(Token::Semicolon);

        expression
    }

    fn parse_binary_expression(&mut self, initial: Expression) -> Expression {
        let node = initial.clone();
        loop {
            if !self.can_peek() {
                break;
            }

            if self.peek().is_mathematical() {
                return self.parse_mathematical_operator(initial);
            }

            if self.peek().is_comparator() {
                return self.parse_comparison_operator(initial);
            }

            return self.parse_logical_operator(initial);
        }
        node
    }

    fn parse_collection(&mut self) -> Expression {
        let mut values = Vec::new();
        while self.peek() != Token::RightBracket {
            let expression = self.parse_expression();
            values.push(expression);
            self.check_and_skip(Token::Comma);
        }
        self.expect(Token::RightBracket);
        self.check_and_skip(Token::Semicolon);
        Expression::Collection(values)
    }

    fn parse_mathematical_operator(&mut self, initial: Expression) -> Expression {
        let op = match self.peek() {
            Token::Plus => MathematicalOperator::Plus,
            Token::Minus => MathematicalOperator::Minus,
            Token::Times => MathematicalOperator::Times,
            Token::Divide => MathematicalOperator::Divide,
            _ => panic!("node is not an operator"),
        };

        self.skip();
        Expression::Binary(
            Box::new(initial),
            Operator::MathematicalOperator(op),
            Box::new(self.parse_expression()),
        )
    }

    fn parse_comparison_operator(&mut self, initial: Expression) -> Expression {
        let comparator = match self.peek() {
            Token::Less => ComparisonOperator::LessThan,
            Token::LessEqual => ComparisonOperator::LessThanEqual,
            Token::Equal => ComparisonOperator::Equal,
            Token::NotEqual => ComparisonOperator::NotEqual,
            Token::Greater => ComparisonOperator::GreaterThan,
            Token::GreaterEqual => ComparisonOperator::GreatherThanEqual,
            _ => panic!("node is not a comparator"),
        };

        self.skip();
        Expression::Binary(
            Box::new(initial),
            Operator::ComparisonOperator(comparator),
            Box::new(self.parse_expression()),
        )
    }

    fn parse_logical_operator(&mut self, initial: Expression) -> Expression {
        let logical = match self.peek() {
            Token::And => LogicalOperator::And,
            Token::Or => LogicalOperator::Or,
            _ => panic!("node is not a comparator"),
        };

        self.skip();
        Expression::Binary(
            Box::new(initial),
            Operator::LogicalOperator(logical),
            Box::new(self.parse_expression()),
        )
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

        self.check_and_skip(Token::Semicolon);

        Expression::FunctionCall(identifier, args)
    }

    fn expect(&mut self, token_type: Token) {
        if !self.can_peek() {
            panic!("expected {token_type:?} but found nothing");
        }

        if self.peek() == token_type {
            self.skip();
            return;
        }

        panic!("expected {:?} but found {:?}", token_type, self.peek());
    }

    fn get_value_from_identifier_token(&self, token: Token) -> String {
        if let Token::Identifier(ident) = token {
            return ident;
        }

        panic!("expected identifier but found {token:?}");
    }

    fn can_peek(&self) -> bool {
        self.pos < self.tokens.len()
    }

    fn can_double_peek(&self) -> bool {
        self.pos + 1 < self.tokens.len()
    }

    fn peek(&self) -> Token {
        if !self.can_peek() {
            panic!(
                "choked at {:?} at position {}",
                self.tokens[self.pos].clone(),
                self.pos
            );
        }

        self.tokens[self.pos].clone()
    }

    fn check(&self, token: Token) -> bool {
        if !self.can_peek() {
            return false;
        }

        self.peek() == token
    }

    fn check_and_skip(&mut self, token: Token) {
        if self.check(token) {
            self.skip();
        }
    }

    fn double_peek(&self) -> Option<(Token, Token)> {
        if !self.can_double_peek() {
            return None;
        }
        Some((self.peek(), self.tokens[self.pos + 1].clone()))
    }

    fn next(&mut self) -> Token {
        if !self.can_peek() {
            panic!(
                "choked at {:?} at position {}",
                self.tokens[self.pos].clone(),
                self.pos
            );
        }

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

    fn parse_for(&mut self) -> Statement {
        self.expect(Token::For);
        let next = self.next();
        let identifier = self.get_value_from_identifier_token(next);
        self.expect(Token::In);
        let collection = self.parse_expression();
        let block = self.parse_block();
        Statement::For(identifier, Box::new(collection), Box::new(block))
    }

}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        let tokens = vec![Token::Number("1".to_string())];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Literal(Value::Number(1.0)))
        );
    }

    #[test]
    fn parse_assignment() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Number("1".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Assignment(
                "x".to_string(),
                Box::new(Expression::Literal(Value::Number(1.0)))
            ))
        );
    }

    #[test]
    fn parse_reassignment() {
        let tokens = vec![
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Number("1".to_string()),
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Reassignment(
                "x".to_string(),
                Box::new(Expression::Literal(Value::Number(1.0)))
            ))
        );
    }

    #[test]
    fn parse_collection() {
        let tokens = vec![
            Token::LeftBracket,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightBracket,
            Token::Semicolon
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Collection(
                vec![
                    Expression::Literal(Value::Number(1.0)),
                    Expression::Literal(Value::Number(2.0))
                ]
            ))
        );
    }

    #[test]
    fn parse_nested_collection() {
        let tokens = vec![
            Token::LeftBracket,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::LeftBracket,
            Token::Number("2".to_string()),
            Token::Comma,
            Token::Number("3".to_string()),
            Token::RightBracket,
            Token::RightBracket,
            Token::Semicolon
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Collection(
                vec![
                    Expression::Literal(Value::Number(1.0)),
                    Expression::Collection(
                        vec![
                            Expression::Literal(Value::Number(2.0)),
                            Expression::Literal(Value::Number(3.0))
                        ]
                    )
                ]
            ))
        );
    }

    #[test]
    fn parse_collection_of_collections() {
        let tokens = vec![
            Token::LeftBracket,
            Token::LeftBracket,
            Token::Number("1".to_string()),
            Token::Comma,
            Token::Number("2".to_string()),
            Token::RightBracket,
            Token::Comma,
            Token::LeftBracket,
            Token::Number("3".to_string()),
            Token::Comma,
            Token::Number("4".to_string()),
            Token::RightBracket,
            Token::RightBracket,
            Token::Semicolon
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Collection(
                vec![
                    Expression::Collection(
                        vec![
                            Expression::Literal(Value::Number(1.0)),
                            Expression::Literal(Value::Number(2.0))
                        ]
                    ),
                    Expression::Collection(
                        vec![
                            Expression::Literal(Value::Number(3.0)),
                            Expression::Literal(Value::Number(4.0))
                        ]
                    )
                ]
            ))
        );
    }

    #[test]
    fn print_no_arguments() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::RightParen,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::FunctionCall("print".to_string(), vec![]))
        );
    }

    #[test]
    fn parse_print() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::RightParen,
            Token::Semicolon,
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
    fn parse_print_with_two_arguments() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::Semicolon,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::FunctionCall(
                "print".to_string(),
                vec![
                    Expression::Identifier("x".to_string()),
                    Expression::Identifier("y".to_string()),
                ]
            ))
        );
    }

    #[test]
    fn print_two_identifiers() {
        let tokens = vec![
            Token::Identifier("print".to_string()),
            Token::LeftParen,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::RightParen,
            Token::Semicolon,
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
            ))
        );
    }

    #[test]
    fn add_two_numbers() {
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
            ))
        );
    }

    #[test]
    fn compare_two_numbers() {
        let tokens = vec![
            Token::Number("1".to_string()),
            Token::Greater,
            Token::Number("2".to_string()),
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
                Box::new(Expression::Literal(Value::Number(2.0)))
            ))
        );
    }

    #[test]
    fn logical_and_two_numbers() {
        let tokens = vec![
            Token::Number("1".to_string()),
            Token::And,
            Token::Number("2".to_string()),
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Expression(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::LogicalOperator(LogicalOperator::And),
                Box::new(Expression::Literal(Value::Number(2.0)))
            ))
        );
    }

    #[test]
    fn create_block_one_statementexpression() {
        let tokens = vec![
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];

        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Block(vec![StatementExpression::Statement(
                Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Literal(Value::Number(1.0)))
                    ))
                )
            )]))
        )
    }

    #[test]
    fn create_block_multiple_statementexpressions() {
        let tokens = vec![
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Minus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];

        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Block(vec![
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Literal(Value::Number(1.0)))
                    ))
                )),
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Minus),
                        Box::new(Expression::Literal(Value::Number(1.0)))
                    ))
                ))
            ]))
        )
    }

    #[test]
    fn parse_function_call() {
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
            ))
        );
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
            StatementExpression::Statement(Statement::FunctionDefinition(
                "adder".to_string(),
                vec!["x".to_string(), "y".to_string(),],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".to_string()),),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("y".to_string()),)
                    )))
                )]))
            ))
        );
    }

    #[test]
    fn parse_function_call_with_args() {
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
            ))
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
            StatementExpression::Expression(Expression::FunctionCall(
                "foo".to_string(),
                vec![Expression::FunctionCall(
                    "bar".to_string(),
                    vec![
                        Expression::Literal(Value::Number(1.0)),
                        Expression::Literal(Value::Number(2.0)),
                    ],
                ),],
            ))
        );
    }

    #[test]
    fn multiple_binary_expressions_in_function() {
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
                vec![Expression::Binary(
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
                )],
            ))
        );
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
            ))
        );
    }

    #[test]
    fn test_for_loop() {
        let tokens = vec![
            Token::For,
            Token::Identifier("i".into()),
            Token::In,
            Token::LeftBracket,
            Token::Number("0".into()),
            Token::Comma,
            Token::Number("1".into()),
            Token::Comma,
            Token::Number("2".into()),
            Token::RightBracket,
            Token::LeftBrace,
            Token::Identifier("print".into()),
            Token::LeftParen,
            Token::Identifier("i".into()),
            Token::RightParen,
            Token::Semicolon,
            Token::RightBrace,
        ];

        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::For(
                "i".into(),
                Box::new(Expression::Collection(vec![
                    Expression::Literal(Value::Number(0.0)),
                    Expression::Literal(Value::Number(1.0)),
                    Expression::Literal(Value::Number(2.0)),
                ])),
                Box::new(Statement::Block(
                    vec![
                        StatementExpression::Expression(
                            Expression::FunctionCall(
                                "print".into(),
                                vec![Expression::Identifier("i".into())],
                            )
                        )
                    ]
                ))
            ))
        );
    }

    #[test]
    fn test_if_statement_no_else() {
        let tokens = vec![
            Token::If,
            Token::Identifier("x".into()),
            Token::Greater,
            Token::Number("5".into()),
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Conditional(
                Expression::Binary(
                    Box::new(Expression::Identifier("x".into())),
                    Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
                    Box::new(Expression::Literal(Value::Number(5.0)))
                ),
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Reassignment(
                        "x".into(),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier("x".into())),
                            Operator::MathematicalOperator(MathematicalOperator::Plus),
                            Box::new(Expression::Literal(Value::Number(1.0))),
                        ))
                    )
                )])),
                Box::new(None),
            ),)
        );
    }

    #[test]
    fn test_if_statement_singular_condition() {
        let tokens = vec![
            Token::If,
            Token::Identifier("x".into()),
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Conditional(
                Expression::Identifier("x".into()),
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Reassignment(
                        "x".into(),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier("x".into())),
                            Operator::MathematicalOperator(MathematicalOperator::Plus),
                            Box::new(Expression::Literal(Value::Number(1.0))),
                        ))
                    )
                )])),
                Box::new(None),
            ),)
        );
    }

    #[test]
    fn test_if_statement_with_else() {
        let tokens = vec![
            Token::If,
            Token::Identifier("x".into()),
            Token::Greater,
            Token::Number("5".into()),
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Minus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Conditional(
                Expression::Binary(
                    Box::new(Expression::Identifier("x".into())),
                    Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
                    Box::new(Expression::Literal(Value::Number(5.0)))
                ),
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Reassignment(
                        "x".into(),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier("x".into())),
                            Operator::MathematicalOperator(MathematicalOperator::Plus),
                            Box::new(Expression::Literal(Value::Number(1.0))),
                        ))
                    )
                )])),
                Box::new(Some(Statement::Block(vec![
                    StatementExpression::Statement(Statement::Reassignment(
                        "x".into(),
                        Box::new(Expression::Binary(
                            Box::new(Expression::Identifier("x".into())),
                            Operator::MathematicalOperator(MathematicalOperator::Minus),
                            Box::new(Expression::Literal(Value::Number(1.0))),
                        ))
                    ))
                ])))
            ),)
        );
    }

    #[test]
    fn test_loop_statement() {
        let tokens = vec![
            Token::Loop,
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Literal(Value::Number(1.0))),
                    ))
                ))
            ]))))
        );
    }

    #[test]
    fn test_loop_with_conditional() {
        let tokens = vec![
            Token::Loop,
            Token::LeftBrace,
            Token::If,
            Token::Identifier("x".into()),
            Token::Less,
            Token::Number("10".into()),
            Token::LeftBrace,
            Token::Identifier("x".into()),
            Token::Assign,
            Token::Identifier("x".into()),
            Token::Plus,
            Token::Number("1".into()),
            Token::Semicolon,
            Token::RightBrace,
            Token::Else,
            Token::LeftBrace,
            Token::Break,
            Token::Semicolon,
            Token::RightBrace,
            Token::RightBrace,
        ];
        let mut parser = Parser::new(tokens);
        let node = parser.parse();
        assert_eq!(
            node,
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::LessThan),
                        Box::new(Expression::Literal(Value::Number(10.0)))
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Reassignment(
                            "x".into(),
                            Box::new(Expression::Binary(
                                Box::new(Expression::Identifier("x".into())),
                                Operator::MathematicalOperator(MathematicalOperator::Plus),
                                Box::new(Expression::Literal(Value::Number(1.0))),
                            ))
                        )
                    )])),
                    Box::new(Some(Statement::Block(vec![
                        StatementExpression::Statement(Statement::Break)
                    ])))
                ))
            ]))))
        );
    }
}
