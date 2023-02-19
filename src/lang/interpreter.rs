use std::collections::HashMap;

use crate::lang::parser::{LogicalOperator, MathematicalOperator};

use super::parser::{
    ComparisonOperator, Expression, Operator, Statement, StatementExpression, Value,
};

// enum for the different types of interpreter return values
#[derive(Debug, PartialEq)]
pub enum InterpreterReturn {
    Value(Value),
    Break,
    None,
}

fn parse_expression(expression: Expression, symbols: &mut HashMap<String, Value>) -> Value {
    match expression {
        Expression::Literal(value) => return value,
        Expression::Binary(lhs, operator, rhs) => {
            let lhs = parse_expression(*lhs, symbols);
            let rhs = parse_expression(*rhs, symbols);

            return match operator {
                Operator::MathematicalOperator(op) => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        MathematicalOperator::Plus => Value::Number(lhs + rhs),
                        MathematicalOperator::Minus => Value::Number(lhs - rhs),
                        MathematicalOperator::Times => Value::Number(lhs * rhs),
                        MathematicalOperator::Divide => Value::Number(lhs / rhs),
                    },
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    (Value::String(lhs), Value::Number(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    (Value::Number(lhs), Value::String(rhs)) => match op {
                        MathematicalOperator::Plus => Value::String(format!("{lhs}{rhs}")),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    },
                    _ => panic!("cannot perform mathematical operators on types other than Number and String")
                },
                Operator::ComparisonOperator(op) => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs <= rhs),
                        ComparisonOperator::NotEqual => Value::Boolean(lhs != rhs),
                    }
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs <= rhs),
                        ComparisonOperator::NotEqual => Value::Boolean(lhs != rhs),
                    }
                    _ => panic!("cannot perform comparison on types other than Number and Number or String and String")
                }
                Operator::LogicalOperator(op) => match (lhs, rhs) {
                    (Value::Boolean(lhs), Value::Boolean(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(lhs && rhs),
                        LogicalOperator::Or => Value::Boolean(lhs || rhs),
                    },
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(!lhs.is_empty() && !rhs.is_empty()),
                        LogicalOperator::Or => Value::Boolean(!lhs.is_empty() || !rhs.is_empty()),
                    }
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(lhs != 0.0 && rhs != 0.0),
                        LogicalOperator::Or => Value::Boolean(lhs != 0.0 || rhs != 0.0),
                    }
                    _ => panic!("cannot perform logical operator on type Function or Null")
                }
            };
        }
        Expression::FunctionCall(identifier, incoming_arguments) => {
            if identifier == "print" {
                let mut print_statement = String::new();
                for arg in incoming_arguments {
                    let val = parse_expression(arg, symbols);
                    print_statement = format!("{print_statement}{val} ");
                }
                println!("{:#?}", print_statement.trim_end());

                return Value::Null;
            }

            if let Some(function) = symbols.get(&identifier) {
                if let Value::Function(defined_arguments, definition) = function {
                    let mut local_symbols = symbols.clone();
                    for (identifier, value) in defined_arguments.iter().zip(incoming_arguments) {
                        let val = parse_expression(value, &mut local_symbols.clone());
                        local_symbols.insert(identifier.to_string(), val);
                    }
                    if let InterpreterReturn::Value(return_value) =
                        parse_block(definition.to_owned(), &mut local_symbols)
                    {
                        return return_value;
                    }

                    return Value::Null;
                }

                panic!("expected function type for identifier {identifier}");
            }

            panic!("function {identifier} was never defined");
        }
        Expression::Identifier(identifier) => {
            if let Some(val) = symbols.get(&identifier) {
                return Value::to_owned(val);
            }

            panic!("could not find identifier {identifier}");
        }
        _ => println!("couldn't parse expression"),
    }

    Value::Null
}

fn parse_statement(
    statement: Statement,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    match statement {
        Statement::Assignment(identifier, expression) => {
            if symbols.get(&identifier).is_some() {
                panic!("identifier {identifier} already exists")
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);

            InterpreterReturn::None
        }
        Statement::Reassignment(identifier, expression) => {
            if symbols.get(&identifier).is_none() {
                panic!("could not find identifier {identifier}")
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);

            InterpreterReturn::None
        }
        Statement::FunctionDefinition(identifier, arguments, definition) => {
            symbols.insert(identifier, Value::Function(arguments, definition));

            InterpreterReturn::None
        }
        Statement::Conditional(condition, consequence, alternative) => {
            let value = parse_expression(condition, symbols);
            if let Value::Boolean(result) = value {
                if result {
                    return parse_block(consequence, symbols);
                }

                if let Some(alt) = *alternative {
                    parse_block(Box::new(alt), symbols);
                }

                return InterpreterReturn::None;
            }

            panic!("expected boolean value for conditional statement");
        }
        Statement::Loop(loop_statement) => loop {
            let out = parse_block(loop_statement.clone(), symbols);
            if let InterpreterReturn::Break = out {
                break out;
            }
        },
        Statement::Block(_) => parse_block(Box::new(statement), symbols),
        Statement::Break => InterpreterReturn::Break,
        Statement::Return(expression) => {
            InterpreterReturn::Value(parse_expression(*expression, symbols))
        }
    }
}

pub fn evaluate(
    statement_expression: StatementExpression,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    match statement_expression {
        StatementExpression::Expression(expression) => {
            parse_expression(expression, symbols);
            InterpreterReturn::None
        }
        StatementExpression::Statement(statement) => parse_statement(statement, symbols),
    }
}

pub fn interpret(statement_expression: Vec<StatementExpression>) {
    let mut symbols = HashMap::new();
    evaluate_statement_expressions(statement_expression, &mut symbols);
}

pub fn evaluate_statement_expressions(
    statement_expressions: Vec<StatementExpression>,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    for statement_expression in statement_expressions {
        let out = evaluate(statement_expression, symbols);

        if InterpreterReturn::None != out {
            return out;
        }
    }

    InterpreterReturn::None
}

pub fn parse_block(
    statement: Box<Statement>,
    symbols: &mut HashMap<String, Value>,
) -> InterpreterReturn {
    if let Statement::Block(block) = *statement {
        return evaluate_statement_expressions(block, symbols);
    }

    panic!("attempted to parse {statement:#?} as a block");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_expression_number_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Number(1.0));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(1.0));
    }

    #[test]
    fn test_parse_expression_string_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::String("hello".to_string()));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("hello".to_string()));
    }

    #[test]
    fn test_parse_expression_boolean_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Boolean(true));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_null_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Null);
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Null);
    }

    #[test]
    fn test_parse_expression_function_literal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Literal(Value::Function(
            vec!["a".to_string()],
            Box::new(Statement::Block(vec![])),
        ));
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(
            value,
            Value::Function(vec!["a".to_string()], Box::new(Statement::Block(vec![])))
        );
    }

    #[test]
    fn test_parse_expression_binary_operator_add_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(3.0));
    }

    #[test]
    fn test_parse_expression_binary_operator_subtract_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::MathematicalOperator(MathematicalOperator::Minus),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(-1.0));
    }

    #[test]
    fn test_parse_expression_binary_operator_multiply_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::MathematicalOperator(MathematicalOperator::Times),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(2.0));
    }

    #[test]
    fn test_parse_expression_binary_operator_divide_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::MathematicalOperator(MathematicalOperator::Divide),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(0.5));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::String("world".to_string()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("helloworld".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_string_and_number() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("hello1".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_number_and_string() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("1hello".to_string()));
    }

    #[test]
    fn test_parse_expression_binary_operator_add_numbers_and_strings_twice() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::MathematicalOperator(MathematicalOperator::Plus),
                Box::new(Expression::Literal(Value::String("hello".to_string()))),
            )),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2.0))),
                Operator::MathematicalOperator(MathematicalOperator::Plus),
                Box::new(Expression::Literal(Value::String("world".to_string()))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::String("1hello2world".to_string()));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parsep_expression_comparison_operator_greater_than_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2.0))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2.0))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_than_or_equal_numbers_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(2.0))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_than_or_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".into()))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("hello".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".into()))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("world".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreaterThan),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_greater_or_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::GreatherThanEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThan),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings_equal() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_less_or_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("zxy".into()))),
            Operator::ComparisonOperator(ComparisonOperator::LessThanEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_comparison_operator_not_equal_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::ComparisonOperator(ComparisonOperator::NotEqual),
            Box::new(Expression::Literal(Value::String("abc".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_and_bools() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_and_bools_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Boolean(false))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_or_bools() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(true))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_or_bools_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Boolean(false))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Boolean(false))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_complex_comparison() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(1.0))),
            )),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2.0))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(2.0))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_complex_comparison_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(1.0))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(1.0))),
            )),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Binary(
                Box::new(Expression::Literal(Value::Number(2.0))),
                Operator::ComparisonOperator(ComparisonOperator::Equal),
                Box::new(Expression::Literal(Value::Number(3.0))),
            )),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("abc".into()))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_or() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::String("zxy".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_strings_or_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("".into()))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::String("".into()))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0.0))),
            Operator::LogicalOperator(LogicalOperator::And),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_or() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0.0))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(true));
    }

    #[test]
    fn test_parse_expression_logical_operator_numbers_or_false() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(0.0))),
            Operator::LogicalOperator(LogicalOperator::Or),
            Box::new(Expression::Literal(Value::Number(0.0))),
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Boolean(false));
    }

    #[test]
    #[should_panic]
    fn test_parse_expression_comparison_operator_equal_numbers_and_strings_unsupported() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::Number(1.0))),
            Operator::ComparisonOperator(ComparisonOperator::Equal),
            Box::new(Expression::Literal(Value::String("1".to_string()))),
        );
        parse_expression(expression, &mut symbols);
    }

    #[test]
    #[should_panic]
    fn test_parse_expression_binary_operator_add_string_and_boolean_unsupported() {
        let mut symbols = HashMap::new();
        let expression = Expression::Binary(
            Box::new(Expression::Literal(Value::String("hello".to_string()))),
            Operator::MathematicalOperator(MathematicalOperator::Plus),
            Box::new(Expression::Literal(Value::Boolean(true))),
        );
        parse_expression(expression, &mut symbols);
    }

    #[test]
    fn test_parse_identifier() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1.0));
        let expression = Expression::Identifier("a".into());
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(1.0));
    }

    #[test]
    #[should_panic(expected = "could not find identifier a")]
    fn test_parse_identifier_not_found() {
        let mut symbols = HashMap::new();
        let expression = Expression::Identifier("a".into());
        parse_expression(expression, &mut symbols);
    }

    #[test]
    fn test_parse_statement_assignment() {
        let mut symbols = HashMap::new();
        let statement = Statement::Assignment(
            "a".into(),
            Box::new(Expression::Literal(Value::Number(1.0))),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1.0));
    }

    #[test]
    #[should_panic(expected = "identifier a already exists")]
    fn test_parse_statement_assignment_identifier_already_exists() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1.0));
        let statement = Statement::Assignment(
            "a".into(),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2.0));
    }

    #[test]
    fn test_parse_statement_reassignment() {
        let mut symbols = HashMap::new();
        symbols.insert("a".into(), Value::Number(1.0));
        let statement = Statement::Reassignment(
            "a".into(),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2.0));
    }

    #[test]
    #[should_panic(expected = "could not find identifier a")]
    fn test_parse_statement_reassignment_identifier_not_found() {
        let mut symbols = HashMap::new();
        let statement = Statement::Reassignment(
            "a".into(),
            Box::new(Expression::Literal(Value::Number(2.0))),
        );
        parse_statement(statement, &mut symbols);
    }

    #[test]
    fn test_parse_statement_function_definintion() {
        let mut symbols = HashMap::new();
        let statement = Statement::FunctionDefinition(
            "adder".into(),
            vec!["a".into(), "b".into()],
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Return(Box::new(Expression::Binary(
                    Box::new(Expression::Identifier("a".into())),
                    Operator::MathematicalOperator(MathematicalOperator::Plus),
                    Box::new(Expression::Identifier("b".into())),
                ))),
            )])),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(
            symbols.get("adder").unwrap(),
            &Value::Function(
                vec!["a".into(), "b".into()],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("a".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("b".into()))
                    )))
                ),]))
            )
        );
    }

    #[test]
    fn test_parse_statement_conditional() {
        let mut symbols = HashMap::new();
        let statement = Statement::Conditional(
            Expression::Literal(Value::Boolean(true)),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Assignment(
                    "a".into(),
                    Box::new(Expression::Literal(Value::Number(1.0))),
                ),
            )])),
            Box::new(None),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1.0));
    }

    #[test]
    fn test_parse_statement_conditional_else() {
        let mut symbols = HashMap::new();
        let statement = Statement::Conditional(
            Expression::Literal(Value::Boolean(false)),
            Box::new(Statement::Block(vec![StatementExpression::Statement(
                Statement::Assignment(
                    "a".into(),
                    Box::new(Expression::Literal(Value::Number(1.0))),
                ),
            )])),
            Box::new(Some(Statement::Block(vec![
                StatementExpression::Statement(Statement::Assignment(
                    "a".into(),
                    Box::new(Expression::Literal(Value::Number(2.0))),
                )),
            ]))),
        );
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(2.0));
    }

    #[test]
    fn test_parse_block() {
        let mut symbols = HashMap::new();
        let block = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "a".into(),
                Box::new(Expression::Literal(Value::Number(1.0))),
            )),
            StatementExpression::Statement(Statement::Assignment(
                "b".into(),
                Box::new(Expression::Literal(Value::Number(2.0))),
            )),
            StatementExpression::Statement(Statement::Assignment(
                "c".into(),
                Box::new(Expression::Literal(Value::Number(3.0))),
            )),
        ]);
        parse_block(Box::new(block), &mut symbols);
        assert_eq!(symbols.get("a").unwrap(), &Value::Number(1.0));
        assert_eq!(symbols.get("b").unwrap(), &Value::Number(2.0));
        assert_eq!(symbols.get("c").unwrap(), &Value::Number(3.0));
    }

    #[test]
    fn test_parse_function_call() {
        let mut symbols = HashMap::new();
        symbols.insert(
            "adder".into(),
            Value::Function(
                vec!["a".into(), "b".into()],
                Box::new(Statement::Block(vec![StatementExpression::Statement(
                    Statement::Return(Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("a".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Identifier("b".into())),
                    ))),
                )])),
            ),
        );
        let expression = Expression::FunctionCall(
            "adder".into(),
            vec![
                Expression::Literal(Value::Number(1.0)),
                Expression::Literal(Value::Number(2.0)),
            ],
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Number(3.0));
    }

    #[test]
    #[should_panic(expected = "function adder was never defined")]
    fn test_parse_function_call_identifier_not_found() {
        let mut symbols = HashMap::new();
        let expression = Expression::FunctionCall(
            "adder".into(),
            vec![
                Expression::Literal(Value::Number(1.0)),
                Expression::Literal(Value::Number(2.0)),
            ],
        );
        let value = parse_expression(expression, &mut symbols);
        assert_eq!(value, Value::Null);
    }

    #[test]
    fn test_parse_statement_loop() {
        let mut symbols = HashMap::new();
        let statement = Statement::Block(vec![
            StatementExpression::Statement(Statement::Assignment(
                "x".into(),
                Box::new(Expression::Literal(Value::Number(0.0))),
            )),
            StatementExpression::Statement(Statement::Loop(Box::new(Statement::Block(vec![
                StatementExpression::Statement(Statement::Reassignment(
                    "x".into(),
                    Box::new(Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::MathematicalOperator(MathematicalOperator::Plus),
                        Box::new(Expression::Literal(Value::Number(1.0))),
                    )),
                )),
                StatementExpression::Statement(Statement::Conditional(
                    Expression::Binary(
                        Box::new(Expression::Identifier("x".into())),
                        Operator::ComparisonOperator(ComparisonOperator::Equal),
                        Box::new(Expression::Literal(Value::Number(10.0))),
                    ),
                    Box::new(Statement::Block(vec![StatementExpression::Statement(
                        Statement::Break,
                    )])),
                    Box::new(None),
                )),
            ])))),
        ]);
        parse_statement(statement, &mut symbols);
        assert_eq!(symbols.get("x").unwrap(), &Value::Number(10.0));
    }
}
