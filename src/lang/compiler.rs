use std::{collections::HashMap};

use crate::lang::parser::{MathematicalOperator, LogicalOperator};

use super::parser::{Operator, Value, Expression, Statement, StatementExpression, ComparisonOperator};

#[derive(Debug, PartialEq)]
pub enum BlockReturn {
    Break,
    ReturnValue(Value),
    None,
}

fn parse_expression(expression: Expression, symbols: &mut HashMap<String, Value>) -> Value {
    match expression {
        Expression::Literal(value) => {
            return match value {
                Value::Number(num) => Value::Number(num),
                Value::String(str) => Value::String(str),
                _ => Value::Null,
            }
        }
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
                        MathematicalOperator::Plus => Value::String(format!("{}{}", lhs, rhs)),
                        _ => panic!("cannot perform mathematical operators other than adding on two strings"),
                    }
                    _ => panic!("cannot perform mathematical operators on types other than Number and String")
                },
                Operator::ComparisonOperator(op) => match (lhs, rhs) {
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::NotEqual => Value::Boolean(lhs != rhs),
                    }
                    (Value::String(lhs), Value::String(rhs)) => match op {
                        ComparisonOperator::Equal => Value::Boolean(lhs == rhs),
                        ComparisonOperator::GreaterThan => Value::Boolean(lhs > rhs),
                        ComparisonOperator::GreatherThanEqual => Value::Boolean(lhs >= rhs),
                        ComparisonOperator::LessThan => Value::Boolean(lhs < rhs),
                        ComparisonOperator::LessThanEqual => Value::Boolean(lhs >= rhs),
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
                        LogicalOperator::And => Value::Boolean(lhs.len() > 0 && rhs.len() > 0),
                        LogicalOperator::Or => Value::Boolean(lhs.len() > 0 || rhs.len() > 0),
                    }
                    (Value::Number(lhs), Value::Number(rhs)) => match op {
                        LogicalOperator::And => Value::Boolean(lhs != 0.0 && rhs != 0.0),
                        LogicalOperator::Or => Value::Boolean(lhs != 0.0 || rhs != 0.0),
                    }
                    _ => panic!("cannot perform logical operator on type Function or Null")
                }
            }
        }
        Expression::FunctionCall(identifier, incoming_arguments) => {
            if identifier == "print" {
                let arg = incoming_arguments[0].clone();
                let val = parse_expression(arg, symbols);
                println!("{:#?}", val);

                return Value::Null;
            }

            if let Some(function) = symbols.get(&identifier) {
                if let Value::Function(defined_arguments, definition) = function {
                    let mut local_symbols = symbols.clone();
                    for (identifier, value) in defined_arguments.iter().zip(incoming_arguments) {
                        let val = parse_expression(value, &mut local_symbols.clone());
                        local_symbols.insert(identifier.to_string(), val);
                    }
                    if let BlockReturn::ReturnValue(return_value) = parse_block(definition.to_owned(), &mut local_symbols) {
                        return return_value
                    }
                    
                }

                panic!("expected function type for identifier {}", identifier);
            }

            panic!("function was never defined");
        }
        Expression::Identifier(identifier) => {
            if let Some(val) = symbols.get(&identifier) {
                return Value::to_owned(val);
            }

            panic!("could not find identifier in symbols table");
        }
        Expression::Return(expression) => return parse_expression(*expression, symbols),
        _ => println!("couldn't parse expression")
    }

    Value::Null
}

fn parse_statement(statement: Statement, symbols: &mut HashMap<String, Value>) {
    match statement {
        Statement::Assignment(identifier, expression) => {
            if let Some(_) = symbols.get(&identifier) {
                panic!("value already assigned for identifier {}", identifier)
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);
        }
        Statement::Reassignment(identifier, expression) => {
            if let None = symbols.get(&identifier) {
                panic!("value not previously assigned for identifier {}", identifier)
            }

            let value = parse_expression(*expression, symbols);
            symbols.insert(identifier, value);
        }
        Statement::FunctionDefinition(identifier, arguments, definition) => {
            symbols.insert(identifier, Value::Function(arguments, definition));
        }
        Statement::Conditional(condition, consequence, alternative) => {
            let value = parse_expression(condition, symbols);
            if let Value::Boolean(result) = value {
                if result {
                    parse_block(consequence, symbols);
                    return;
                }

                if let Some(alt) = *alternative {
                    parse_block(Box::new(alt), symbols);
                }
            }
        },
        Statement::Loop(_loop_statement) => {
            unimplemented!()
        }
        Statement::Block(_) => {
            parse_block(Box::new(statement), symbols);
        }
    }
}

pub fn evaluate(statement_expression: StatementExpression, symbols: &mut HashMap<String, Value>) {
    match statement_expression {
        StatementExpression::Expression(expression) => {
            parse_expression(expression, symbols);
        },
        StatementExpression::Statement(statement) => {
            parse_statement(statement, symbols);
        }
    }
}

pub fn run(statement_expression: Vec<StatementExpression>, symbols: &mut HashMap<String, Value>) {
    for statement_expression in statement_expression {
        evaluate(statement_expression, symbols);
    }
}


pub fn parse_block(statement: Box<Statement>, symbols: &mut HashMap<String, Value>) -> BlockReturn {
    let mut out = BlockReturn::None;

    if let Statement::Block(block) = *statement {
        for statement_expression in block {
            if let StatementExpression::Expression(expression) = statement_expression.clone() {
                if let Expression::Return(return_expression) = expression.clone() {
                    out = BlockReturn::ReturnValue(parse_expression(*return_expression, symbols));
                    break;
                }

                if let Expression::Break = expression.clone() {
                    out = BlockReturn::Break;
                    break;
                }
            }

            evaluate(statement_expression, symbols)
        }
    } else {
        panic!("attempted to parse {:#?} as a block", statement); 
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parser_parse_assignment() {
        let mut symbols: HashMap<String, Value> = HashMap::new();

        let ast = StatementExpression::Statement(Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::Literal(Value::Number(1.0)))
        ));

        evaluate(ast, &mut symbols);

        let x = symbols.get("x").unwrap();

        assert_eq!(
            x,
            &Value::Number(1.0),
        )   
    }

    #[test]
    fn test_parser_parse_function_definition() {
        let mut symbols: HashMap<String, Value> = HashMap::new();

        let ast = StatementExpression::Statement(Statement::Assignment(
            "x".to_string(),
            Box::new(Expression::Literal(Value::Number(1.0)))
        ));

        evaluate(ast, &mut symbols);

        let x = symbols.get("x").unwrap();

        assert_eq!(
            x,
            &Value::Number(1.0),
        )   
    }
}