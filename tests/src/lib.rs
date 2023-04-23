mod attribute;

#[cfg(test)]
mod tests {
    // brain does NOT support unary expressions, bitwise operations.

    use std::collections::BTreeMap;

    use brain_grammar::{
        grammar::{token::BrainToken, value::Value},
        Program,
    };
    use brain_token::Brain;

    fn run(source: String) -> Result<Program, Box<dyn std::error::Error>> {
        let stream = BrainToken::lex(source);
        let mut program = Program::new(stream.unwrap(), true);
        program.run()?;
        Ok(program)
    }

    #[test]
    fn test_with_verbose() {
        let source = "let first = 0".to_string();

        let stream = BrainToken::lex(source);
        let mut program = Program::new(stream.unwrap(), true);

        let _ = program.run();

        assert_eq!(
            program.context.symbols.get("first"),
            Some(&Value::new_number(0))
        );
    }

    #[test]
    fn assignment() {
        let source = "let first = 0".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::new_number(0))
        );
    }

    #[test]
    fn assignment_with_binary_expression_mathematical_numbers() {
        let source = "let first = 0 + 1".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn assignment_with_binary_expression_mathematical_strings() {
        let source = r#"let first = "a" + "b""#.to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::new_string("ab".to_string()))
        );
    }

    #[test]
    fn assignment_with_binary_expression_comparison_numbers() {
        let source = "let first = 0 == 1".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("first"),
            Some(&Value::new_boolean(false))
        );
    }

    #[test]
    fn function_definition_binary_return() {
        let source = "fn add(a, b) { return a + b }; let x = add(1, 2)".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(3))
        );
    }

    #[test]
    fn map() {
        let source = "let x = { a: 1, b: 2 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_map(BTreeMap::from_iter(vec![
                (Value::new_string("a".to_string()), Value::new_number(1)),
                (Value::new_string("b".to_string()), Value::new_number(2))
            ])))
        );
    }

    #[test]
    fn map_with_number_key() {
        let source = r#"let x = { 1: "a", 2: "b" }"#.to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_map(BTreeMap::from_iter(vec![
                (Value::new_number(1), Value::new_string("a".to_string())),
                (Value::new_number(2), Value::new_string("b".to_string()))
            ])))
        );
    }

    #[test]
    fn map_with_bool_key() {
        let source = r#"let x = { true: "a" }"#.to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_map(BTreeMap::from_iter(vec![(
                Value::new_boolean(true),
                Value::new_string("a".to_string())
            ),])))
        );
    }

    #[test]
    fn map_with_null_key() {
        let source = r#"let x = { null: "a" }"#.to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_map(BTreeMap::from_iter(vec![(
                Value::new_null(),
                Value::new_string("a".to_string())
            ),])))
        );
    }

    #[test]
    fn collection() {
        let source = "let x = [1, 2, 3]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_collection(vec![
                Value::new_number(1),
                Value::new_number(2),
                Value::new_number(3)
            ]))
        );
    }

    #[test]
    fn collection_of_collections() {
        let source = "let x = [[1, 2], [3, 4]]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_collection(vec![
                Value::new_collection(vec![Value::new_number(1), Value::new_number(2)]),
                Value::new_collection(vec![Value::new_number(3), Value::new_number(4)])
            ]))
        );
    }

    #[test]
    fn collection_with_map() {
        let source = "let x = [{ a: 1, b: 2 }, { a: 3, b: 4 }]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_collection(vec![
                Value::new_map(BTreeMap::from_iter(vec![
                    (Value::new_string("a".to_string()), Value::new_number(1)),
                    (Value::new_string("b".to_string()), Value::new_number(2))
                ])),
                Value::new_map(BTreeMap::from_iter(vec![
                    (Value::new_string("a".to_string()), Value::new_number(3)),
                    (Value::new_string("b".to_string()), Value::new_number(4))
                ]))
            ]))
        );
    }

    #[test]
    fn collection_with_index_accessor() {
        let source = "let x = [1, 2, 3]; let y = x[0]".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn map_with_field_accessor() {
        let source = "let x = { a: 1, b: 2 }; let y = x.a".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn map_with_field_accessor_with_index_accessor() {
        let source = "let x = [{ a: 1, b: 2 }, { a: 3, b: 4 }]; let y = x[0].a".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("y"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn r#loop() {
        let source = "let x = 0; loop { if x < 10 { x = x + 1; continue; } break; }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(10))
        );
    }

    #[test]
    fn r#if() {
        let source = "let x = 0; if x == 0 { x = 1 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn r#if_else() {
        let source = "let x = 0; if x == 1 { x = 1 } else { x = 2 }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(2))
        );
    }

    #[test]
    fn r#for() {
        let source = "let x = 0; for i in [1, 2, 3] { x = x + i }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(6))
        );
    }

    #[test]
    fn r#for_with_break() {
        let source = "let x = 0; for i in [1, 2, 3] { x = x + i; if x == 3 { break } }".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(3))
        );
    }

    #[test]
    fn r#bitwise_and() {
        let source = "let x = 5 & 1;".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(1))
        );
    }

    #[test]
    fn r#bitwise_or() {
        let source = "let x = 5 | 1;".to_string();

        let result = run(source);

        assert_eq!(
            result.unwrap().context.symbols.get("x"),
            Some(&Value::new_number(5))
        );
    }

    #[test]
    fn print() {
        let source = "print(1)".to_string();

        let result = run(source);

        assert!(result.is_ok())
    }

    #[test]

    fn r#enum() {
        let source = "enum Test { One, Two }; let variant = Test::One; if variant == Test::One { let conditional = true }".to_string();
        let result = run(source);

        let context = &result.unwrap().context;

        assert_eq!(
            context.symbols.get("Test").unwrap(),
            &Value::EnumDefinition(
                "Test".to_string(),
                vec!["One".to_string(), "Two".to_string()]
            )
        );

        assert_eq!(
            context.symbols.get("variant").unwrap(),
            &Value::EnumVariant("Test".to_string(), "One".to_string())
        );

        assert_eq!(
            context.symbols.get("conditional").unwrap(),
            &Value::Boolean(true)
        );
    }
}
