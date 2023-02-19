use lang::interpreter::interpret;
use std::{
    env,
    fs::File,
    io::{self, BufRead},
    path::Path,
};

use crate::lang::{lexer::Lexer, parser::Parser};

mod lang;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut verbose = false;

    if args.get(1).is_none() {
        println!("Usage: brain FILENAME\n");
        println!("  version: 0.1");

        return;
    }

    if let Some(verbosity) = args.get(2) {
        verbose = verbosity == "--verbose";
    }

    let file = &args[1];

    let mut input = Vec::new();

    if let Ok(lines) = read_lines(file) {
        for line in lines {
            if let Ok(line) = line {
                if !line.is_empty() {
                    input.push(line);
                }
            }
        }

        let mut lexer = Lexer::new(input.concat());
        let tokens = lexer.get_all_tokens();

        if verbose {
            println!("tokens:\n {tokens:#?}\n");
        }

        let mut parser = Parser::new(tokens);
        let ast = parser.create_ast();

        if verbose {
            println!("ast:\n {ast:#?}\n");
        }

        interpret(ast);
    }
}

fn read_lines<P>(filename: P) -> io::Result<io::Lines<io::BufReader<File>>>
where
    P: AsRef<Path>,
{
    let file = File::open(filename)?;
    Ok(io::BufReader::new(file).lines())
}
//
