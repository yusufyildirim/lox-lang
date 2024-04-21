pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use parser::Parser;

use crate::interpreter::Interpreter;

fn main() {
    println!("Welcome to Lox REPL!");
    let tokens = lexer::scan("(122 - 2) * 10 + 5");
    // let tokens = lexer::scan("!1");
    // let tokens = lexer::scan("1-6/3-1");
    let mut parser = Parser::new(&tokens);
    let result = parser.parse();
    let interpreter = Interpreter::new();

    if let Ok(result) = result {
        println!("Parsed: {}", result);

        let eval = interpreter.evaluate(result);
        match eval {
            Ok(result) => println!("Result: {}", result),
            Err(_) => println!("Runtime Error!",),
        }
    }
}
