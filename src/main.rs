pub mod ast;
pub mod interpreter;
pub mod lexer;
pub mod parser;

use parser::Parser;

use crate::interpreter::Interpreter;

fn main() {
    println!("Welcome to Lox REPL!");
    // let tokens = lexer::scan("let mahmut = 15;");
    let tokens = lexer::scan(
        "
        print (122 - 2) * 10 + 5;
        print 8;
        ",
    );
    // let tokens = lexer::scan("!1");
    // let tokens = lexer::scan("1-6/3-1");
    let mut parser = Parser::new(&tokens);
    let interpreter = Interpreter::new();

    match parser.parse() {
        Ok(statements) => interpreter.interpret(statements),
        Err(errors) => {
            for error in errors {
                println!("Caught parser errors! {:?}", error);
            }
        }
    }
}
