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
        var firstVar = 35;
        print (122 - 2) * 10 + 5;
        print 8;
        print firstVar;
        ",
    );
    // let tokens = lexer::scan("!1");
    // let tokens = lexer::scan("1-6/3-1");
    let mut parser = Parser::new(&tokens);
    let mut interpreter = Interpreter::new();

    match parser.parse() {
        Ok(statements) => interpreter.interpret(statements),
        Err(errors) => {
            for error in errors {
                println!("Caught parser errors! {:?}", error);
            }
        }
    }
}
