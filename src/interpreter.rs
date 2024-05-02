use std::{collections::HashMap, ops::Deref};

use crate::ast::{BinaryOp, Expr, LiteralValue, Stmt, UnaryOp};

#[derive(Debug)]
pub enum RuntimeError {
    Generic,
    WrongType,
}

pub type EvalResult = Result<LiteralValue, RuntimeError>;
pub type ExecResult = Result<(), RuntimeError>;

pub struct Environment {
    vars: HashMap<String, LiteralValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            vars: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: String, value: LiteralValue) -> Option<LiteralValue> {
        self.vars.insert(key, value)
    }

    pub fn get(&self, key: &str) -> Option<&LiteralValue> {
        self.vars.get(key)
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: Environment::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) {
        for stmt in statements {
            match self.execute(stmt) {
                Ok(_res) => {}
                Err(_err) => {
                    println!("Runtime Error!");
                    return;
                }
            }
        }
    }

    fn execute(&mut self, stmt: Stmt) -> ExecResult {
        match stmt {
            Stmt::Expr(expr) => {
                self.evaluate(expr)?;
                Ok(())
            }
            Stmt::Print(expr) => {
                let res = self.evaluate(expr)?;
                println!("{}", res);
                Ok(())
            }
            Stmt::VarDecl { name, initializer } => {
                let mut value = LiteralValue::Nil;

                if let Some(expr) = initializer {
                    value = self.evaluate(expr.deref().clone())?;
                }

                self.environment.define(name, value);
                Ok(())
            }
        }
    }

    fn evaluate(&self, expr: Expr) -> EvalResult {
        match expr {
            Expr::Binary { left, op, right } => self.binary(*left, op, *right),
            Expr::Unary { op, right } => self.unary(op, *right),
            Expr::Grouping(expr) => self.evaluate(*expr),
            Expr::Literal(value) => Ok(value),
            Expr::Variable(name) => match self.environment.get(&name) {
                Some(value) => Ok(value.clone()),
                None => todo!(),
            },
        }
    }

    fn binary(&self, left_expr: Expr, op: BinaryOp, right_expr: Expr) -> EvalResult {
        let left = self.evaluate(left_expr)?;
        let right = self.evaluate(right_expr)?;

        match (op, left, right) {
            (BinaryOp::Plus, LiteralValue::Number(n1), LiteralValue::Number(n2)) => {
                Ok(LiteralValue::Number(n1 + n2))
            }
            (BinaryOp::Plus, LiteralValue::String(s1), LiteralValue::String(s2)) => {
                Ok(LiteralValue::String(format!("{s1}{s2}")))
            }
            (BinaryOp::Minus, LiteralValue::Number(n1), LiteralValue::Number(n2)) => {
                Ok(LiteralValue::Number(n1 - n2))
            }
            (BinaryOp::Star, LiteralValue::Number(n1), LiteralValue::Number(n2)) => {
                Ok(LiteralValue::Number(n1 * n2))
            }
            (BinaryOp::Slash, LiteralValue::Number(n1), LiteralValue::Number(n2)) => {
                Ok(LiteralValue::Number(n1 / n2))
            }
            (BinaryOp::Plus, _, _) => Err(RuntimeError::WrongType),
            (BinaryOp::Minus, _, _) => Err(RuntimeError::WrongType),
            (BinaryOp::Star, _, _) => Err(RuntimeError::WrongType),
            (BinaryOp::Slash, _, _) => Err(RuntimeError::WrongType),
            _ => Err(RuntimeError::Generic),
        }
    }

    fn unary(&self, op: UnaryOp, expr: Expr) -> EvalResult {
        match (op, self.evaluate(expr)?) {
            (UnaryOp::Minus, LiteralValue::Number(num)) => Ok(LiteralValue::Number(-num)),
            (UnaryOp::Bang, LiteralValue::Bool(bool)) => Ok(LiteralValue::Bool(!bool)),
            (UnaryOp::Minus, _) | (UnaryOp::Bang, _) => Err(RuntimeError::WrongType),
        }
    }
}
