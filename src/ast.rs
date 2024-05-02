use std::fmt::Display;

use crate::lexer::Token;

#[derive(Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Star,
    Slash,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    EqualEqual,
    BangEqual,
}

#[derive(Debug)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Plus => write!(f, "+"),
            BinaryOp::Minus => write!(f, "-"),
            BinaryOp::Star => write!(f, "*"),
            BinaryOp::Slash => write!(f, "/"),
            BinaryOp::Greater => todo!(),
            BinaryOp::GreaterEqual => todo!(),
            BinaryOp::Less => todo!(),
            BinaryOp::LessEqual => todo!(),
            BinaryOp::Equal => todo!(),
            BinaryOp::EqualEqual => todo!(),
            BinaryOp::BangEqual => todo!(),
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Minus => write!(f, "-"),
            UnaryOp::Bang => write!(f, "!"),
        }
    }
}

impl TryFrom<&Token> for BinaryOp {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Plus => BinaryOp::Plus,
            Token::Minus => BinaryOp::Minus,
            Token::Star => BinaryOp::Star,
            Token::Slash => BinaryOp::Slash,
            Token::Greater => BinaryOp::Greater,
            Token::GreaterEqual => BinaryOp::GreaterEqual,
            Token::Less => BinaryOp::Less,
            Token::LessEqual => BinaryOp::LessEqual,
            Token::EqualEqual => BinaryOp::EqualEqual,
            Token::BangEqual => BinaryOp::BangEqual,
            _ => return Err(()),
        })
    }
}

impl TryFrom<&Token> for UnaryOp {
    type Error = ();

    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        Ok(match token {
            Token::Minus => UnaryOp::Minus,
            Token::Bang => UnaryOp::Bang,
            _ => return Err(()),
        })
    }
}

#[derive(Debug)]
pub enum LiteralValue {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralValue::String(s) => write!(f, "{}", s),
            LiteralValue::Number(n) => write!(f, "{}", n),
            LiteralValue::Bool(b) => write!(f, "{}", b),
            LiteralValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        right: Box<Expr>,
    },
    Grouping(Box<Expr>),

    // Not sure about this
    Literal(LiteralValue),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary { left, op, right } => write!(f, "({} {} {})", op, left, right),
            Expr::Unary { op, right } => write!(f, "({} {})", op, right),
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::Literal(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Print(Expr),
}
