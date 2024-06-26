use std::{iter::Peekable, slice::Iter};

use crate::{
    ast::{BinaryOp, Expr, LiteralValue, Stmt, UnaryOp},
    lexer::Token,
};

#[derive(Debug)]
pub enum ParseError {
    GenericError,
    ExpectedLiteral,
    ParenExpected,
    ExpectedSemicolon,
    ExpectedStatement,
    ExpectedIdentifier,
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

/*
* program       -> statement* EOF;
* declaration   -> varDecl | statement;
* varDecl       -> "var" IDENTIFIER ("=" expression)?";";
* statement     -> exprStmt | printStmt;
* exprStmt      -> expression ";";
* printStmt     -> "print" expression ";";
* expression    -> equality;
* equality      -> comparison (("!=" | "==") comparison)*;
* comparison    -> term ((">" | ">=" | "<" | "<=") term)*;
* term          -> factor (("+" | "-") factor)*;
* factor        -> unary (("*" | "/") unary)*;
* unary         -> ("!" | "-") unary | primary;
* primary       -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER;
*/

const EQUALITY_TOKENS: &[Token] = &[Token::EqualEqual, Token::BangEqual];
const COMPARISON_TOKENS: &[Token] = &[
    Token::Greater,
    Token::GreaterEqual,
    Token::Less,
    Token::LessEqual,
];
const TERM_TOKENS: &[Token] = &[Token::Plus, Token::Minus];
const FACTOR_TOKENS: &[Token] = &[Token::Star, Token::Slash];
const UNARY_TOKENS: &[Token] = &[Token::Bang, Token::Minus];

type E = Result<Expr, ParseError>;
type S = Result<Stmt, ParseError>;

impl<'a> Parser<'a> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser {
            tokens: tokens.iter().peekable(),
        }
    }

    fn syncronize(&mut self) {
        while let Some(&token) = self.tokens.peek() {
            match token {
                Token::Class
                | Token::Else
                | Token::Fun
                | Token::For
                | Token::If
                | Token::Print
                | Token::Var
                | Token::While => {
                    return;
                }
                Token::Semicolon => {
                    let _ = self.consume();
                    return;
                }
                _ => {
                    let _ = self.consume();
                }
            }
        }
    }

    fn consume(&mut self) -> Result<&Token, ParseError> {
        match self.tokens.next() {
            Some(token) => Ok(token),
            None => Err(ParseError::GenericError),
        }
    }

    fn consume_or_err(&mut self, token: Token, err: ParseError) -> Result<&Token, ParseError> {
        match self.tokens.next() {
            Some(t) if t.eq(&token) => Ok(t),
            _ => Err(err),
        }
    }

    fn next_if_one_of(&mut self, tokens: &[Token]) -> Option<&Token> {
        match self.tokens.next_if(|&t| tokens.contains(t)) {
            Some(token) => Some(token),
            None => None,
        }
    }

    fn next_if_eq(&mut self, token: Token) -> Option<&Token> {
        match self.tokens.next_if(|&t| t == &token) {
            Some(token) => Some(token),
            None => None,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut statements: Vec<Stmt> = vec![];
        let mut errors: Vec<ParseError> = vec![];

        while self.tokens.peek().is_some() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(error) => {
                    errors.push(error);
                    self.syncronize();
                }
            }
        }

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(statements)
    }

    fn declaration(&mut self) -> S {
        match self.next_if_one_of(&[Token::Var]) {
            Some(Token::Var) => self.var_decl(),
            Some(_) | None => self.statement(),
        }
    }

    fn var_decl(&mut self) -> S {
        match self.tokens.next() {
            Some(Token::Identifier(name)) => {
                let mut initializer = None;

                if self.next_if_eq(Token::Equal).is_some() {
                    initializer = Some(Box::new(self.expression()?));
                    self.consume_or_err(Token::Semicolon, ParseError::ExpectedSemicolon)?;
                }

                Ok(Stmt::VarDecl {
                    name: name.clone(),
                    initializer,
                })
            }
            Some(_) | None => Err(ParseError::ExpectedIdentifier),
        }
    }

    fn statement(&mut self) -> S {
        self.print_stmt()
    }

    fn print_stmt(&mut self) -> S {
        match self.next_if_eq(Token::Print) {
            Some(_) => {
                let expr = self.expression()?;
                self.consume_or_err(Token::Semicolon, ParseError::ExpectedSemicolon)?;

                Ok(Stmt::Print(expr))
            }
            None => Err(ParseError::ExpectedStatement),
        }
    }

    fn expression(&mut self) -> E {
        self.equality()
    }

    fn equality(&mut self) -> E {
        let mut expr = self.comparison()?;
        while let Some(token) = self.next_if_one_of(EQUALITY_TOKENS) {
            let op = BinaryOp::try_from(token).unwrap();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }
    fn comparison(&mut self) -> E {
        let mut expr = self.term()?;
        while let Some(token) = self.next_if_one_of(COMPARISON_TOKENS) {
            let op = BinaryOp::try_from(token).unwrap();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn term(&mut self) -> E {
        let mut expr = self.factor()?;
        while let Some(token) = self.next_if_one_of(TERM_TOKENS) {
            let op = BinaryOp::try_from(token).unwrap();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn factor(&mut self) -> E {
        let mut expr = self.unary()?;
        while let Some(token) = self.next_if_one_of(FACTOR_TOKENS) {
            let op = BinaryOp::try_from(token).unwrap();
            let right = self.unary()?;

            expr = Expr::Binary {
                left: Box::new(expr),
                op,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn unary(&mut self) -> E {
        if let Some(token) = self.next_if_one_of(UNARY_TOKENS) {
            let op = UnaryOp::try_from(token).unwrap();
            let right = self.unary()?;

            return Ok(Expr::Unary {
                op,
                right: Box::new(right),
            });
        }

        self.primary()
    }

    fn primary(&mut self) -> E {
        let token = self.consume()?;

        match token {
            Token::Number(number) => Ok(Expr::Literal(LiteralValue::Number(*number))),
            Token::String(s) => Ok(Expr::Literal(LiteralValue::String(s.clone()))),
            Token::True => Ok(Expr::Literal(LiteralValue::Bool(true))),
            Token::False => Ok(Expr::Literal(LiteralValue::Bool(false))),
            Token::Nil => Ok(Expr::Literal(LiteralValue::Nil)),
            Token::LeftParen => {
                let expr = Expr::Grouping(Box::new(self.expression()?));
                self.consume_or_err(Token::RightParen, ParseError::ParenExpected)?;
                Ok(expr)
            }
            Token::Identifier(name) => Ok(Expr::Variable(name.clone())),
            _ => Err(ParseError::ExpectedLiteral),
        }
    }
}
