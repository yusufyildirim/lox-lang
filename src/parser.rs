use std::{iter::Peekable, slice::Iter};

use crate::{
    ast::{BinaryOp, Expr, LiteralValue, UnaryOp},
    lexer::Token,
};

#[derive(Debug)]
pub enum ParseError {
    GenericError,
    ExpectedLiteral,
    ParenExpected,
}

pub struct Parser<'a> {
    tokens: Peekable<Iter<'a, Token>>,
}

/*
* expression    -> equality;
* equality      -> comparison (("!=" | "==") comparison)*;
* comparison    -> term ((">" | ">=" | "<" | "<=") term)*;
* term          -> factor (("+" | "-") factor)*;
* factor        -> unary (("*" | "/") unary)*;
* unary         -> ("!" | "-") unary | primary;
* primary       -> NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")";
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

impl<'a> Parser<'a> {
    pub fn new(tokens: &[Token]) -> Parser {
        Parser {
            tokens: tokens.iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> E {
        self.expression()
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

    fn peek(&mut self) -> Result<&Token, ParseError> {
        match self.tokens.peek() {
            Some(token) => Ok(token),
            None => Err(ParseError::GenericError),
        }
    }

    fn next_if_one_of(&mut self, tokens: &[Token]) -> Result<&Token, ParseError> {
        if let Some(token) = self.tokens.next_if(|&t| tokens.contains(t)) {
            return Ok(token);
        }

        Err(ParseError::GenericError)
    }

    fn expression(&mut self) -> E {
        self.equality()
    }

    fn equality(&mut self) -> E {
        let mut expr = self.comparison()?;
        while let Ok(token) = self.next_if_one_of(EQUALITY_TOKENS) {
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
        while let Ok(token) = self.next_if_one_of(COMPARISON_TOKENS) {
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
        while let Ok(token) = self.next_if_one_of(TERM_TOKENS) {
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
        while let Ok(token) = self.next_if_one_of(FACTOR_TOKENS) {
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
        if let Ok(token) = self.next_if_one_of(UNARY_TOKENS) {
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
            _ => Err(ParseError::ExpectedLiteral),
        }
    }
}
