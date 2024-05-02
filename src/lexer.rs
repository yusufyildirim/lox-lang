use std::{iter::Peekable, str::Chars};

#[derive(PartialEq, Debug)]
pub enum Token {
    // Single-char tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two char tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    // EOF
    Unknown,
    EoF,
}

struct Scanner<'source> {
    source: Peekable<Chars<'source>>,
    tokens: Vec<Token>,
}

impl<'source> Scanner<'source> {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            source: source.chars().peekable(),
            tokens: Vec::new(),
        }
    }

    fn next_while<F>(&mut self, func: F) -> String
    where
        F: Fn(&char) -> bool,
    {
        let mut consumed = "".to_string();

        while let Some(c) = self.source.next_if(|x| func(x)) {
            consumed.push(c);
        }

        consumed
    }

    fn next_if(&mut self, ch: char) -> bool {
        self.source.next_if_eq(&ch).is_some()
    }

    fn add_token(&mut self, token: Token) {
        if token != Token::Unknown {
            self.tokens.push(token);
        }
    }

    pub fn scan_tokens(&mut self) {
        while self.scan_token().is_ok() {}
        println!("Tokens {:?}", self.tokens);
    }

    fn scan_token(&mut self) -> Result<(), ()> {
        let c = match self.source.next() {
            Some(ch) => ch,
            None => return Err(()),
        };

        let token = match c {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '-' => Token::Minus,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '*' => Token::Star,
            '/' => Token::Slash,

            // Operators
            '!' => match self.next_if('=') {
                true => Token::BangEqual,
                false => Token::Bang,
            },
            '=' => match self.next_if('=') {
                true => Token::EqualEqual,
                false => Token::Equal,
            },
            '<' => match self.next_if('=') {
                true => Token::LessEqual,
                false => Token::Less,
            },
            '>' => match self.next_if('=') {
                true => Token::GreaterEqual,
                false => Token::Greater,
            },
            // String, Number, Identifier
            '"' => self.string(),
            x if x.is_numeric() => self.number(x),
            x if x.is_alphabetic() || x == '_' => self.identifier(x),
            _ => Token::Unknown,
        };

        self.add_token(token);
        Ok(())
    }

    fn number(&mut self, first_digit: char) -> Token {
        let mut number = first_digit.to_string();

        let base_digits = self.next_while(|c| c.is_numeric());
        number.push_str(&base_digits[..]);

        // Handle fractions
        if self.source.next_if_eq(&'.').is_some() {
            let fractional_digits = self.next_while(|c| c.is_numeric());
            if fractional_digits.is_empty() {
                // TODO: Throw a syntax error if no number's found after '.'
            }
            number = format!("{}.{}", number, fractional_digits);
        }

        let number: f64 = number.parse().unwrap();
        Token::Number(number)
    }

    fn string(&mut self) -> Token {
        let text = self.next_while(|c| c != &'"');

        match self.source.next() {
            Some(_) => Token::String(text),
            None => {
                // TODO: Throw unterminated string error
                Token::Unknown
            }
        }
    }

    fn identifier(&mut self, initial_letter: char) -> Token {
        let mut id = initial_letter.to_string();
        id.push_str(&self.next_while(|c| c.is_alphanumeric())[..]);

        match &id[..] {
            "fun" => Token::Fun,
            "return" => Token::Return,
            "var" => Token::Var,
            "true" => Token::True,
            "false" => Token::False,
            "nil" => Token::Nil,
            "and" => Token::And,
            "or" => Token::Or,
            "if" => Token::If,
            "else" => Token::Else,
            "for" => Token::For,
            "while" => Token::While,
            "class" => Token::Class,
            "super" => Token::Super,
            "this" => Token::This,
            "print" => Token::Print,
            _ => Token::Identifier(id),
        }
    }
}

pub fn scan(source: &str) -> Vec<Token> {
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens();

    println!("Lexing: {source} ");
    scanner.tokens
}
