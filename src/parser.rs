use std::fmt;
use std::collections::HashMap;

use super::scanner;
use super::scanner::{Scanner, Token, Position};

#[derive(Debug)]
pub enum Expr {
    Map(HashMap<String, Expr>),
    Array(Vec<Expr>),
    String(String),
    Float(f64),
    Integer(i64),
    Bool(bool),
    Null(),
}

#[derive(Debug)]
pub enum Error {
    ParseError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::ParseError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<scanner::Error> for Error {
    fn from(err: scanner::Error) -> Error {
        Error::ParseError(err.to_string())
    }
}

pub struct Parser {
    tokens: Vec<Token>,
    cur: usize,
}

impl Parser {
    pub fn new(data: &str) -> Result<Self> {
        let tokens = Scanner::new(data).scan()?;
        let p = Parser {
            tokens: tokens,
            cur: 0,
        };
        Ok(p)
    }

    pub fn peek(&mut self, n: usize) -> Token {
        if self.cur < self.tokens.len() - n {
            self.tokens[self.cur + n].clone()
        } else {
            Token::Eof
        }
    }

    pub fn read(&mut self) -> Token {
        let ret = self.cur;

        if ret >= self.tokens.len() {
            self.cur += 1;
            return Token::Eof;
        }

        self.cur += 1;
        self.tokens[ret].clone()
    }

    pub fn create_error(&mut self, msg: &str) -> Error {
        if self.tokens.len() == 0 {
            return Error::ParseError(format!("empty file"));
        }
        if self.cur >= self.tokens.len() {
            return Error::ParseError(format!("parse_error at {}: {}", self.tokens[self.tokens.len()-1].position(), msg));
        }
        Error::ParseError(format!("parse_error at {}: {}", self.tokens[self.cur].position(), msg))
    }

    pub fn unexpected(&mut self, msg: &str) -> Error {
        let t = self.read();
        self.create_error(&format!("unexpected {} {}", t, msg))
    }

    pub fn require(&mut self, t: Token) -> Result<()> {
        let t2 = self.read();
        if t != t2 {
            return Err(self.create_error(&format!("expected {} but got {}", t, t2)));
        }
        Ok(())
    }

    pub fn require_string(&mut self) -> Result<String> {
        match self.read() {
            Token::String(_, s) => Ok(s),
            _ => Err(self.create_error("expected string")),
        }
    }

    pub fn require_float(&mut self) -> Result<f64> {
        match self.read() {
            Token::Float(_, s) => Ok(s),
            _ => Err(self.create_error("expected float")),
        }
    }

    pub fn require_integer(&mut self) -> Result<i64> {
        match self.read() {
            Token::Integer(_, s) => Ok(s),
            _ => Err(self.create_error("expected integer")),
        }
    }

    pub fn consume_whitespace(&mut self) {
        loop {
            match self.peek(0) {
                Token::Whitespace | Token::Newline(_) => self.read(),
                _ => break,
            };
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.consume_whitespace();
        let expr = self.parse_expr()?;
        self.consume_whitespace();
        Ok(expr)
    }

    pub fn parse_expr(&mut self) -> Result<Expr> {
        match self.peek(0) {
            Token::OpenCurly(_) => self.parse_map(),
            Token::OpenSquare(_) => self.parse_array(),
            _ => self.parse_value(),
        }
    }

    pub fn parse_keyvalue(&mut self) -> Result<(String, Expr)> {
        let key = self.require_string()?;
        self.consume_whitespace();
        self.require(Token::Colon(Position{col: 0,row: 0}))?;
        self.consume_whitespace();
        let expr = self.parse_expr()?;
        Ok((key, expr))
    }

    pub fn parse_map(&mut self) -> Result<Expr> {
        self.require(Token::OpenCurly(Position{col: 0,row: 0}))?;
        let mut map = HashMap::new();
        let mut first = true;
        loop {
            self.consume_whitespace();
            if let Token::CloseCurly(_) = self.peek(0) {
                break;
            }
            if let Token::Eof = self.peek(0) {
                return Err(self.create_error("unexpected EOF"));
            }
            if first {
                first = false;
            } else {
                self.require(Token::Comma(Position{col: 0,row: 0}))?;
            }
            self.consume_whitespace();
            let (key, value) = self.parse_keyvalue()?;
            map.insert(key, value);
        }
        self.require(Token::CloseCurly(Position{col: 0,row: 0}))?;
        Ok(Expr::Map(map))
    }

    pub fn parse_array(&mut self) -> Result<Expr> {
        self.require(Token::OpenSquare(Position{col: 0,row: 0}))?;
        let mut array = Vec::new();
        let mut first = true;
        loop {
            self.consume_whitespace();
            if let Token::CloseSquare(_) = self.peek(0) {
                break;
            }
            if let Token::Eof = self.peek(0) {
                return Err(self.create_error("unexpected EOF"));
            }
            if first {
                first = false;
            } else {
                self.require(Token::Comma(Position{col: 0,row: 0}))?;
            }
            self.consume_whitespace();
            let expr = self.parse_expr()?;
            array.push(expr);
        }
        self.require(Token::CloseSquare(Position{col: 0,row: 0}))?;
        Ok(Expr::Array(array))
    }

    pub fn parse_value(&mut self) -> Result<Expr> {
        match self.peek(0) {
            Token::String(_, _) => {
                let s = self.require_string()?;
                Ok(Expr::String(s))
            }
            Token::Float(_, _) => {
                let f = self.require_float()?;
                Ok(Expr::Float(f))
            }
            Token::Integer(_, _) => {
                let i = self.require_integer()?;
                Ok(Expr::Integer(i))
            }
            Token::True(_) => {
                self.read();
                Ok(Expr::Bool(true))
            }
            Token::False(_) => {
                self.read();
                Ok(Expr::Bool(false))
            }
            Token::Null(_) => {
                self.read();
                Ok(Expr::Null())
            }
            _ => Err(self.create_error("expected value")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse1() {
        let text = "(;GM[1])";
        let _ = Parser::new(text).unwrap().parse().unwrap();
    }
}
