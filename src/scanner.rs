use std::fmt;

#[derive(Debug)]
pub enum Error {
    ScanError(String),
    Eof,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let t = match self {
            Error::ScanError(s) => s,
            Error::Eof => "EOF",
        };
        write!(f, "{}", t)
    }
}

pub type Result<T> = std::result::Result<T, Error>;

impl From<std::num::ParseIntError> for Error {
    fn from(err: std::num::ParseIntError) -> Error {
        Error::ScanError(err.to_string())
    }
}

impl From<std::num::ParseFloatError> for Error {
    fn from(err: std::num::ParseFloatError) -> Error {
        Error::ScanError(err.to_string())
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub row: u32,
    pub col: u32,
}

impl PartialEq for Position {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}:{})", self.row, self.col)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    Whitespace,
    Newline(Position),

    // symbols
    Colon(Position),
    OpenCurly(Position),
    CloseCurly(Position),
    OpenSquare(Position),
    CloseSquare(Position),
    Comma(Position),
    

    // number types
    Float(Position, f64),
    Integer(Position, i64),

    // strings
    String(Position, String),

    // other
    True(Position),
    False(Position),
    Null(Position),
}

impl Token {
    pub fn position(&self) -> Position {
        match self {
            Token::Eof => Position {row: 0, col: 0},
            Token::Whitespace => Position {row: 0, col: 0},
            Token::Newline(pos) => *pos,
            Token::Colon(pos) => *pos,
            Token::OpenCurly(pos) => *pos,
            Token::CloseCurly(pos) => *pos,
            Token::OpenSquare(pos) => *pos,
            Token::CloseSquare(pos) => *pos,
            Token::Comma(pos) => *pos,
            Token::Float(pos, _) => *pos,
            Token::Integer(pos, _) => *pos,
            Token::String(pos, _) => *pos,
            Token::True(pos) => *pos,
            Token::False(pos) => *pos,
            Token::Null(pos) => *pos,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Token::Eof => write!(f, ""),
            Token::Whitespace => write!(f, ""),
            Token::Newline(_) => writeln!(f),
            Token::Colon(_) => write!(f, ":"),
            Token::OpenCurly(_) => write!(f, "("),
            Token::CloseCurly(_) => write!(f, ")"),
            Token::OpenSquare(_) => write!(f, "["),
            Token::CloseSquare(_) => write!(f, "]"),
            Token::Comma(_) => write!(f, ","),
            Token::Float(_, d) => write!(f, "{}", d),
            Token::Integer(_, i) => write!(f, "{}", i),
            Token::String(_, s) => write!(f, "\"{}\"", s),
            Token::True(_) => write!(f, "true"),
            Token::False(_) => write!(f, "false"),
            Token::Null(_) => write!(f, "null"),
        }
    }
}

pub struct Scanner {
    input: Vec<char>,
    cur: usize,
    pos: Position,
}

impl Scanner {
    pub fn new(s: &str) -> Self {
        Scanner {
            input: s.chars().collect(),
            cur: 0,
            pos: Position {row: 1, col: 0},
        }
    }

    pub fn scan(&mut self) -> Result<Vec<Token>> {
        let mut tokens = vec![];
        loop {
            match self.scan_token() {
                Ok(Token::Eof) => break,
                Ok(Token::Whitespace) => continue,
                Ok(tok) => tokens.push(tok),
                Err(e) => return Err(self.create_error(e.to_string())),
            }
        }
        Ok(tokens)
    }

    pub fn scan_token(&mut self) -> Result<Token> {
        // this should be comprehensive
        let token = match self.peek(0) {
            '\0' => Ok(Token::Eof),
            ' ' | '\t' | '\r' => self.scan_whitespace(),
            '\n' => self.scan_newlines(),
            ':' => self.create_token(Token::Colon(self.pos)),
            ',' => self.create_token(Token::Comma(self.pos)),
            '{' => self.create_token(Token::OpenCurly(self.pos)),
            '}' => self.create_token(Token::CloseCurly(self.pos)),
            '[' => self.create_token(Token::OpenSquare(self.pos)),
            ']' => self.create_token(Token::CloseSquare(self.pos)),
            '"' => self.scan_string(),
            '0'..='9' | '-' => self.scan_number(),
            't' => self.scan_true(),
            'f' => self.scan_false(),
            'n' => self.scan_null(),
            c => Err(self.create_error(format!("invalid character: {}", c))),
        };
        token
    }

    pub fn create_error(&mut self, msg: String) -> Error {
        Error::ScanError(format!("scan_error at {}: {}", self.pos, msg))
    }

    pub fn create_token(&mut self, tok: Token) -> Result<Token> {
        self.read();
        Ok(tok)
    }

    pub fn peek(&mut self, n: usize) -> char {
        if self.cur < self.input.len() - n {
            self.input[self.cur + n]
        } else {
            '\0'
        }
    }

    pub fn read(&mut self) -> char {
        let ret = self.cur;

        if ret >= self.input.len() {
            self.cur += 1;
            return '\0';
        }

        if self.input[ret] == '\n' {
            self.pos.row += 1;
            self.pos.col = 0;
        } else {
            self.pos.col += 1;
        }

        self.cur += 1;
        self.input[ret]
    }

    pub fn scan_whitespace(&mut self) -> Result<Token> {
        loop {
            match self.peek(0) {
                ' ' | '\t' | '\r' => self.read(),
                _ => break,
            };
        }
        Ok(Token::Whitespace)
    }

    pub fn scan_newlines(&mut self) -> Result<Token> {
        loop {
            match self.peek(0) {
                '\n' => self.read(),
                _ => break,
            };
        };
        Ok(Token::Newline(self.pos))
    }

    pub fn scan_string(&mut self) -> Result<Token> {
        self.read();
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            match self.peek(0) {
                '"' => {
                    self.read();
                    break;
                },
                '\\' => {
                    match self.peek(1) {
                        '"' => {
                            self.read();
                            char_vec.push(self.read());
                        },
                        'a' => {
                            self.read();
                            self.read();
                            char_vec.push(0x07 as char);
                        }
                        'b' => {
                            self.read();
                            self.read();
                            char_vec.push(0x08 as char);
                        }
                        'f' => {
                            self.read();
                            self.read();
                            char_vec.push(0x0c as char);
                        }
                        'n' => {
                            self.read();
                            self.read();
                            char_vec.push(0x0a as char);
                        }
                        'r' => {
                            self.read();
                            self.read();
                            char_vec.push(0x0d as char);
                        }
                        't' => {
                            self.read();
                            self.read();
                            char_vec.push(0x09 as char);
                        }
                        'v' => {
                            self.read();
                            self.read();
                            char_vec.push(0x0b as char);
                        }
                        '\\' => {
                            self.read();
                            self.read();
                            char_vec.push('\\');
                        }
                        _ => {
                            self.read();
                            char_vec.push(self.read());
                        }
                    };
                }
                '\0' => return Err(self.create_error("EOF in string".to_string())),
                _ => char_vec.push(self.read()),
            }
        }
        let s: String = char_vec.into_iter().collect();
        Ok(Token::String(self.pos, s))
    }

    pub fn scan_number(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        let mut float = false;
        loop {
            match self.peek(0) {
                '0'..='9' | '-' | '+' => char_vec.push(self.read()),
                '.' | 'e' | 'E' => {
                    char_vec.push(self.read());
                    float = true;
                }
                _ => break,
            }
        }
        let s: String = char_vec.into_iter().collect();
        if float {
            let f: f64 = s.parse()?;
            Ok(Token::Float(self.pos, f))
        } else {
            let n: i64 = s.parse()?;
            Ok(Token::Integer(self.pos, n))
        }
    }

    pub fn scan_true(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            let c = self.peek(0);
            if is_identifier(c) {
                char_vec.push(c);
                self.read();
            } else {
                break;
            }
        }
        let s: String = char_vec.into_iter().collect();
        match s.as_str() {
            "true" => Ok(Token::True(self.pos)),
            _ => Err(self.create_error(format!("invalid identifier: {}", s))),
        }
    }

    pub fn scan_false(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            let c = self.peek(0);
            if is_identifier(c) {
                char_vec.push(c);
                self.read();
            } else {
                break;
            }
        }
        let s: String = char_vec.into_iter().collect();
        match s.as_str() {
            "false" => Ok(Token::False(self.pos)),
            _ => Err(self.create_error(format!("invalid identifier: {}", s))),
        }
    }

    pub fn scan_null(&mut self) -> Result<Token> {
        let mut char_vec: Vec<char> = Vec::new();
        loop {
            let c = self.peek(0);
            if is_identifier(c) {
                char_vec.push(c);
                self.read();
            } else {
                break;
            }
        }
        let s: String = char_vec.into_iter().collect();
        match s.as_str() {
            "null" => Ok(Token::Null(self.pos)),
            _ => Err(self.create_error(format!("invalid identifier: {}", s))),
        }
    }
}

fn is_digit(c: char) -> bool {
    return c >= '0' && c <= '9'
}

fn is_identifier_start(c: char) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_identifier(c: char) -> bool {
    return is_digit(c) || is_identifier_start(c)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scan_let1() {
        let text = "let a = 0";
        let tokens = Scanner::new(text).scan().unwrap();
        if let Token::Let(_) = &tokens[0] {
            assert_eq!(true, true);
        } else {
            panic!("expecting \"let\"");
        }
    }

    #[test]
    fn scan_let2() {
        let text = "let A = 0";
        let tokens = Scanner::new(text).scan().unwrap();
        if let Token::Let(_) = &tokens[0] {
            assert_eq!(true, true);
        } else {
            panic!("expecting \"let\"");
        }
    }

}
