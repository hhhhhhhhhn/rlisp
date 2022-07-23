use std::io::Read;
use std::io::Bytes;

#[derive(Debug, Clone)]
pub enum TokenValue {
    LParen,
    RParen,
    Name(String),
    Number(f64),
    Str(String),
}

#[derive(Clone, Copy, Debug)]
pub struct Loc {
    row: i32,
    col: i32,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub loc: Loc,
}

pub struct Input {
    stdin: Bytes<std::io::Stdin>,
    current: Option<u8>,
    loc: Loc,
}

impl Input {
    pub fn new() -> Input {
        Input {
            stdin: std::io::stdin().bytes(),
            current: None,
            loc: Loc {row: 1, col: 1},
        }
    }

    // In row, col format
    pub fn loc(&self) -> Loc {
        return self.loc
    }
}

impl Iterator for Input {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current.is_some() {
            let ret = self.current;
            self.current = None;
            return ret;
        }

        let byte = self.stdin.next()?.ok()?;

        if byte == 0xA { // Newline
            self.loc.row += 1;
            self.loc.col = 1;
        } else {
            self.loc.col += 1;
        }

        return Some(byte)
    }
}

impl Input {
    fn peek(&mut self) -> Option<u8> {
        if self.current.is_none() {
            self.current = self.next();
        }
        return self.current;
    }
}

fn read_token(input: &mut Input) -> Option<Token> {
    consume_spaces(input);
    match input.peek()? {
        0x28 | 0x29 => consume_paren(input),
        0x30..=0x39 => consume_number(input),
        0x22 => consume_string(input),
        _ => consume_name(input),
    }
}

fn consume_spaces(input: &mut Input) {
    while input.peek() == Some(0xA) || input.peek() == Some(0x20) {
        input.next();
    }
}

fn consume_number(input: &mut Input) -> Option<Token> {
    let loc = input.loc();
    let mut number: Vec<u8> = Vec::new();
    loop {
        match input.peek() {
            Some(0x30..=0x39) | Some(0x2d..=0x2e) => {
                let chr = input.next()?;
                number.push(chr);
            }
            _ => return Some(Token{
                loc,
                value: TokenValue::Number(
                    String::from_utf8(number).ok()?.parse().ok()?
                )
            }),
        }
    }
}

fn consume_paren(input: &mut Input) -> Option<Token> {
    let loc = input.loc();
    match input.next()? {
        0x28 => Some(Token{loc, value: TokenValue::LParen}),
        0x29 => Some(Token{loc, value: TokenValue::RParen}),
        _ => None,
    }
}

fn consume_string(input: &mut Input) -> Option<Token> {
    let loc = input.loc();
    let mut string: Vec<u8> = Vec::new();
    input.next(); // Consume quote
    loop {
        match input.next() {
            Some(0x5C) => { // Backslash
                match input.next() {
                    Some(0x6e) => {
                        string.push(0xA);
                    },
                    Some(chr) => string.push(chr),
                    None => return None,
                }
            }
            // 0x22 = quote
            Some(0x22) => return Some(Token{
                value: TokenValue::Str(String::from_utf8(string).ok()?),
                loc,
            }),
            Some(chr) => string.push(chr),
            None => return None,
        }
    }
}

fn consume_name(input: &mut Input) -> Option<Token> {
    let loc = input.loc();
    let mut name: Vec<u8> = Vec::new();
    loop {
        match input.peek() {
            Some(0xA | 0x20 | 0x28..=0x29) => return Some(Token{ // \n, " ", (, )
                value: TokenValue::Name(String::from_utf8(name).ok()?),
                loc,
            }),
            _ => {
                let chr = input.next()?;
                name.push(chr);
            }
        }
    }
}

pub fn tokenize(input: &mut Input) -> Vec<Token> {
    let mut output: Vec<Token> = Vec::new();
    loop {
        match read_token(input) {
            Some(token) => output.push(token),
            None => return output,
        }
    }
}

