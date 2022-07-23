use std::iter::Peekable;
use std::fmt;

use crate::tokenizer::{Token, TokenValue};
use crate::evaluator::Value;

#[derive(Clone)]
pub enum Expr {
    Value(Value),
    Call(Box<Expr>, Vec<Expr>),
    Variable(String),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Value(value) => write!(f, "{value}"),
            Expr::Call(name, arguments) => {
                write!(f, "{name}(").ok();
                write!(f, "{}",
                    arguments.iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>().join(", ")
                    ).ok();
                write!(f, ")")
            }
            Expr::Variable(name) => write!(f, "{name}"),
        }
    }
}

fn parse_expr<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Option<Expr> {
    match tokens.peek()? {
        Token{value: TokenValue::LParen, ..} => consume_call(tokens),
        Token{value: TokenValue::RParen, ..} => None,
        Token{value: TokenValue::Name(_), ..} => consume_variable(tokens),
        _ => consume_value(tokens),
    }
}

fn consume_call<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Option<Expr> {
    tokens.next();
    let name = parse_expr(tokens)?;
    let mut arguments: Vec<Expr> = Vec::new();
    loop {
        match tokens.peek()? {
            Token{value: TokenValue::RParen, ..} => {tokens.next(); break},
            _ => {}
        }
        match parse_expr(tokens) {
            Some(expr) => arguments.push(expr),
            None => break
        }
    }
    return Some(Expr::Call(Box::new(name), arguments))
}

fn consume_variable<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Option<Expr> {
    match tokens.next() {
        Some(Token{value: TokenValue::Name(name), ..}) => Some(Expr::Variable(name)),
        _ => None,
    }
}

fn consume_value<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Option<Expr> {
    match tokens.next()? {
        Token{value: TokenValue::Str(string), ..} => Some(Expr::Value(Value::Str(string))),
        Token{value: TokenValue::Number(number), ..} => Some(Expr::Value(Value::Number(number))),
        _ => None,
    }
}

pub fn parse<T: Iterator<Item = Token>>(tokens: &mut Peekable<T>) -> Vec<Expr> {
    let mut output: Vec<Expr> = Vec::new();
    loop {
        match parse_expr(tokens) {
            Some(expr) => output.push(expr),
            None => return output,
        }
    }
}


