use std::hash::Hash;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::cmp::PartialEq;
use std::fmt;
use crate::parser::Expr;

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Number(f64),
    Str(String),
    List(Vec<Value>),
    Map(HashMap<Value, Value>),
    Function(Vec<String>, Box<Expr>),
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Number(a), Value::Number(b)) =>
                (*a * 1024.0 * 1024.0).round() as i32 == (*b * 1024.0 * 1024.0).round() as i32,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a.len() == b.len() && a.keys().all(|k| b.get(k) == a.get(k)),
            (Value::Function(a, body_a), Value::Function(b, body_b)) => a == b && format!("{:p}", body_a) == format!("{:p}", body_b),
            _ => false
        }
    }

    fn ne(&self, other: &Self) -> bool {
        return !self.eq(other)
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Number(n) => ((*n * 1000.0).round() as i32).hash(state),
            Value::Str(string) => string.hash(state),
            Value::Bool(b) => b.hash(state),
            Value::List(vec) => vec.hash(state),
            Value::Map(map) => map.values().collect::<Vec<&Value>>().hash(state),
            Value::Function(_, body) => format!("{:p}", body).hash(state),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(num) => num.fmt(f),
            Value::Bool(b) => b.fmt(f),
            Value::Str(string) => write!(f, "{:?}", string),
            Value::List(values) => {
                write!(f, "[").ok();
                write!(f, "{}",
                    values.iter()
                        .map(|a| a.to_string())
                        .collect::<Vec<String>>().join(", ")
                    ).ok();
                write!(f, "]")
            }
            Value::Map(values) => {
                write!(f, "{{").ok();
                write!(f, "{}",
                    values.iter()
                        .map(|(key, val)| format!("{key}: {val}"))
                        .collect::<Vec<String>>().join(", ")
                    ).ok();
                write!(f, "}}")
            }
            Value::Function(_, body) => write!(f, "function({:p})", body),
        }
    }
}


pub type Scope<'a> = HashMap<String, Rc<RefCell<Value>>>;

pub fn evaluate_expr(expr: Expr, scope: &mut Scope) -> Option<Value> {
    match expr {
        Expr::Value(value) => Some(value),
        Expr::Call(varbox, arguments) => evaluate_call(varbox.as_ref(), arguments, scope),
        Expr::Variable(variable) => match scope.get(&variable) {
            Some(value) => Some(value.borrow().to_owned()),
            None => None,
        }
    }
}

fn evaluate_call(function: &Expr, arguments: Vec<Expr>, scope: &mut Scope) -> Option<Value> {
    match stdlib(function, &arguments, scope) {
        Some(value) => return Some(value),
        None => ()
    }
    match evaluate_expr(function.clone(), scope)? {
        Value::Function(parameters, body) => {
            let mut function_scope = create_subscope(scope);
            parameters.iter().zip(arguments.iter()).for_each(|(parameter, arg)| {
                match evaluate_expr(arg.clone(), scope) {
                    Some(value) => {
                        function_scope.insert(parameter.to_owned(), Rc::new(RefCell::new(value)));
                    },
                    None => (),
                }
            });
            return evaluate_expr(*body, &mut function_scope)
        }
        _ => None,
    }
}

fn stdlib(function: &Expr, arguments: &Vec<Expr>, scope: &mut Scope) -> Option<Value> {
    match function {
        Expr::Variable(name) => {
            match &**name {
                "define" => {
                    match &arguments[..] {
                        [Expr::Variable(name), expr] => {
                            let value = evaluate_expr(expr.to_owned(), scope)?;
                            scope.insert(name.to_owned(), Rc::new(RefCell::new(value.clone())));
                            Some(value)
                        },
                        _ => return None,
                    }
                }
                "set" => {
                    match &arguments[..] {
                        [Expr::Variable(name), expr] => {
                            let value = evaluate_expr(expr.to_owned(), scope)?;
                            let mut variable_ref = scope.get_mut(name)?.borrow_mut();
                            *variable_ref = value.clone();
                            Some(value)
                        },
                        _ => return None,
                    }
                }
                "fn" => {
                    match &arguments[..] {
                        [Expr::Call(first, parameters), body] => {
                            let mut parameters = parameters.clone();
                            parameters.insert(0, *first.clone());
                            let parsed_params = parameters.iter().filter_map(|param| {
                                match param {
                                    Expr::Variable(name) => Some(name.clone()),
                                    _ => None
                                }
                            }).collect();
                            return Some(Value::Function(parsed_params, Box::new(body.to_owned())))
                        },
                        _ => None,
                    }
                }
                "do" => {
                    let mut arguments = arguments.clone();
                    let last = arguments.pop()?;
                    arguments.iter().map(|a| evaluate_expr(a.to_owned(), scope)).for_each(|_| {});
                    return evaluate_expr(last, scope);
                }
                "if" => {
                    match &arguments[..] {
                        [condition, if_true, if_false] => {
                            match evaluate_expr(condition.to_owned(), scope) {
                                Some(Value::Bool(true)) => evaluate_expr(if_true.to_owned(), scope),
                                Some(Value::Bool(false)) => evaluate_expr(if_false.to_owned(), scope),
                                _ => None
                            }
                        },
                        _ => None,
                    }
                }
                "while" => {
                    match &arguments[..] {
                        [condition, body] => {
                            loop {
                                match evaluate_expr(condition.to_owned(), scope) {
                                    Some(Value::Bool(true)) => {
                                        evaluate_expr(body.to_owned(), scope);
                                    }
                                    _ => break
                                }
                            }
                            return Some(Value::Bool(true))
                        },
                        _ => None,
                    }
                }
                "+" => {
                    let mut sum: f64 = 0.0;
                    for arg in arguments {
                        match evaluate_expr(arg.to_owned(), scope) {
                            Some(Value::Number(n)) => {sum += n;},
                            _ => {},
                        }
                    }
                    return Some(Value::Number(sum))
                }
                "*" => {
                    let mut prod: f64 = 1.0;
                    for arg in arguments {
                        match evaluate_expr(arg.to_owned(), scope) {
                            Some(Value::Number(n)) => {prod *= n;},
                            _ => {},
                        }
                    }
                    return Some(Value::Number(prod))
                }
                "-" => {
                    let mut sum: f64 = 0.0;
                    for (i, arg) in arguments.iter().enumerate() {
                        match evaluate_expr(arg.to_owned(), scope) {
                            Some(Value::Number(n)) => {
                                if i == 0 {
                                    sum += n;
                                } else {
                                    sum -= n;
                                }
                            },
                            _ => {},
                        }
                    }
                    return Some(Value::Number(sum))
                }
                "/" => {
                    let mut div: f64 = 1.0;
                    for (i, arg) in arguments.iter().enumerate() {
                        match evaluate_expr(arg.to_owned(), scope) {
                            Some(Value::Number(n)) => {
                                if i == 0 {
                                    div *= n;
                                } else {
                                    div /= n;
                                }
                            },
                            _ => {},
                        }
                    }
                    return Some(Value::Number(div))
                }
                "=" => {
                    let mut last: Option<Value> = None;
                    for argument in arguments {
                        match evaluate_expr(argument.to_owned(), scope) {
                            Some(value) => {
                                match &last {
                                    Some(last) => {
                                        if *last != value {
                                            return Some(Value::Bool(false))
                                        }
                                    },
                                    None => {
                                        last = Some(value);
                                    }
                                }
                            },
                            None => return None
                        }
                    }
                    return Some(Value::Bool(true))
                }
                "not" => {
                    match &arguments[..] {
                        [arg] => {
                            match evaluate_expr(arg.to_owned(), scope)? {
                                Value::Bool(true) => return Some(Value::Bool(false)),
                                Value::Bool(false) => return Some(Value::Bool(true)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                "and" => {
                    match &arguments[..] {
                        [arg1, arg2] => {
                            match (evaluate_expr(arg1.to_owned(), scope)?, evaluate_expr(arg2.to_owned(), scope)?) {
                                (Value::Bool(a), Value::Bool(b)) => return Some(Value::Bool(a && b)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                "or" => {
                    match &arguments[..] {
                        [arg1, arg2] => {
                            match (evaluate_expr(arg1.to_owned(), scope)?, evaluate_expr(arg2.to_owned(), scope)?) {
                                (Value::Bool(a), Value::Bool(b)) => return Some(Value::Bool(a || b)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                "<" => {
                    match &arguments[..] {
                        [arg1, arg2] => {
                            match (evaluate_expr(arg1.to_owned(), scope)?, evaluate_expr(arg2.to_owned(), scope)?) {
                                (Value::Number(a), Value::Number(b)) => return Some(Value::Bool(a < b)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                ">" => {
                    match &arguments[..] {
                        [arg1, arg2] => {
                            match (evaluate_expr(arg1.to_owned(), scope)?, evaluate_expr(arg2.to_owned(), scope)?) {
                                (Value::Number(a), Value::Number(b)) => return Some(Value::Bool(a > b)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                "," => {
                    let list: Vec<Value> = arguments.iter()
                        .filter_map(|arg| evaluate_expr(arg.to_owned(), scope))
                        .collect();
                    return Some(Value::List(list))
                }
                ":" => {
                    let mut map: HashMap<Value, Value> = HashMap::new();
                    for argument in arguments {
                        match argument {
                            Expr::Call(boxed_key, boxed_value) => {
                                let key = evaluate_expr(*boxed_key.clone(), scope)?;
                                let value = evaluate_expr(boxed_value.get(0)?.to_owned(), scope)?;
                                map.insert(key, value);
                            },
                            _ => {}
                        }
                    }
                    return Some(Value::Map(map))
                }
                "len" => {
                    match &arguments[..] {
                        [list] => {
                            match evaluate_expr(list.to_owned(), scope)? {
                                Value::List(list) => return Some(Value::Number(list.len() as f64)),
                                Value::Map(map) => return Some(Value::Number(map.len() as f64)),
                                _ => return None
                            }
                        }
                        _ => return None
                    }
                }
                "get" => {
                    match &arguments[..] {
                        [list_or_map, key] => {
                            match (evaluate_expr(list_or_map.to_owned(), scope)?, evaluate_expr(key.to_owned(), scope)?) {
                                (Value::List(list), Value::Number(i)) => {
                                    match list.get(i as usize) {
                                        Some(v) => Some(v.to_owned()),
                                        _ => None,
                                    }
                                },
                                (Value::Map(map), key) => {
                                    match map.get(&key) {
                                        Some(v) => Some(v.to_owned()),
                                        _ => None,
                                    }
                                },
                                _ => None,
                            }
                        },
                        _ => return None,
                    }
                }
                "insert" => {
                    match &arguments[..] {
                        [map, Expr::Call(key, value)] => {
                            match (&mut evaluate_expr(map.to_owned(), scope)?, evaluate_expr(*key.to_owned(), scope)?, evaluate_expr(value.get(0)?.clone(), scope)?) {
                                (Value::Map(map), key, value) => {
                                    map.insert(key, value.to_owned());
                                    return Some(value.to_owned());
                                },
                                _ => return None
                            }
                        },
                        _ => return None
                    }
                }
                "push" => {
                    match &arguments[..] {
                        [list, value] => {
                            match (&mut evaluate_expr(list.to_owned(), scope)?, evaluate_expr(value.to_owned(), scope)?) {
                                (Value::List(list), value) => {
                                    list.push(value);
                                    return Some(Value::List(list.to_vec()))
                                },
                                _ => return None,
                            }
                        },
                        _ => return None,
                    }
                }
                _ => return None
            }
        },
        _ => return None
    }
}

fn create_subscope<'a>(scope: &'a Scope) -> Scope<'a> {
    let mut new_scope = Scope::new();
    for (variable, value) in scope.iter() {
        new_scope.insert(variable.to_string(), Rc::clone(&value));
    }
    return new_scope
}
