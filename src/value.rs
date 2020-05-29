use std::cell::RefCell;
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::rc::Rc;

use crate::ast::Field;

pub struct Table(pub Vec<Field>);

#[derive(Clone)]
pub enum Value {
    Bool(bool),
    Function(Rc<RefCell<()>>),
    Nil,
    Number(f64),
    String(String),
    Table(Rc<RefCell<Table>>),

    // Unimplemented:
    Thread,
    Userdata,
}

impl Value {
    pub fn as_bool(&self) -> bool {
        match self {
            Value::Nil | Value::Bool(false) => false,
            _ => true,
        }
    }

    pub fn as_number(&self) -> f64 {
        match self {
            Value::Number(value) => *value,
            Value::String(value) => value.parse::<f64>().unwrap(),
            _ => panic!(),
        }
    }

    pub fn as_string(&self) -> String {
        match self {
            Value::String(value) => value.clone(),
            Value::Number(value) => value.to_string(),
            _ => panic!(),
        }
    }

    pub fn length(&self) -> usize {
        match self {
            Value::String(value) => value.len(),
            Value::Table(value) => value.borrow().0.len(),
            _ => panic!(),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::Table(lhs), Value::Table(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs.partial_cmp(rhs),
            (Value::String(lhs), Value::String(rhs)) => lhs.partial_cmp(rhs),
            _ => None,
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(value) => write!(f, "{}", value),
            Value::Function(_function) => todo!(),
            Value::Nil => write!(f, "nil"),
            Value::Number(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Table(_table) => todo!(),
            Value::Thread => todo!(),
            Value::Userdata => todo!(),
        }
    }
}
