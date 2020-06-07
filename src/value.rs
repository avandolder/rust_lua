use std::cell::RefCell;
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::fmt;
use std::rc::Rc;

use im::HashMap;

use crate::ast::{FunctionArity, FunctionType, Name, Stmt};
use crate::error::{self, LuaError, LuaResult};

#[derive(Clone, Debug)]
pub struct Function {
    pub params: Vec<Name>,
    pub arity: FunctionArity,
    pub body: Vec<Stmt>,
    pub scope: HashMap<String, Handle>,
}

impl Function {
    pub fn new(
        ftype: FunctionType,
        mut params: Vec<Name>,
        arity: FunctionArity,
        body: Vec<Stmt>,
        scope: HashMap<String, Handle>,
    ) -> Rc<RefCell<Self>> {
        if let FunctionType::Method = ftype {
            params.insert(0, Name("self".to_string()));
        }

        Rc::new(RefCell::new(Function {
            params,
            arity,
            body,
            scope,
        }))
    }
}

#[derive(Clone, Debug)]
pub struct Table(Vec<(Value, Handle)>);

impl Table {
    pub fn new(fields: Vec<(Value, Handle)>) -> Self {
        Self(fields)
    }

    pub fn get_handle(&mut self, key: Value) -> Handle {
        self.0
            .iter()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v.clone())
            .unwrap_or_else(|| {
                let handle = Handle::new();
                self.0.push((key, handle.clone()));
                handle
            })
    }

    pub fn get_value(&self, key: &Value) -> Value {
        self.0
            .iter()
            .find(|(k, _)| k == key)
            .map_or(Value::Nil, |(_, v)| v.value())
    }

    pub fn set(&mut self, key: Value, value: Value) {
        let index = self.0.iter()
            .enumerate()
            .find(|(_, (k, _))| *k == key)
            .map(|(index, _)| index);

        match (index, value) {
            (None, Value::Nil) => (),
            (None, value) => self.0.push((key, Handle::from_value(value))),
            (Some(index), Value::Nil) => { self.0.remove(index); }
            (Some(index), value) => self.0[index] = (key, Handle::from_value(value)),
        }
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl fmt::Display for Table {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{{")?;
        self.0.iter().take(1).try_for_each(|(key, value)| write!(f, "[{}] = {}", key, value))?;
        self.0.iter().skip(1).try_for_each(|(key, value)| write!(f, ", [{}] = {}", key, value))?;
        write!(f, "}}")
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Bool(bool),
    Function(Rc<RefCell<Function>>),
    Nil,
    Number(f64),
    String(String),
    Table(Rc<RefCell<Table>>),

    // Not technically a Lua value, used for multi-return values and varargs.
    List(Vec<Value>),

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

    pub fn as_number(&self) -> LuaResult<f64> {
        match self {
            Value::Number(value) => Ok(*value),
            Value::String(value) =>
                value.parse::<f64>().map_err(|_| LuaError { ty: error::ParseNumberError, line: 0 }),
            _ => LuaError::new(error::ValueNotValidNumber),
        }
    }

    pub fn as_string(&self) -> LuaResult<String> {
        match self {
            Value::String(value) => Ok(value.clone()),
            Value::Number(value) => Ok(value.to_string()),
            _ => LuaError::new(error::ValueNotValidString),
        }
    }

    pub fn get_handle(&self, key: Value) -> LuaResult<Handle> {
        if let Value::Table(table) = self {
            // TODO: support for index metamethods.
            Ok(table.borrow_mut().get_handle(key))
        } else {
            LuaError::new(error::IndexNonTableValue)
        }
    }

    pub fn get_value(&self, key: &Value) -> LuaResult<Value> {
        if let Value::Table(table) = self {
            // TODO: support for index metamethods.
            Ok(table.borrow_mut().get_value(key))
        } else {
            LuaError::new(error::IndexNonTableValue)
        }
    }

    pub fn length(&self) -> LuaResult<usize> {
        match self {
            Value::String(value) => Ok(value.len()),
            Value::Table(value) => Ok(value.borrow().len()),
            // TODO: add support for length metamethods.
            _ => LuaError::new(error::ValueHasNoLength),
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
            (Value::Table(_lhs), Value::Table(_rhs)) => todo!(),
            (Value::Function(_lhs), Value::Function(_rhs)) => todo!(),
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
            Value::String(value) => write!(f, "\"{}\"", value),
            Value::Table(table) => write!(f, "{}", table.borrow()),
            Value::List(list) => {
                list.iter().take(1).try_for_each(|value| write!(f, "{}", value))?;
                list.iter().skip(1).try_for_each(|value| write!(f, ", {}", value))
            }
            Value::Thread | Value::Userdata => todo!(),
        }
    }
}

impl Default for Value {
    fn default() -> Self {
        Self::Nil
    }
}

#[derive(Clone, Debug, Default)]
pub struct Handle(Rc<RefCell<Value>>);

impl Handle {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Value::Nil)))
    }

    pub fn from_value(value: Value) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn equals(&self, other: &Handle) -> bool {
        if self.is_table() && other.is_table() {
            Rc::ptr_eq(&self.0, &other.0)
        } else {
            *self.0.borrow() == *other.0.borrow()
        }
    }

    pub fn is_table(&self) -> bool {
        match *self.0.borrow() {
            Value::Table(_) => true,
            _ => false,
        }
    }

    pub fn value(&self) -> Value {
        self.0.borrow().clone()
    }

    pub fn set(&self, value: Value) {
        *self.0.borrow_mut() = value;
    }
}

impl fmt::Display for Handle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.borrow().fmt(f)
    }
}
