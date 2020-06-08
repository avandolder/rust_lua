use std::cell::RefCell;
use std::cmp::{Ordering, PartialEq, PartialOrd};
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

use im;

use crate::ast::{FunctionArity, FunctionType, Name, Stmt};
use crate::error::{self, LuaError, LuaResult};
use crate::interpreter::Interpreter;

type NativeFunction = fn(&mut Interpreter, Vec<Value>) -> LuaResult<Value>;

#[derive(Clone)]
pub enum Function {
    Lua(LuaFunction),
    Native(NativeFunction),
}

impl Function {
    pub fn from_native(func: NativeFunction) -> Value {
        Value::Function(Rc::new(RefCell::new(Function::Native(func))))
    }

    pub fn as_lua(&self) -> Option<&LuaFunction> {
        match self {
            Function::Lua(f) => Some(f),
            _ => None,
        }
    }
}

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::Lua(func) => func.fmt(f),
            Function::Native(_) => write!(f, "[Native Function]"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LuaFunction {
    pub params: Vec<Name>,
    pub arity: FunctionArity,
    pub body: Vec<Stmt>,
    pub scope: im::HashMap<String, Handle>,
}

impl LuaFunction {
    pub fn new(
        ftype: FunctionType,
        mut params: Vec<Name>,
        arity: FunctionArity,
        body: Vec<Stmt>,
        scope: im::HashMap<String, Handle>,
    ) -> Rc<RefCell<Function>> {
        if let FunctionType::Method = ftype {
            params.insert(0, Name("self".to_string()));
        }

        Rc::new(RefCell::new(Function::Lua(LuaFunction {
            params,
            arity,
            body,
            scope,
        })))
    }
}

#[derive(Clone, Debug)]
pub struct Table(HashMap<Value, Handle>);

impl Table {
    pub fn new(fields: Vec<(Value, Handle)>) -> Self {
        Self(fields.into_iter().collect())
    }

    pub fn get_handle(&mut self, key: Value) -> Handle {
        self.0.entry(key).or_default().clone()
    }

    pub fn get_value(&self, key: &Value) -> Value {
        self.0.get(key).map_or(Value::Nil, Handle::value)
    }

    pub fn set(&mut self, key: Value, value: Value) {
        match (self.0.get(&key), value) {
            (None, Value::Nil) => (),
            (Some(_), Value::Nil) => { self.0.remove(&key); }
            (_, value) => { self.0.insert(key, Handle::from_value(value)); }
        }
    }

    pub fn length(&self) -> usize {
        (1..).take_while(|&i| self.0.contains_key(&Value::Number(i as f64))).count()
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
            Value::List(list) => list.get(0).map_or(false, |v| v.as_bool()),
            _ => true,
        }
    }

    pub fn as_number(&self) -> LuaResult<f64> {
        match self {
            Value::Number(value) => Ok(*value),
            Value::String(value) => value.parse::<f64>().map_err(|_| LuaError {
                ty: error::ParseNumberError,
                line: 0,
            }),
            Value::List(list) => list.get(0).map_or_else(
                || LuaError::new(error::ValueNotValidNumber),
                |v| v.as_number()
            ),
            _ => LuaError::new(error::ValueNotValidNumber),
        }
    }

    pub fn as_string(&self) -> LuaResult<String> {
        match self {
            Value::Number(value) => Ok(value.to_string()),
            Value::String(value) => Ok(value.clone()),
            Value::List(list) => list.get(0).map_or_else(
                || LuaError::new(error::ValueNotValidString),
                |v| v.as_string()
            ),
            _ => LuaError::new(error::ValueNotValidString),
        }
    }

    pub fn as_function(&self) -> LuaResult<Rc<RefCell<Function>>> {
        if let Value::Function(func) = self {
            Ok(func.clone())
        } else if let Value::Table(_) = self {
            todo!("add support for tables with callable metamethods")
        } else {
            LuaError::new(error::ValueNotCallable)
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
            Value::Table(value) => Ok(value.borrow().length()),
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
            (Value::Table(lhs), Value::Table(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}
impl Eq for Value {}

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

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Bool(value) => value.hash(state),
            Value::Function(ptr) => Rc::into_raw(ptr.clone()).hash(state),
            Value::Nil => ().hash(state),
            // This is ... not good. Need to get a real hashing algorithm for f64.
            Value::Number(value) => value.to_bits().hash(state),
            Value::String(value) => value.hash(state),
            Value::Table(ptr) => Rc::into_raw(ptr.clone()).hash(state),
            Value::List(_list) => panic!("lists aren't hashable (this should never happen)"),
            Value::Thread | Value::Userdata => todo!(),
        }
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
