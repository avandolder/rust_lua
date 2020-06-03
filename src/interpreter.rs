use im::HashMap;

use crate::ast::{BinaryOp, Expr, Stmt, UnaryOp};
use crate::error;
use crate::value::{Handle, Value};

struct Frame {
    args: Vec<Value>,
}

struct Interpreter {
    globals: HashMap<String, Handle>,
    scope: HashMap<String, Handle>,
    stack_frame: Vec<Frame>,
}

enum Branch {
    Return(Vec<Value>),
    Break,
    Throw(error::Error<'static>),
}

impl Branch {
    fn ret(values: Vec<Value>) -> Result<(), Self> {
        Err(Self::Return(values))
    }

    fn throw(err: error::Type<'static>) -> Result<(), Self> {
        Err(Self::Throw(err.as_error()))
    }
}

impl From<error::Error<'static>> for Branch {
    fn from(err: error::Error<'static>) -> Self {
        Branch::Throw(err)
    }
}

enum ScopeType {
    Outer,
    Inner,
}

impl Interpreter {
    fn new(args: Vec<Value>) -> Self {
        Interpreter {
            globals: HashMap::new(),
            scope: HashMap::new(),
            stack_frame: vec![Frame {args}],
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Nil => Value::Nil,
            Expr::Bool(value) => Value::Bool(*value),
            Expr::Number(value) => Value::Number(*value),
            Expr::String(value) => Value::String(parse_string(&value)),

            Expr::Function => todo!(),
            Expr::Table(_table) => todo!(),

            Expr::BinaryOp(op, lhs, rhs) => {
                let (lhs, rhs) = (self.evaluate(lhs), self.evaluate(rhs));

                // If either operand is a table, do a metamethod lookup.
                if let Value::Table(_lhs) = lhs {
                    todo!()
                } else if let Value::Table(_rhs) = rhs {
                    todo!()
                }

                match op {
                    BinaryOp::Add => Value::Number(lhs.as_number() + rhs.as_number()),
                    BinaryOp::Sub => Value::Number(lhs.as_number() - rhs.as_number()),
                    BinaryOp::Mul => Value::Number(lhs.as_number() * rhs.as_number()),
                    BinaryOp::Div => Value::Number(lhs.as_number() / rhs.as_number()),
                    BinaryOp::Mod => Value::Number(lhs.as_number() % rhs.as_number()),
                    BinaryOp::Pow => Value::Number(lhs.as_number().powf(rhs.as_number())),
                    BinaryOp::Concat => Value::String(lhs.as_string() + &rhs.as_string()),
                    BinaryOp::And => if lhs.as_bool() { rhs } else { lhs },
                    BinaryOp::Or => if lhs.as_bool() { lhs } else { rhs },
                    BinaryOp::EQ => Value::Bool(lhs == rhs),
                    BinaryOp::NEQ => Value::Bool(lhs != rhs),
                    BinaryOp::GTE => Value::Bool(lhs >= rhs),
                    BinaryOp::LTE => Value::Bool(lhs <= rhs),
                    BinaryOp::GT => Value::Bool(lhs > rhs),
                    BinaryOp::LT => Value::Bool(lhs < rhs),
                }
            }

            Expr::UnaryOp(op, expr) => {
                let value = self.evaluate(expr);

                // Handle potential metamethod lookup.
                if let Value::Table(_table) = value {
                    todo!()
                }

                match op {
                    UnaryOp::Not => Value::Bool(!value.as_bool()),
                    UnaryOp::Neg => Value::Number(-value.as_number()),
                    UnaryOp::Len => Value::Number(value.length() as f64),
                }
            }

            Expr::Vararg => Value::List(self.stack_frame.last().unwrap().args.clone()),
            Expr::Name(name) => if let Some(handle) = self.scope.get(name.as_str()) {
                    handle.value()
                } else if let Some(handle) = self.globals.get(name.as_str()) {
                    handle.value()
                } else {
                    Value::Nil
                },
            Expr::Index(_table_path, _index) => todo!(),
            Expr::Call(_function_path, _args) => todo!(),
            Expr::Member(_table_path, _name) => todo!(),
            Expr::Method(_table_path, _name) => todo!(),
        }
    }

    fn resolve(&mut self, expr: &Expr) -> Handle {
        match expr {
            Expr::Name(name) => {
                if let Some(handle) = self.scope.get(name.as_str()) {
                    handle.clone()
                } else if let Some(handle) = self.globals.get(name.as_str()) {
                    handle.clone()
                } else {
                    let new_handle = Handle::new();
                    self.globals.insert(name.to_string(), new_handle.clone());
                    new_handle
                }
            }
            _ => todo!(),
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Branch> {
        match stmt {
            Stmt::Assign(lhs, rhs) => {
                for (handle, value) in lhs.iter().zip(rhs.iter()) {
                    let value = self.evaluate(value);
                    self.resolve(handle).set(value);
                }
                // TODO: evaluate any remaining handles/values, they could have side-effects.
            },

            Stmt::Block(body) => self.execute_block(body, ScopeType::Inner)?,

            Stmt::Break => Err(Branch::Break)?,

            Stmt::Call(_fexpr, _args) => todo!(),

            Stmt::For(index, start, end, step, body) => {
                let end = self.evaluate(end);
                let step = step
                    .as_ref()
                    .map(|expr| self.evaluate(&expr))
                    .unwrap_or(Value::Number(1.0));
                let index_handle = Handle::from_value(self.evaluate(start));
                self.scope.insert(index.to_string(), index_handle.clone());
                let mut body = body.into_iter();

                while index_handle.value() < end {
                    if let Some(stmt) = body.next() {
                        match self.execute(stmt) {
                            Err(Branch::Break) => break,
                            br => br?,
                        }
                    } else {
                        break;
                    }

                    index_handle.set( 
                        Value::Number(index_handle.value().as_number() + step.as_number()));
                }

                self.scope.remove(&index.to_string());
            },

            Stmt::ForIn(_names, _exprs, _body) => todo!(),
            Stmt::Function(_ftype, _fname, _params, _farity, _body) => todo!(),

            Stmt::If(cond, then_body, else_body) => {
                let cond = self.evaluate(cond);
                if cond.as_bool() {
                    self.execute_block(then_body, ScopeType::Inner)?;
                } else {
                    self.execute_block(else_body, ScopeType::Inner)?;
                }
            }

            Stmt::LocalAssign(lhs, rhs) => {
                for (name, expr) in lhs.iter().zip(rhs.iter()) {
                    let value = self.evaluate(expr);
                    let handle = Handle::from_value(value);
                    self.scope.insert(name.to_string(), handle);
                }
                // TODO: evaluate any remaining exprs, they could have side-effects.
            }

            Stmt::LocalFunction(_name, _params, _farity, _body) => todo!(),
            Stmt::Return(exprs) =>
                Branch::ret(exprs.iter().map(|expr| self.evaluate(expr)).collect())?,
            Stmt::Until(_cond, _body) => todo!(),
            Stmt::While(_cond, _body) => todo!(),
        }
        Ok(())
    }

    fn execute_block(&mut self, stmts: &Vec<Stmt>, scope_type: ScopeType) -> Result<(), Branch> {
        let prev_scope = self.scope.clone();
        if let ScopeType::Outer = scope_type {
            self.scope = HashMap::new();
        }
        let branch = stmts.iter().try_for_each(|stmt| self.execute(stmt));        
        self.scope = prev_scope;        
        branch
    }
}

fn parse_string(s: &str) -> String {
    let opener = s.chars().next().unwrap();

    s.chars().skip(1).take_while(|&c| c != opener).collect()
}

pub fn interpret(ast: Vec<Stmt>, args: Vec<Value>) -> error::Result<'static, Value> {
    let mut interpreter = Interpreter::new(args);
    for stmt in &ast {
        match interpreter.execute(stmt) {
            Ok(()) => (),
            Err(Branch::Return(values)) => return Ok(Value::List(values)),
            Err(Branch::Throw(err)) => return Err(err),
            Err(Branch::Break) => panic!("top-level break statment"),
        }
    }
    Ok(Value::Nil)
}
