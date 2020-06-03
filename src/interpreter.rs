use im::HashMap;

use crate::ast::{BinaryOp, Expr, Stmt, UnaryOp};
use crate::error;
use crate::value::{Handle, Value};

pub struct Interpreter {
    globals: HashMap<String, Handle>,
    scope: HashMap<String, Handle>,
}

enum Branch {
    Return(Vec<Value>),
    Break,
    Throw(error::Error<'static>),
}

enum ScopeType {
    Outer,
    Inner,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            globals: HashMap::new(),
            scope: HashMap::new(),
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

            Expr::Vararg => todo!(),
            Expr::Name(_name) => todo!(),
            Expr::Index(_table_path, _index) => todo!(),
            Expr::Call(_function_path, _args) => todo!(),
            Expr::Member(_table_path, _name) => todo!(),
            Expr::Method(_table_path, _name) => todo!(),
        }
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), Branch> {
        match stmt {
            Stmt::Assign(_lhs, _rhs) => todo!(),
            Stmt::Block(body) => self.execute_block(body, ScopeType::Inner),
            Stmt::Break => Err(Branch::Break),
            Stmt::Call(_fexpr, _args) => todo!(),
            Stmt::For(_index, _start, _end, _step, _body) => todo!(),
            Stmt::ForIn(_names, _exprs, _body) => todo!(),
            Stmt::Function(_ftype, _fname, _params, _farity, _body) => todo!(),
            Stmt::If(_cond, _then_body, _else_body) => todo!(),
            Stmt::LocalAssign(_lhs, _rhs) => todo!(),
            Stmt::LocalFunction(_name, _params, _farity, _body) => todo!(),
            Stmt::Return(exprs) =>
                Err(Branch::Return(exprs.iter().map(|expr| self.evaluate(expr)).collect())),
            Stmt::Until(_cond, _body) => todo!(),
            Stmt::While(_cond, _body) => todo!(),
        }
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

pub fn interpret(expr: Expr) -> error::Result<'static, Value> {
    let mut interpreter = Interpreter::new();
    Ok(interpreter.evaluate(&expr))
}
