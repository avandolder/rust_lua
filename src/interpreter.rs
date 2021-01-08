use im::HashMap;

use crate::ast::{BinaryOp, Expr, Field, FunctionArity, FunctionType, Stmt, UnaryOp};
use crate::error::{self, LuaError, LuaResult};
use crate::native;
use crate::value::{Function, Handle, LuaFunction, Table, Value};

#[derive(Clone, Debug)]
pub struct Interpreter {
    globals: HashMap<String, Handle>,
    scope: HashMap<String, Handle>,

    // arguments represents what is returned by the ... operator within the current executing
    // context. I.e., it stores any command line arguments on the program start, or the list
    // of arguments to the current var-arg function call.
    arguments: Option<Vec<Value>>,
}

#[derive(Clone, Debug)]
pub enum Branch {
    Return(Value),
    Break,
    Throw(LuaError),
}

impl Branch {
    fn ret(values: Vec<Value>) -> Result<(), Self> {
        Err(Self::Return(Value::List(values)))
    }

    fn throw<T>(ty: error::Type) -> Result<T, Self> {
        Err(Self::Throw(LuaError { ty, line: 0 }))
    }
}

impl From<LuaError> for Branch {
    fn from(err: LuaError) -> Self {
        Branch::Throw(err)
    }
}

macro_rules! native {
    ($e:expr, $id:ident) => {
        (
            $e.to_string(),
            Handle::from_value(Function::from_native(native::$id)),
        )
    };
}

macro_rules! table {
    ($e:expr, $($k:expr => $v:ident),*) => {
        (
            $e.to_string(),
            Handle::from_value(Value::Table(Table::new(vec![
                $(
                    (
                        Value::String($k.to_string()),
                        Handle::from_value(Function::from_native(native::$v)),
                    ),
                )*
            ])))
        )
    };
}

impl Interpreter {
    pub fn new(args: Vec<Value>) -> Self {
        // Register global functions.
        let globals: &[(String, Handle)] = &[
            native!("assert", assert),
            native!("print", print),
            native!("tonumber", tonumber),
            native!("tostring", tostring),
            native!("type", value_type),
            native!("pack", pack),
            native!("unpack", unpack),
            table!("os", "exit" => exit),
        ];

        Interpreter {
            arguments: Some(args),
            globals: globals.into(),
            scope: HashMap::new(),
        }
    }

    fn evaluate(&mut self, expr: &Expr) -> LuaResult<Value> {
        Ok(match expr {
            Expr::Nil => Value::Nil,
            Expr::Bool(value) => Value::Bool(*value),
            Expr::Number(value) => Value::Number(*value),
            Expr::String(value) => Value::String(parse_string(&value)),

            Expr::Function(params, arity, body) => Value::Function(LuaFunction::new(
                FunctionType::Static,
                params.clone(),
                *arity,
                body.clone(),
                self.scope.clone(),
            )),

            Expr::Table(fields) => {
                let single_fields = fields
                    .iter()
                    .filter_map(Field::as_single)
                    .enumerate()
                    .map(|(index, value)| {
                        Ok((
                            Value::Number(index as f64 + 1.0),
                            Handle::from_value(self.evaluate(value)?),
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?;
                let mut pair_fields = fields
                    .iter()
                    .rev()
                    .filter_map(Field::as_pair)
                    .map(|(key, value)| {
                        Ok((
                            {
                                match key {
                                    Expr::Name(name) => Value::String(name.to_string()),
                                    key => self.evaluate(key)?,
                                }
                            },
                            Handle::from_value(self.evaluate(value)?),
                        ))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let mut fields: Vec<_> = single_fields;
                fields.append(&mut pair_fields);
                fields.dedup_by_key(|(key, _)| key.clone());
                Value::Table(Table::new(fields))
            }

            Expr::BinaryOp(op, lhs, rhs) => {
                let lhs = self.evaluate(lhs)?;

                // Handle short-circuiting operations first.
                match op {
                    BinaryOp::And if lhs.as_bool() => self.evaluate(rhs)?,
                    BinaryOp::And => lhs,
                    BinaryOp::Or if lhs.as_bool() => lhs,
                    BinaryOp::Or => self.evaluate(rhs)?,
                    op => {
                        let rhs = self.evaluate(rhs)?;

                        // If either operand is a table, do a metamethod lookup.
                        match (&lhs, &rhs) {
                            (Value::Table(t), _o) | (_o, Value::Table(t))
                                if t.borrow().meta.is_some() =>
                            {
                                todo!()
                            }
                            _ => (),
                        }

                        match op {
                            BinaryOp::Add => Value::Number(lhs.as_number()? + rhs.as_number()?),
                            BinaryOp::Sub => Value::Number(lhs.as_number()? - rhs.as_number()?),
                            BinaryOp::Mul => Value::Number(lhs.as_number()? * rhs.as_number()?),
                            BinaryOp::Div => Value::Number(lhs.as_number()? / rhs.as_number()?),
                            BinaryOp::Mod => Value::Number(lhs.as_number()? % rhs.as_number()?),
                            BinaryOp::Pow => Value::Number(lhs.as_number()?.powf(rhs.as_number()?)),
                            BinaryOp::Concat => Value::String(lhs.as_string()? + &rhs.as_string()?),
                            BinaryOp::EQ => Value::Bool(lhs == rhs),
                            BinaryOp::NEQ => Value::Bool(lhs != rhs),
                            BinaryOp::GTE => Value::Bool(lhs >= rhs),
                            BinaryOp::LTE => Value::Bool(lhs <= rhs),
                            BinaryOp::GT => Value::Bool(lhs > rhs),
                            BinaryOp::LT => Value::Bool(lhs < rhs),
                            _ => unreachable!(),
                        }
                    }
                }
            }

            Expr::UnaryOp(op, expr) => {
                let value = self.evaluate(expr)?;

                // TODO: handle potential metamethod lookup.
                match op {
                    UnaryOp::Not => Value::Bool(!value.as_bool()),
                    UnaryOp::Neg => Value::Number(-value.as_number()?),
                    UnaryOp::Len => Value::Number(value.length()? as f64),
                }
            }

            Expr::Vararg => {
                if let Some(ref args) = self.arguments {
                    Value::List(args.clone())
                } else {
                    return LuaError::new(error::VarargOutsideOfVarargFunction);
                }
            }

            Expr::Name(name) => {
                if let Some(handle) = self.scope.get(name.as_str()) {
                    handle.value()
                } else if let Some(handle) = self.globals.get(name.as_str()) {
                    handle.value()
                } else {
                    Value::Nil
                }
            }

            Expr::Call(expr, args) => self.call_function(expr, args)?,

            Expr::Index(expr, key) => {
                let table = self.evaluate(expr)?;
                let key = self.evaluate(key)?;
                table.get_value(&key)?
            }
            Expr::Member(expr, name) | Expr::Method(expr, name) => {
                let table = self.evaluate(expr)?;
                let key = Value::String(name.to_string());
                table.get_value(&key)?
            }
        })
    }

    fn resolve(&mut self, expr: &Expr) -> LuaResult<Handle> {
        Ok(match expr {
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
            Expr::Index(expr, key) => {
                let table = self.evaluate(expr)?;
                let key = self.evaluate(key)?;
                table.get_handle(key)?
            }
            Expr::Member(expr, name) | Expr::Method(expr, name) => {
                let table = self.evaluate(expr)?;
                let key = Value::String(name.to_string());
                table.get_handle(key)?
            }
            _ => LuaError::new(error::UnresolvableExpression)?,
        })
    }

    pub fn execute(&mut self, stmt: &Stmt) -> Result<(), Branch> {
        match stmt {
            Stmt::Assign(lhs, rhs) => {
                let (mut vars, mut exprs) = (lhs.iter(), rhs.iter());
                while let (Some(var), Some(expr)) = (vars.next(), exprs.next()) {
                    let value = match self.evaluate(expr)? {
                        Value::List(list) => list.into_iter().next().unwrap_or(Value::Nil),
                        value => value,
                    };
                    self.resolve(var)?.set(value);
                }
                for var in vars {
                    self.resolve(var)?;
                }
                for expr in exprs {
                    self.evaluate(expr)?;
                }
            }

            Stmt::Block(body) => self.execute_block(body)?,

            Stmt::Break => return Err(Branch::Break),

            Stmt::Call(fexpr, args) => {
                self.call_function(fexpr, args)?;
            }

            Stmt::For(index_name, start, end, step, body) => {
                let prev_scope = self.scope.clone();

                let end = self.evaluate(end)?.as_number()?;
                let step = step
                    .as_ref()
                    .map::<Result<f64, LuaError>, _>(|expr| {
                        Ok(self.evaluate(&expr)?.as_number()?)
                    })
                    .unwrap_or(Ok(1.0))?;
                let index = Handle::from_value(self.evaluate(start)?);
                self.scope.insert(index_name.to_string(), index.clone());

                while index.value().as_number()? <= end {
                    match body.iter().try_for_each(|stmt| self.execute(stmt)) {
                        Err(Branch::Break) => break,
                        br => br?,
                    }

                    index.set(Value::Number(index.value().as_number()? + step));
                }

                self.scope = prev_scope;
            }

            Stmt::ForIn(_names, _exprs, _body) => todo!(),
            Stmt::Function(ftype, fname, params, arity, body) => {
                let handle = self.resolve(fname)?;
                let func = LuaFunction::new(
                    *ftype,
                    params.clone(),
                    *arity,
                    body.clone(),
                    self.scope.clone(),
                );
                handle.set(Value::Function(func));
            }

            Stmt::If(cond, then_body, else_body) => {
                let cond = self.evaluate(cond)?;
                if cond.as_bool() {
                    self.execute_block(then_body)?;
                } else {
                    self.execute_block(else_body)?;
                }
            }

            Stmt::LocalAssign(lhs, rhs) => {
                let (mut names, mut exprs) = (lhs.iter(), rhs.iter());
                while let (Some(name), Some(expr)) = (names.next(), exprs.next()) {
                    let value = match self.evaluate(expr)? {
                        Value::List(list) => list.into_iter().next().unwrap_or(Value::Nil),
                        value => value,
                    };
                    let handle = Handle::from_value(value);
                    self.scope.insert(name.to_string(), handle);
                }
                for name in names {
                    self.scope.insert(name.to_string(), Handle::new());
                }
                for expr in exprs {
                    self.evaluate(expr)?;
                }
            }

            Stmt::LocalFunction(name, params, arity, body) => {
                let func = LuaFunction::new(
                    FunctionType::Static,
                    params.clone(),
                    *arity,
                    body.clone(),
                    self.scope.clone(),
                );
                let handle = Handle::from_value(Value::Function(func));
                self.scope.insert(name.to_string(), handle);
            }

            Stmt::Return(exprs) => Branch::ret(
                exprs
                    .iter()
                    .map(|expr| self.evaluate(expr))
                    .collect::<Result<Vec<_>, _>>()?,
            )?,

            Stmt::Until(cond, body) => {
                // Local variables within the repeat..until block can appear in the
                // condition, so we can't use execute_block here.
                loop {
                    let prev_scope = self.scope.clone();
                    match body.iter().try_for_each(|stmt| self.execute(stmt)) {
                        Err(Branch::Break) => return Ok(()),
                        br => br?,
                    }
                    if self.evaluate(cond)?.as_bool() {
                        self.scope = prev_scope;
                        break;
                    }
                    self.scope = prev_scope;
                }
            }
            Stmt::While(cond, body) => {
                while self.evaluate(cond)?.as_bool() {
                    match self.execute_block(body) {
                        Err(Branch::Break) => return Ok(()),
                        br => br?,
                    }
                }
            }
        }
        Ok(())
    }

    fn execute_block(&mut self, stmts: &[Stmt]) -> Result<(), Branch> {
        let prev_scope = self.scope.clone();
        let branch = stmts.iter().try_for_each(|stmt| self.execute(stmt));
        self.scope = prev_scope;
        branch
    }

    fn call_function(&mut self, func: &Expr, args: &[Expr]) -> LuaResult<Value> {
        let mut args = args
            .iter()
            .map(|arg| self.evaluate(arg))
            .collect::<Result<Vec<_>, _>>()?;
        if let Expr::Method(expr, _) = func.clone() {
            args.insert(0, self.evaluate(&expr)?)
        }

        let func = self.evaluate(func)?.as_function()?.borrow().clone();
        let (params, arity, body, scope) = match func {
            Function::Lua(LuaFunction {
                params,
                arity,
                body,
                scope,
            }) => (params, arity, body, scope),
            Function::Native(func) => return func(self, args),
        };

        let prev_scope = self.scope.clone();
        self.scope = scope;

        let mut args = args.into_iter();
        let mut params = params.iter();
        while let (Some(param), Some(arg)) = (params.next(), args.next()) {
            self.scope
                .insert(param.to_string(), Handle::from_value(arg));
        }
        for param in params {
            self.scope
                .insert(param.to_string(), Handle::from_value(Value::Nil));
        }

        let prev_arguments = self.arguments.take();
        let arguments = args.collect::<Vec<_>>();
        if let FunctionArity::Variable = arity {
            self.arguments = Some(arguments);
        }

        let result = match body.iter().try_for_each(|stmt| self.execute(stmt)) {
            Err(Branch::Return(result)) => result,
            Ok(()) => Value::Nil,
            Err(Branch::Break) => LuaError::new(error::BreakNotInsideLoop)?,
            Err(Branch::Throw(err)) => Err(err)?,
        };

        self.arguments = prev_arguments;
        self.scope = prev_scope;

        Ok(result)
    }
}

fn parse_string(s: &str) -> String {
    let opener = s.chars().next().unwrap();
    s.chars().skip(1).take_while(|&c| c != opener).collect()
}

pub fn interpret(ast: &[Stmt], args: Vec<Value>) -> error::LuaResult<Value> {
    let mut interpreter = Interpreter::new(args);
    for stmt in ast {
        match interpreter.execute(stmt) {
            Ok(()) => (),
            Err(Branch::Return(value)) => return Ok(value),
            Err(Branch::Throw(err)) => return Err(err),
            Err(Branch::Break) => return LuaError::new(error::BreakNotInsideLoop),
        }
    }
    Ok(Value::Nil)
}
