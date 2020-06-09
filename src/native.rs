use crate::error::{self, LuaError, LuaResult};
use crate::interpreter::Interpreter;
use crate::value::Value;

pub fn print(_: &mut Interpreter, args: Vec<Value>) -> LuaResult<Value> {
    args.iter().take(1).for_each(|v| print!("{}", v));
    args.iter().skip(1).for_each(|v| print!("\t{}", v));
    println!();

    Ok(Value::Nil)
}

pub fn value_type(_: &mut Interpreter, args: Vec<Value>) -> LuaResult<Value> {
    let arg = match args.into_iter().next() {
        Some(Value::List(list)) => list
            .into_iter()
            .next()
            .map_or_else(|| LuaError::new(error::InvalidArguments), Ok)?,
        Some(value) => value,
        None => return LuaError::new(error::InvalidArguments),
    };

    Ok(Value::String(
        match arg {
            Value::Bool(_) => "boolean",
            Value::Function(_) => "function",
            Value::Nil => "nil",
            Value::Number(_) => "number",
            Value::String(_) => "string",
            Value::Table(_) => "table",
            Value::Thread => "thread",
            Value::Userdata => "userdata",
            // Handled earlier, should never happen.
            Value::List(_) => "",
        }
        .to_owned(),
    ))
}

pub fn assert(_: &mut Interpreter, args: Vec<Value>) -> LuaResult<Value> {
    match args.first() {
        None => LuaError::new(error::InvalidArguments),
        Some(value) if value.as_bool() => Ok(Value::List(args)),
        _ => LuaError::new(error::Assertion(args.get(1).map_or_else(
            || Ok("assertion failed!".to_owned()),
            |msg| msg.as_string(),
        )?)),
    }
}
