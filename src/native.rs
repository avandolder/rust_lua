use crate::error::{self, LuaError, LuaResult};
use crate::interpreter::Interpreter;
use crate::value::Value;

pub fn print(_: &mut Interpreter, args: Vec<Value>) -> LuaResult<Value> {
    args.iter().take(1).for_each(|v| print!("{}", v));
    args.iter().skip(1).for_each(|v| print!("\t{}", v));
    println!();

    Ok(Value::Nil)
}

pub fn value_type(_int: &mut Interpreter, mut args: Vec<Value>) -> LuaResult<Value> {
    while let Some(Value::List(list)) = args.first() {
        args = list.clone();
    }
    if args.is_empty() {
        return LuaError::new(error::InvalidArguments);
    }

    Ok(Value::String(match &args[0] {
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
    }.to_owned()))
}