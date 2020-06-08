use crate::error::LuaResult;
use crate::interpreter::Interpreter;
use crate::value::Value;

pub fn print(_: &mut Interpreter, args: Vec<Value>) -> LuaResult<Value> {
    args.iter().take(1).for_each(|v| print!("{}", v));
    args.iter().skip(1).for_each(|v| print!("\t{}", v));
    println!();

    Ok(Value::Nil)
}