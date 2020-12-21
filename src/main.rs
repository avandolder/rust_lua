use std::fs;
use std::io::{self, Write};

use rust_lua::{
    error::{self, LuaError},
    interpreter::{Branch, Interpreter},
    lexer, parser,
    value::Value,
};

fn read_line() -> io::Result<String> {
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;
    Ok(line)
}

fn interpret_source(interpreter: &mut Interpreter, src: &str) -> bool {
    let src = src.chars().collect::<Vec<_>>();
    let tokens = lexer::tokenize(&src);

    let ast = match parser::parse(tokens) {
        Ok(ast) => ast,
        Err(LuaError {
            ty: error::UnexpectedEndOfInput(_),
            ..
        })
        | Err(LuaError {
            ty: error::UnexpectedEOF,
            ..
        }) => return true,
        err => {
            println!("parsing error: {:?}", err);
            return false;
        }
    };

    match ast.iter().try_for_each(|stmt| interpreter.execute(stmt)) {
        Ok(()) => (),
        Err(Branch::Return(value)) => println!("==> {}", value),
        Err(Branch::Throw(err)) => println!("error: {:?}", err),
        Err(Branch::Break) => println!("error: top-level break"),
    }

    false
}

fn main() -> io::Result<()> {
    let args = std::env::args().skip(1).map(Value::String).collect();
    let mut interpreter = Interpreter::new(args);

    if let Some("-f") = std::env::args().nth(1).as_deref() {
        let src = fs::read_to_string(std::env::args().nth(2).unwrap())?;
        interpret_source(&mut interpreter, &src);
    } else {
        println!("rust lua");
    }

    loop {
        print!("> ");
        io::stdout().flush()?;
        let mut src = read_line()?;

        while interpret_source(&mut interpreter, &src) {
            src.push_str(read_line()?.as_str());
        }
    }
}
