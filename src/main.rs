use std::fs;
use std::io::{self, Write};

use rust_lua::{
    interpreter::{Branch, Interpreter},
    lexer,
    parser,
    value::Value,
};

fn read_line() -> io::Result<String> {
    let mut line = String::new();
    io::stdin().read_line(&mut line)?;
    Ok(line)
}

fn main() -> io::Result<()> {
    let args = std::env::args().skip(2).map(Value::String).collect();
    let mut interpreter = Interpreter::new(args);

    if let Some(path) = std::env::args().nth(1) {
        let src = fs::read_to_string(path)?.chars().collect::<Vec<_>>();
        let tokens = lexer::tokenize(&src);
        let ast = parser::parse(tokens);
        match ast.iter().try_for_each(|stmt| interpreter.execute(stmt)) {
            Ok(()) => (),
            Err(Branch::Return(value)) => println!("==> {}", value),
            Err(Branch::Throw(err)) => println!("error: {:?}", err),
            Err(Branch::Break) => println!("error: top-level break"),
        }
    } else {
        println!("rust lua");
    }

    loop {
        print!("> ");
        io::stdout().flush()?;

        let src = read_line()?.chars().collect::<Vec<_>>();
        let tokens = lexer::tokenize(&src);
        let ast = parser::parse(tokens);
        for stmt in &ast {
            match interpreter.execute(stmt) {
                Ok(()) => (),
                Err(Branch::Return(value)) => println!("==> {}", value),
                Err(Branch::Throw(err)) => println!("error: {:?}", err),
                Err(Branch::Break) => println!("error: top-level break"),
            }
        }
    }
}
