#![warn(clippy::all)]
#![warn(clippy::pedantic)]

pub mod ast;
pub mod error;
pub mod interpreter;
pub mod lexer;
pub mod parser;
pub mod token;
pub mod value;
