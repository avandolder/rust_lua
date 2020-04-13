use std::collections::HashMap;

use lazy_static::lazy_static;
use maplit::hashmap;

#[rustfmt::skip]
#[derive(Clone, Debug,)]
pub enum Type {
    EQ, NEQ, GTE, LTE, GT, LT,

    Add, Sub, Div, Mul, Pow, Mod, Concat,

    Assign, Colon, Comma, Semicolon, Period,
    Hash,
    Vararg,
    LBrace, RBrace,
    LBracket, RBracket,
    LParen, RParen,

    // Keywords
    Nil, True, False,
    Do, End, While, For, In, If, Then, ElseIf, Else, Function, Repeat, Until,
    Local, Return, Break,
    And, Or, Not,

    Name(String),
    Num(f64, usize),
    Str(Vec<u8>, usize),
}

pub use Type::*;

impl Type {
    #[must_use]
    pub fn length(&self) -> usize {
        match self {
            Name(n) => n.len(),
            Num(_, size) | Str(_, size) => *size,

            Concat | EQ | NEQ | GTE | LTE | Do | In | If | Or => 2,
            Vararg | Nil | End | For | And | Not => 3,
            True | Then | Else => 4,
            Local | False | While | Until | Break => 5,
            ElseIf | Repeat | Return => 6,
            Function => 8,

            _ => 1,
        }
    }
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Type> = hashmap! {
        "do" => Do,
        "in" => In,
        "if" => If,
        "or" => Or,
        "nil" => Nil,
        "end" => End,
        "for" => For,
        "and" => And,
        "not" => Not,
        "true" => True,
        "then" => Then,
        "else" => Else,
        "local" => Local,
        "false" => False,
        "while" => While,
        "until" => Until,
        "break" => Break,
        "elseif" => ElseIf,
        "repeat" => Repeat,
        "return" => Return,
        "function" => Function,
    };
}

pub struct Token {
    pub ty: Type,
    pub line: usize,
}
