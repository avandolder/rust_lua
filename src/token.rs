use std::collections::HashMap;

use lazy_static::lazy_static;
use maplit::hashmap;

#[rustfmt::skip]
#[derive(Clone, Debug)]
pub enum Kind {
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

impl Kind {
    pub fn length(&self) -> usize {
        use Kind::*;

        match self {
            Name(n) => n.len(),
            Num(_, size) => *size,
            Str(_, size) => *size,

            // Keywords
            Do | In | If | Or => 2,
            Nil | End | For | And | Not => 3,
            True | Then | Else => 4,
            Local | False | While | Until | Break => 5,
            ElseIf | Repeat | Return => 6,
            Function => 8,

            // Symbols
            Vararg => 3,
            Concat | EQ | NEQ | GTE | LTE => 2,
            _ => 1,
        }
    }
}

lazy_static! {
    pub static ref KEYWORDS: HashMap<&'static str, Kind> = hashmap! {
        "do" => Kind::Do,
        "in" => Kind::In,
        "if" => Kind::If,
        "or" => Kind::Or,
        "nil" => Kind::Nil,
        "end" => Kind::End,
        "for" => Kind::For,
        "and" => Kind::And,
        "not" => Kind::Not,
        "true" => Kind::True,
        "then" => Kind::Then,
        "else" => Kind::Else,
        "local" => Kind::Local,
        "false" => Kind::False,
        "while" => Kind::While,
        "until" => Kind::Until,
        "break" => Kind::Break,
        "elseif" => Kind::ElseIf,
        "repeat" => Kind::Repeat,
        "return" => Kind::Return,
        "function" => Kind::Function,
    };
}

pub struct Token {
    pub kind: Kind,
    pub line: usize,
}
