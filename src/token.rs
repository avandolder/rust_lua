use std::collections::HashMap;
use std::fmt;

use lazy_static::lazy_static;
use maplit::hashmap;

#[rustfmt::skip]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    EOF,
    Name,
    Num,
    Str,
}

pub use Type::*;

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

#[derive(Clone, Debug)]
pub struct Token<'a> {
    pub ty: Type,
    pub line: usize,
    pub raw: &'a [char],
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let raw_string = self.raw.iter().collect::<String>();
        write!(f, "Token {:?} '{}' on line {}", self.ty, raw_string, self.line)
    }
}
