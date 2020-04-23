use std::convert::TryFrom;
use std::fmt;

use itertools::Itertools;

use crate::token::{self, Token};

pub struct Name(String);

impl<'a> TryFrom<Token<'a>> for Name {
    type Error = String;

    fn try_from(tok: Token<'a>) -> Result<Self, Self::Error> {
        if let token::Name = tok.ty {
            Ok(Name(tok.raw.iter().collect::<String>()))
        } else {
            Err(format!("Token {} cannot be converted into a Name.", tok))
        }
    }
}

pub enum Field {
    Pair(Expr, Expr),
    Single(Expr),
}

pub enum Expr {
    BinaryOp(token::Type, Box<Expr>, Box<Expr>),
    UnaryOp(token::Type, Box<Expr>),
    Table(Vec<Field>),
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
    Vararg,
    Name(Name),
    Index(Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Member(Box<Expr>, Name),
    Method(Box<Expr>, Name),
    Function,
}

impl<'a> TryFrom<Token<'a>> for Expr {
    type Error = String;

    fn try_from(tok: Token<'a>) -> Result<Self, Self::Error> {
        Ok(match tok.ty {
            token::Nil => Expr::Nil,
            token::True => Expr::Bool(true),
            token::False => Expr::Bool(false),
            token::Num => number(&tok),
            token::Str => string(&tok),
            token::Vararg => Expr::Vararg,
            token::Name => name(&tok),
            _ => return Err(format!("Token {} cannot be converted into expression.", tok)),
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let formatted_expr = match self {
            Expr::BinaryOp(op, lhs, rhs) => format!("({} {:?} {})", lhs, op, rhs),
            Expr::UnaryOp(op, e) => format!("{:?} {}", op, e),
            Expr::Table(table) => {
                let fields = table
                    .iter()
                    .map(|field| match field {
                        Field::Pair(key, value) => {
                            if let Expr::Name(name) = key {
                                format!("{} = {}", name.0, value)
                            } else {
                                format!("[{}] = {}", key, value)
                            }
                        }
                        Field::Single(value) => format!("{}", value),
                    })
                    .join(", ");
                format!("{{ {} }}", fields)
            }
            Expr::String(s) => format!("'{}'", s),
            Expr::Number(n) => n.to_string(),
            Expr::Bool(bool) => bool.to_string(),
            Expr::Nil => "nil".to_owned(),
            Expr::Vararg => "...".to_owned(),
            Expr::Name(name) => name.0.to_owned(),
            Expr::Index(e, index) => format!("{}[{}]", e, index),
            Expr::Call(e, args) => {
                let args = args.iter().map(|arg| arg.to_string()).join(", ");
                format!("{}({})", e, args)
            }
            Expr::Member(e, name) => format!("{}.{}", e, name.0),
            Expr::Method(e, name) => format!("{}:{}", e, name.0),
            Expr::Function => "function".to_owned(),
        };

        write!(f, "{}", formatted_expr)
    }
}

fn number(tok: &Token) -> Expr {
    let num = tok.raw.iter().collect::<String>();
    Expr::Number(num.parse::<f64>().unwrap())
}

fn string(tok: &Token) -> Expr {
    Expr::String(tok.raw.iter().collect::<String>())
}

fn name(tok: &Token) -> Expr {
    Expr::Name(Name(tok.raw.iter().collect::<String>()))
}
