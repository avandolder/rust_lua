use std::convert::TryFrom;
use std::fmt;

use itertools::Itertools;

use crate::error::{self, LuaError};
use crate::token::{self, Token};

#[derive(Clone, Debug)]
pub struct Name(pub String);

impl Name {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'a> TryFrom<Token<'a>> for Name {
    type Error = LuaError;

    fn try_from(tok: Token<'a>) -> Result<Self, Self::Error> {
        if let token::Name = tok.ty {
            Ok(Name(tok.raw.iter().collect::<String>()))
        } else {
            Err(LuaError {
                ty: error::InvalidName(tok.to_string()),
                line: tok.line,
            })
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Concat,
    And,
    Or,
    EQ,
    NEQ,
    GTE,
    LTE,
    GT,
    LT,
}

impl TryFrom<token::Type> for BinaryOp {
    type Error = LuaError;

    fn try_from(ty: token::Type) -> Result<Self, Self::Error> {
        Ok(match ty {
            token::Add => BinaryOp::Add,
            token::Sub => BinaryOp::Sub,
            token::Mul => BinaryOp::Mul,
            token::Div => BinaryOp::Div,
            token::Mod => BinaryOp::Mod,
            token::Pow => BinaryOp::Pow,
            token::Concat => BinaryOp::Concat,
            token::And => BinaryOp::And,
            token::Or => BinaryOp::Or,
            token::EQ => BinaryOp::EQ,
            token::NEQ => BinaryOp::NEQ,
            token::GTE => BinaryOp::GTE,
            token::LTE => BinaryOp::LTE,
            token::GT => BinaryOp::GT,
            token::LT => BinaryOp::LT,
            t => LuaError::new(error::InvalidBinaryOp(t))?,
        })
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #![allow(clippy::enum_glob_use)]
        use BinaryOp::*;

        let formatted_op = match self {
            Add => "+",
            Sub => "-",
            Mul => "*",
            Div => "/",
            Mod => "%",
            Pow => "^",
            Concat => "..",
            And => "and",
            Or => "or",
            EQ => "==",
            NEQ => "~=",
            GTE => ">=",
            LTE => "<=",
            GT => ">",
            LT => "<",
        };
        write!(f, "{}", formatted_op)
    }
}

#[derive(Clone, Copy, Debug)]
pub enum UnaryOp {
    Not,
    Neg,
    Len,
}

impl TryFrom<token::Type> for UnaryOp {
    type Error = LuaError;

    fn try_from(ty: token::Type) -> Result<Self, Self::Error> {
        Ok(match ty {
            token::Not => UnaryOp::Not,
            token::Sub => UnaryOp::Neg,
            token::Hash => UnaryOp::Len,
            t => LuaError::new(error::InvalidUnaryOp(t))?,
        })
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #![allow(clippy::enum_glob_use)]
        use UnaryOp::*;

        let formatted_op = match self {
            Not => "not ",
            Neg => "-",
            Len => "#",
        };
        write!(f, "{}", formatted_op)
    }
}

#[derive(Clone, Debug)]
pub enum Field {
    Pair(Expr, Expr),
    Single(Expr),
}

impl Field {
    pub fn as_pair(&self) -> Option<(&Expr, &Expr)> {
        match self {
            Field::Pair(x, y) => Some((x, y)),
            Field::Single(_) => None,
        }
    }

    pub fn as_single(&self) -> Option<&Expr> {
        match self {
            Field::Pair(_, _) => None,
            Field::Single(x) => Some(x),
        }
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Field::Pair(Expr::Name(name), value) => write!(f, "{} = {}", name, value),
            Field::Pair(key, value) => write!(f, "[{}] = {}", key, value),
            Field::Single(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
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
    Function(Vec<Name>, FunctionArity, Vec<Stmt>),
}

impl<'a> TryFrom<Token<'a>> for Expr {
    type Error = LuaError;

    fn try_from(tok: Token<'a>) -> Result<Self, Self::Error> {
        Ok(match tok.ty {
            token::Nil => Expr::Nil,
            token::True => Expr::Bool(true),
            token::False => Expr::Bool(false),
            token::Num => number(&tok),
            token::Str => string(&tok),
            token::Vararg => Expr::Vararg,
            token::Name => name(&tok),
            _ => Err(LuaError {
                ty: error::TokenIsNotValidExpression(tok.to_string()),
                line: tok.line,
            })?,
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::BinaryOp(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::UnaryOp(op, e) => write!(f, "{}{}", op, e),
            Expr::Table(table) => {
                write!(f, "{{")?;
                table
                    .first()
                    .iter()
                    .try_for_each(|field| write!(f, "{}", field))?;
                table
                    .iter()
                    .skip(1)
                    .try_for_each(|field| write!(f, ", {}", field))?;
                write!(f, "}}")
            }
            Expr::String(s) => write!(f, "{}", s),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Bool(b) => write!(f, "{}", b),
            Expr::Nil => write!(f, "nil"),
            Expr::Vararg => write!(f, "..."),
            Expr::Name(name) => write!(f, "{}", name.0),
            Expr::Index(e, index) => write!(f, "{}[{}]", e, index),
            Expr::Call(e, args) => {
                write!(f, "{}(", e)?;
                args.first()
                    .iter()
                    .try_for_each(|arg| write!(f, "{}", arg))?;
                args.iter()
                    .skip(1)
                    .try_for_each(|arg| write!(f, ", {}", arg))?;
                write!(f, ")")
            }
            Expr::Member(e, name) => write!(f, "{}.{}", e, name.0),
            Expr::Method(e, name) => write!(f, "{}:{}", e, name.0),
            Expr::Function(params, arity, body) => {
                write!(f, "function(")?;
                params
                    .iter()
                    .take(1)
                    .try_for_each(|param| write!(f, "{}", param))?;
                params
                    .iter()
                    .skip(1)
                    .try_for_each(|param| write!(f, ", {}", param))?;

                if let FunctionArity::Variable = arity {
                    if params.is_empty() {
                        write!(f, "...")?;
                    } else {
                        write!(f, ", ...")?;
                    }
                }
                writeln!(f, ")")?;

                body.iter()
                    .try_for_each(|stmt| writeln!(f, "{}", stmt.format()))?;
                write!(f, "end")
            }
        }
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

#[derive(Clone, Copy, Debug)]
pub enum FunctionArity {
    Variable,
    Fixed,
}

#[derive(Clone, Copy, Debug)]
pub enum FunctionType {
    Method,
    Static,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Assign(Vec<Expr>, Vec<Expr>),
    Block(Vec<Stmt>),
    Break,
    Call(Expr, Vec<Expr>),
    For(Name, Box<Expr>, Box<Expr>, Option<Expr>, Vec<Stmt>),
    ForIn(Vec<Name>, Vec<Expr>, Vec<Stmt>),
    Function(FunctionType, Expr, Vec<Name>, FunctionArity, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    LocalAssign(Vec<Name>, Vec<Expr>),
    LocalFunction(Name, Vec<Name>, FunctionArity, Vec<Stmt>),
    Return(Vec<Expr>),
    Until(Expr, Vec<Stmt>),
    While(Expr, Vec<Stmt>),
}

fn join<T: fmt::Display>(ts: &[T], sep: &str) -> String {
    ts.iter().map(std::string::ToString::to_string).join(sep)
}

fn format_block(stmts: &[Stmt], level: usize) -> String {
    stmts.iter().map(|stmt| stmt.format_level(level)).join("")
}

fn format_parameters(params: &[Name], arity: FunctionArity) -> String {
    let params = join(params, ", ");
    let vararg = if let FunctionArity::Variable = arity {
        if params.is_empty() {
            "..."
        } else {
            ", ..."
        }
    } else {
        ""
    };
    params + vararg
}

impl Stmt {
    pub fn format(&self) -> String {
        self.format_level(0)
    }

    fn format_level(&self, level: usize) -> String {
        #![allow(clippy::enum_glob_use)]
        use Stmt::*;

        let indent = "  ".repeat(level);
        let stmt = match self {
            Assign(vars, exprs) => {
                let vars = join(vars, ", ");
                let exprs = join(exprs, ", ");
                format!("{} = {}", vars, exprs)
            }
            Block(stmts) => {
                let stmts = format_block(stmts, level + 1);
                format!("do\n{}end", stmts)
            }
            Break => "break".to_owned(),
            Call(func, args) => {
                let args = join(args, ", ");
                format!("{}({})", func, args)
            }
            For(index, start, end, step, block) => {
                let step = step
                    .as_ref()
                    .map(|expr| format!(", {}", expr.to_string()))
                    .unwrap_or_default();
                let block = format_block(block, level + 1);
                format!(
                    "for {} = {}, {}{} do\n{}{}end",
                    index, start, end, step, block, indent
                )
            }
            ForIn(indexes, exprs, block) => {
                let indexes = join(indexes, ", ");
                let exprs = join(exprs, ", ");
                let block = format_block(block, level + 1);
                format!("for {} in {} do\n{}{}end", indexes, exprs, block, indent)
            }
            Function(_ftype, name, params, arity, block) => {
                let name = name.to_string();
                let params = format_parameters(params, *arity);
                let block = format_block(block, level + 1);
                format!("function {}({})\n{}{}end", name, params, block, indent)
            }
            If(cond, then_block, else_block) => {
                let then_block = format_block(then_block, level + 1);

                let mut block = else_block;
                let mut else_block = String::new();
                while let Some(If(c, t, e)) = block.first() {
                    if block.len() > 1 {
                        break;
                    }

                    let t = format_block(t, level + 1);
                    else_block.push_str(&format!("{}elseif {} then\n{}", indent, c, t));
                    block = e;
                }

                if !block.is_empty() {
                    else_block = format!(
                        "{}{}else\n{}",
                        else_block,
                        indent,
                        format_block(block, level + 1)
                    );
                }

                format!(
                    "if {} then\n{}{}{}end",
                    cond, then_block, else_block, indent
                )
            }
            LocalAssign(names, exprs) => {
                let names = join(names, ", ");
                let exprs = join(exprs, ", ");
                if exprs.is_empty() {
                    format!("local {}", names)
                } else {
                    format!("local {} = {}", names, exprs)
                }
            }
            LocalFunction(name, params, arity, block) => {
                let params = format_parameters(params, *arity);
                let block = format_block(block, level + 1);
                format!(
                    "local function {}({})\n{}{}end",
                    name, params, block, indent
                )
            }
            Return(exprs) => {
                let exprs = join(exprs, ", ");
                format!("return {}", exprs)
            }
            Until(cond, block) => {
                let block = format_block(block, level + 1);
                format!("repeat\n{}{}until {}", block, indent, cond)
            }
            While(cond, block) => {
                let block = format_block(block, level + 1);
                format!("while {} do\n{}{}end", cond, block, indent)
            }
        };

        format!("{}{}\n", indent, stmt)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn format_statement() {
        use super::{Expr, Name, Stmt};
        let stmt = Stmt::Block(vec![
            Stmt::LocalAssign(vec![Name("a".to_owned())], vec![]),
            Stmt::While(Expr::Bool(true), vec![Stmt::Break]),
        ]);

        assert_eq!(
            stmt.format(),
            r#"do
  local a
  while true do
    break
  end
end
"#
        );
    }

    #[test]
    fn format_elseif() {
        use super::{Expr, Name, Stmt};

        let stmt = Stmt::If(
            Expr::Bool(true),
            vec![Stmt::Call(Expr::Name(Name("print".to_owned())), vec![])],
            vec![Stmt::If(
                Expr::Nil,
                vec![Stmt::Return(vec![Expr::Table(vec![])])],
                vec![Stmt::LocalAssign(
                    vec![Name("a".to_owned())],
                    vec![Expr::Number(1.0)],
                )],
            )],
        );

        assert_eq!(
            stmt.format(),
            r#"if true then
  print()
elseif nil then
  return {}
else
  local a = 1
end
"#
        );
    }
}
