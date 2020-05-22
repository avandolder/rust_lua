use std::convert::TryFrom;
use std::fmt;

use itertools::Itertools;

use crate::token::{self, Token};

pub struct Name(String);

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

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
    type Error = String;

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
            t => return Err(format!("Token type {:?} cannot be converted to BinaryOp.", t)),
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

pub enum UnaryOp {
    Not,
    Neg,
    Len,
}

impl TryFrom<token::Type> for UnaryOp {
    type Error = String;

    fn try_from(ty: token::Type) -> Result<Self, Self::Error> {
        Ok(match ty {
            token::Not => UnaryOp::Not,
            token::Sub => UnaryOp::Neg,
            token::Hash => UnaryOp::Len,
            t => return Err(format!("Token type {:?} cannot be converted to UnaryOp.", t)),
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

pub enum Field {
    Pair(Expr, Expr),
    Single(Expr),
}

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
            _ => return Err(format!("Token {} cannot be converted into Expression.", tok)),
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        #![allow(clippy::enum_glob_use)]
        use Expr::*;

        let expr = match self {
            BinaryOp(op, lhs, rhs) => format!("({} {} {})", lhs, op, rhs),
            UnaryOp(op, e) => format!("{}{}", op, e),
            Table(table) => {
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
                format!("{{{}}}", fields)
            }
            String(s) => s.clone(),
            Number(n) => n.to_string(),
            Bool(bool) => bool.to_string(),
            Nil => "nil".to_owned(),
            Vararg => "...".to_owned(),
            Name(name) => name.0.to_owned(),
            Index(e, index) => format!("{}[{}]", e, index),
            Call(e, args) => {
                let args = args.iter().map(Expr::to_string).join(", ");
                format!("{}({})", e, args)
            }
            Member(e, name) => format!("{}.{}", e, name.0),
            Method(e, name) => format!("{}:{}", e, name.0),
            Function => "function".to_owned(),
        };

        write!(f, "{}", expr)
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

pub enum Stmt {
    Assign(Vec<Expr>, Vec<Expr>),
    Block(Vec<Stmt>),
    Break,
    Call(Expr, Vec<Expr>),
    For(Name, Expr, Expr, Option<Expr>, Vec<Stmt>),
    ForIn(Vec<Name>, Vec<Expr>, Vec<Stmt>),
    Function(Vec<Name>, Vec<Name>, bool, Vec<Stmt>),
    If(Expr, Vec<Stmt>, Vec<Stmt>),
    Method(Vec<Name>, Name, Vec<Name>, bool, Vec<Stmt>),
    MethodCall(Expr, Name, Vec<Expr>),
    LocalAssign(Vec<Name>, Vec<Expr>),
    LocalFunction(Name, Vec<Name>, bool, Vec<Stmt>),
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

fn format_parameters(params: &[Name], is_vararg: bool) -> String {
    let params = join(params, ", ");
    let vararg = if is_vararg && !params.is_empty() {
        ", ..."
    } else if is_vararg {
        "..."
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
                format!("for {} = {}, {}{} do\n{}{}end", index, start, end, step, block, indent)
            }
            ForIn(indexes, exprs, block) => {
                let indexes = join(indexes, ", ");
                let exprs = join(exprs, ", ");
                let block = format_block(block, level + 1);
                format!("for {} in {} do\n{}{}end", indexes, exprs, block, indent)
            }
            Function(name, params, is_vararg, block) => {
                let name = join(name, ".");
                let params = format_parameters(params, *is_vararg);
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
                    else_block = format!("{}{}else\n{}", else_block, indent, format_block(block, level + 1));
                }

                format!("if {} then\n{}{}{}end", cond, then_block, else_block, indent)
            }
            Method(path, name, params, is_vararg, block) => {
                let path = join(path, ".");
                let params = format_parameters(params, *is_vararg);
                let block = format_block(block, level + 1);
                format!("function {}:{}({})\n{}{}end", path, name, params, block, indent)
            }
            MethodCall(object, name, args) => {
                let args = join(args, ", ");
                format!("{}:{}({})", object, name, args)
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
            LocalFunction(name, params, is_vararg, block) => {
                let params = format_parameters(params, *is_vararg);
                let block = format_block(block, level + 1);
                format!("local function {}({})\n{}{}end", name, params, block, indent)
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
                vec![Stmt::LocalAssign(vec![Name("a".to_owned())], vec![Expr::Number(1.0)])],
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
