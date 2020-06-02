use crate::ast::{BinaryOp, Expr, UnaryOp};
use crate::error;
use crate::value::Value;

fn parse_string(s: String) -> String {
    let opener = s.chars().next().unwrap();

    s.chars().skip(1).take_while(|&c| c != opener).collect()
}

pub fn eval_expr(expr: Expr) -> Value {
    match expr {
        Expr::Nil => Value::Nil,
        Expr::Bool(value) => Value::Bool(value),
        Expr::Number(value) => Value::Number(value),
        Expr::String(value) => Value::String(parse_string(value)),

        Expr::Function => todo!(),
        Expr::Table(_table) => todo!(),

        Expr::BinaryOp(op, lhs, rhs) => {
            let (lhs, rhs) = (eval_expr(*lhs), eval_expr(*rhs));

            // If either operand is a table, do a metamethod lookup.
            if let Value::Table(_lhs) = lhs {
                todo!()
            } else if let Value::Table(_rhs) = rhs {
                todo!()
            }

            match op {
                BinaryOp::Add => Value::Number(lhs.as_number() + rhs.as_number()),
                BinaryOp::Sub => Value::Number(lhs.as_number() - rhs.as_number()),
                BinaryOp::Mul => Value::Number(lhs.as_number() * rhs.as_number()),
                BinaryOp::Div => Value::Number(lhs.as_number() / rhs.as_number()),
                BinaryOp::Mod => Value::Number(lhs.as_number() % rhs.as_number()),
                BinaryOp::Pow => Value::Number(lhs.as_number().powf(rhs.as_number())),
                BinaryOp::Concat => Value::String(lhs.as_string() + &rhs.as_string()),
                BinaryOp::And => if lhs.as_bool() { rhs } else { lhs },
                BinaryOp::Or => if lhs.as_bool() { lhs } else { rhs },
                BinaryOp::EQ => Value::Bool(lhs == rhs),
                BinaryOp::NEQ => Value::Bool(lhs != rhs),
                BinaryOp::GTE => Value::Bool(lhs >= rhs),
                BinaryOp::LTE => Value::Bool(lhs <= rhs),
                BinaryOp::GT => Value::Bool(lhs > rhs),
                BinaryOp::LT => Value::Bool(lhs < rhs),
            }
        }

        Expr::UnaryOp(op, expr) => {
            let value = eval_expr(*expr);

            // Handle potential metamethod lookup.
            if let Value::Table(_table) = value {
                todo!()
            }

            match op {
                UnaryOp::Not => Value::Bool(!value.as_bool()),
                UnaryOp::Neg => Value::Number(-value.as_number()),
                UnaryOp::Len => Value::Number(value.length() as f64),
            }
        }

        Expr::Vararg => todo!(),
        Expr::Name(_name) => todo!(),
        Expr::Index(_table_path, _index) => todo!(),
        Expr::Call(_function_path, _args) => todo!(),
        Expr::Member(_table_path, _name) => todo!(),
        Expr::Method(_table_path, _name) => todo!(),
    }
}

pub fn interpret() -> error::Result<'static, ()> {
    Ok(())
}
