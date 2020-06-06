use std::convert::TryInto;
use std::iter::Peekable;

use crate::ast::{Expr, Field, FunctionArity, FunctionType, Name, Stmt};
use crate::error::LuaResult;
use crate::token::{self, Token};

struct Parser<'a, I: Iterator<Item = LuaResult<Token<'a>>>> {
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = LuaResult<Token<'a>>>> Parser<'a, I> {
    fn peek(&mut self) -> Option<Token<'a>> {
        self.tokens.peek().and_then(|result| result.clone().ok())
    }

    fn peek_type(&mut self) -> Option<token::Type> {
        self.peek().map(|tok| tok.ty)
    }

    fn consume(&mut self) -> Token {
        self.tokens
            .next()
            .expect("Unexpected end of input while parsing.")
            .expect("Scanning error")
    }

    fn expect(&mut self, expected: token::Type) -> Token {
        match self.peek_type() {
            Some(ty) if ty == expected => self.consume(),
            Some(_) => panic!("Expected {:?}, got {}", expected, self.consume()),
            None => panic!("End of input, expecting {:?}", expected),
        }
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];
        loop {
            match self.peek_type() {
                Some(token::Return) => {
                    self.consume();
                    let exprs = self.parse_expression_list();
                    stmts.push(Stmt::Return(exprs));
                    break;
                }
                Some(token::Break) => {
                    self.consume();
                    stmts.push(Stmt::Break);
                    break;
                }
                Some(token::End)
                | Some(token::Until)
                | Some(token::ElseIf)
                | Some(token::Else)
                | None =>
                    break,
                Some(_) => stmts.push(self.parse_statement()),
            }

            if let Some(token::Semicolon) = self.peek_type() {
                self.consume();
            }
        }
        stmts
    }

    fn parse_statement(&mut self) -> Stmt {
        match self.peek_type().unwrap() {
            token::Do => {
                self.consume();
                let stmts = self.parse_block();
                self.expect(token::End);
                Stmt::Block(stmts)
            }
            token::While => self.parse_while(),
            token::Repeat => self.parse_repeat(),
            token::If => self.parse_if(),
            token::For => self.parse_for(),
            token::Function => self.parse_function(),
            token::Local => self.parse_local(),
            token::Name | token::LParen => {
                // This call will either parse a variable assignment statement
                // (potentially a list assignment) or a function call statement.
                let expr = self.parse_prefixexp();

                if let Some(token::Comma) | Some(token::Assign) = self.peek_type() {
                    let mut lhs = vec![expr];
                    while let Some(token::Comma) = self.peek_type() {
                        lhs.push(self.parse_prefixexp());
                    }

                    self.expect(token::Assign);
                    let rhs = self.parse_expression_list();
                    Stmt::Assign(lhs, rhs)
                } else if let Expr::Call(fexpr, args) = expr {
                    Stmt::Call(*fexpr, args)
                } else {
                    panic!()
                }
            }
            _ => panic!(),
        }
    }

    fn parse_while(&mut self) -> Stmt {
        self.expect(token::While);
        let cond = self.parse_expression();
        self.expect(token::Do);
        let body = self.parse_block();
        self.expect(token::End);
        Stmt::While(cond, body)
    }

    fn parse_repeat(&mut self) -> Stmt {
        self.expect(token::Repeat);
        let body = self.parse_block();
        self.expect(token::Until);
        let cond = self.parse_expression();
        Stmt::Until(cond, body)
    }

    fn parse_if(&mut self) -> Stmt {
        self.expect(token::If);
        let cond = self.parse_expression();
        self.expect(token::Then);
        let body = self.parse_block();
        Stmt::If(cond, body, self.parse_elseif())
    }

    fn parse_elseif(&mut self) -> Vec<Stmt> {
        match self.consume().ty {
            token::ElseIf => {
                let cond = self.parse_expression();
                self.expect(token::Then);
                let body = self.parse_block();
                vec![Stmt::If(cond, body, self.parse_elseif())]
            }
            token::Else => {
                let body = self.parse_block();
                self.expect(token::End);
                body
            }
            token::End => vec![],
            _ => panic!(),
        }
    }

    fn parse_for(&mut self) -> Stmt {
        self.expect(token::For);
        let name = self.expect(token::Name).try_into().unwrap();

        match self.consume().ty {
            token::Comma => {
                let mut names = vec![name];
                names.extend(self.parse_name_list());
                self.expect(token::In);
                let exprs = self.parse_expression_list();

                self.expect(token::Do);
                let body = self.parse_block();
                self.expect(token::End);

                Stmt::ForIn(names, exprs, body)
            }
            token::Assign => {
                let start = Box::new(self.parse_expression());
                self.expect(token::Comma);
                let end = Box::new(self.parse_expression());

                let step = if let Some(token::Comma) = self.peek_type() {
                    self.consume();
                    Some(self.parse_expression())
                } else {
                    None
                };

                self.expect(token::Do);
                let body = self.parse_block();
                self.expect(token::End);

                Stmt::For(name, start, end, step, body)
            }
            _ => panic!(),
        }
    }

    fn parse_function(&mut self) -> Stmt {
        self.expect(token::Function);
        let (ftype, name) = self.parse_funcname();
        let (params, arity, body) = self.parse_funcbody();
        Stmt::Function(ftype, name, params, arity, body)
    }

    fn parse_funcbody(&mut self) -> (Vec<Name>, FunctionArity, Vec<Stmt>) {
        let (params, arity) = self.parse_parameter_list();
        let body = self.parse_block();
        self.expect(token::End);
        (params, arity, body)
    }

    fn parse_funcname(&mut self) -> (FunctionType, Expr) {
        let mut name = self.expect(token::Name).try_into().unwrap();

        while let Some(token::Period) = self.peek_type() {
            self.consume();
            name = Expr::Member(Box::new(name), self.expect(token::Name).try_into().unwrap());
        }

        if let Some(token::Colon) = self.peek_type() {
            self.consume();
            name = Expr::Member(Box::new(name), self.expect(token::Name).try_into().unwrap());
            (FunctionType::Method, name)
        } else {
            (FunctionType::Static, name)
        }
    }

    fn parse_local(&mut self) -> Stmt {
        self.expect(token::Local);

        if let Some(token::Function) = self.peek_type() {
            self.consume();
            let name = self.expect(token::Name).try_into().unwrap();
            let (params, arity, body) = self.parse_funcbody();
            Stmt::LocalFunction(name, params, arity, body)
        } else {
            let names = self.parse_name_list();
            let exprs = if let Some(token::Assign) = self.peek_type() {
                self.consume();
                self.parse_expression_list()
            } else {
                vec![]
            };
            Stmt::LocalAssign(names, exprs)
        }
    }

    fn parse_table(&mut self) -> Expr {
        let mut fields = vec![];

        self.expect(token::LBrace);
        while self.peek_type().unwrap() != token::RBrace {
            fields.push(self.parse_field());

            if let Some(token::Comma) | Some(token::Semicolon) = self.peek_type() {
                self.consume();
            } else {
                break;
            }
        }
        self.expect(token::RBrace);

        Expr::Table(fields)
    }

    fn parse_field(&mut self) -> Field {
        match self.peek_type().unwrap() {
            token::LBracket => {
                self.consume();
                let key = self.parse_expression();
                self.expect(token::RBracket);
                self.expect(token::Assign);
                let value = self.parse_expression();
                Field::Pair(key, value)
            }
            token::Name => {
                let key = self.consume().try_into().unwrap();
                self.expect(token::Assign);
                let value = self.parse_expression();
                Field::Pair(key, value)
            }
            _ => Field::Single(self.parse_expression()),
        }
    }

    fn parse_name_list(&mut self) -> Vec<Name> {
        let mut names = vec![self.expect(token::Name).try_into().unwrap()];
        while let Some(token::Comma) = self.peek_type() {
            self.consume();
            names.push(self.expect(token::Name).try_into().unwrap());
        }
        names
    }

    fn parse_parameter_list(&mut self) -> (Vec<Name>, FunctionArity) {
        self.expect(token::LParen);

        let mut params = vec![];
        loop {
            if let Some(token::Vararg) = self.peek_type() {
                self.consume();
                return (params, FunctionArity::Variable);
            } else if let Some(token::Name) = self.peek_type() {
                params.push(self.consume().try_into().unwrap());
            }

            if let Some(token::Comma) = self.peek_type() {
                self.consume();
                continue;
            }

            break;
        }

        self.expect(token::RParen);
        (params, FunctionArity::Fixed)
    }

    fn parse_prefixexp(&mut self) -> Expr {
        let e = match self.peek_type().unwrap() {
            token::Name => self.consume().try_into().unwrap(),
            token::LParen => {
                self.consume();
                let e = self.parse_expression();
                self.expect(token::RParen);
                e
            }
            _ => panic!(),
        };
        self.parse_var_or_funccall(e)
    }

    fn parse_var_or_funccall(&mut self, e: Expr) -> Expr {
        let e = match self.peek_type() {
            Some(token::LParen) | Some(token::LBrace) | Some(token::Str) => {
                let args = self.parse_arguments();
                Expr::Call(Box::new(e), args)
            }
            Some(token::LBracket) => {
                self.consume();
                let index = self.parse_expression();
                self.expect(token::RBracket);
                Expr::Index(Box::new(e), Box::new(index))
            }
            Some(token::Period) => {
                self.consume();
                let name = self.expect(token::Name).try_into().unwrap();
                Expr::Member(Box::new(e), name)
            }
            Some(token::Colon) => {
                self.consume();
                let name = self.expect(token::Name).try_into().unwrap();
                let method = Expr::Method(Box::new(e), name);
                Expr::Call(Box::new(method), self.parse_arguments())
            }
            _ => return e,
        };
        self.parse_var_or_funccall(e)
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        match self.peek_type().unwrap() {
            token::Str => vec![self.consume().try_into().unwrap()],
            token::LBrace => vec![self.parse_table()],
            token::LParen => {
                self.consume();
                let args = match self.peek_type() {
                    Some(token::RParen) => vec![],
                    _ => self.parse_expression_list(),
                };
                self.expect(token::RParen);
                args
            }
            _ => panic!(),
        }
    }

    fn parse_expression_list(&mut self) -> Vec<Expr> {
        let mut exprs = vec![self.parse_expression()];

        while let Some(token::Comma) = self.peek_type() {
            self.consume();
            exprs.push(self.parse_expression());
        }

        exprs
    }

    fn parse_expression(&mut self) -> Expr {
        self.pratt_parse(0)
    }

    fn pratt_parse(&mut self, min_prec: i32) -> Expr {
        // Pratt expression parser inspired by
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html.

        // Consume the left-hand side of the expression.
        let tok = self.peek().unwrap();
        let mut lhs = match tok.ty {
            token::Nil | token::True | token::False | token::Num | token::Str | token::Vararg => {
                self.consume();
                tok.try_into().unwrap()
            }
            token::Function => {
                self.consume();
                let (params, arity, body) = self.parse_funcbody();
                Expr::Function(params, arity, body)
            }
            token::LBrace => self.parse_table(),
            // Match prefix operators.
            op => match unary_precedence(op) {
                Some(prec) => {
                    let op = self.consume().ty.try_into().unwrap();
                    Expr::UnaryOp(op, Box::new(self.pratt_parse(prec)))
                }
                None => self.parse_prefixexp(),
            },
        };

        while let Some(op) = self.peek_type() {
            if let Some((l_prec, r_prec)) = binary_precedence(op) {
                if l_prec < min_prec {
                    break;
                }

                let op = self.consume().ty.try_into().unwrap();
                lhs = Expr::BinaryOp(op, Box::new(lhs), Box::new(self.pratt_parse(r_prec)));
            } else {
                break;
            }
        }

        lhs
    }
}

fn unary_precedence(op: token::Type) -> Option<i32> {
    Some(match op {
        token::Sub | token::Hash | token::Not => 8,
        _ => return None,
    })
}

fn binary_precedence(op: token::Type) -> Option<(i32, i32)> {
    Some(match op {
        token::Pow => (9, 8),
        token::Mul | token::Div | token::Mod => (7, 8),
        token::Add | token::Sub => (6, 7),
        token::Concat => (5, 4),
        token::LT | token::GT | token::LTE | token::GTE | token::EQ | token::NEQ => (3, 4),
        token::And => (2, 3),
        token::Or => (1, 2),
        _ => return None,
    })
}

pub fn parse<'a>(
    tokens: Peekable<impl Iterator<Item = LuaResult<Token<'a>>>>
) -> LuaResult<Vec<Stmt>> {
    let mut parser = Parser { tokens };
    let stmts = parser.parse_block();
    assert!(parser.tokens.next().is_none());
    Ok(stmts)
}

pub fn parse_expression<'a>(
    tokens: Peekable<impl Iterator<Item = LuaResult<Token<'a>>>>
) -> Expr {
    let mut parser = Parser { tokens };
    parser.parse_expression()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_token_stream(
        tokens: &'static [Token],
    ) -> Peekable<impl Iterator<Item = LuaResult<Token<'static>>>> {
        tokens.iter().map(|tok| Ok(tok.clone())).peekable()
    }

    macro_rules! parse_tokens {
        (($t:expr, $raw:expr)) => {
            Token { ty: $t, line: 0, raw: &$raw }
        };

        ($t:expr) => {
            Token { ty: $t, line: 0, raw: &[] }
        };

        ($($t:tt),*) => {
            {
                let mut parser = Parser { tokens: create_token_stream(&[$(parse_tokens!($t),)*]) };
                parser.parse_expression().to_string()
            }
        };
    }

    #[test]
    fn test_expressions() {
        use token::Type::*;

        assert_eq!(parse_tokens!(True, And, False), "(true and false)");

        assert_eq!(
            parse_tokens!(Function, LParen, RParen, End, Add, Nil),
            "(function()\nend + nil)"
        );

        assert_eq!(
            parse_tokens!(
                Sub,
                (Num, ['1']),
                Add,
                (Num, ['2']),
                Pow,
                LParen,
                (Num, ['3']),
                Mul,
                (Num, ['4']),
                RParen
            ),
            "(-1 + (2 ^ (3 * 4)))"
        );
    }

    #[test]
    fn test_right_associativity() {
        use token::Type::*;

        assert_eq!(
            parse_tokens!((Num, ['2']), Pow, (Num, ['2']), Pow, (Num, ['2'])),
            "(2 ^ (2 ^ 2))",
        );

        assert_eq!(
            parse_tokens!(
                (Str, ['"', '"']),
                Concat,
                (Str, ['"', '"']),
                Concat,
                (Str, ['"', '"'])
            ),
            "(\"\" .. (\"\" .. \"\"))",
        );
    }

    #[test]
    fn test_function_call() {
        use token::Type::*;

        assert_eq!(
            parse_tokens!(
                (Name, ['a']),
                Period,
                (Name, ['b']),
                LParen,
                (Num, ['1']),
                Comma,
                (Str, ['"', '"']),
                Comma,
                LBrace,
                RBrace,
                RParen
            ),
            "a.b(1, \"\", {})"
        );
    }

    #[test]
    fn test_table_constructor() {
        use token::Type::*;

        assert_eq!(
            parse_tokens!(
                LBrace,
                (Num, ['1']),
                Comma,
                (Num, ['2']),
                Semicolon,
                (Name, ['a']),
                Assign,
                (Num, ['1']),
                Comma,
                LBracket,
                (Str, ['"', '.', '"']),
                RBracket,
                Assign,
                True,
                RBrace
            ),
            "{1, 2, a = 1, [\".\"] = true}"
        )
    }
}
