use std::convert::TryInto;
use std::iter::Peekable;

use crate::error::Result;
use crate::ast::{Expr, Field};
use crate::token::{self, Token};

struct Parser<'a, I: Iterator<Item = Result<'a, Token<'a>>>> {
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Result<'a, Token<'a>>>> Parser<'a, I> {
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

    fn parse_block(&mut self) {
        loop {
            match self.peek_type() {
                Some(token::Return) => {
                    self.consume();
                    self.parse_expression_list();
                    break;
                }
                Some(token::Break) => {
                    self.consume();
                    break;
                }
                Some(token::End) | Some(token::Until) | Some(token::ElseIf) | Some(token::Else) => {
                    break;
                }
                Some(_) => self.parse_statement(),
                None => break,
            }

            if let Some(token::Semicolon) = self.peek_type() {
                self.consume();
            }
        }
    }

    fn parse_statement(&mut self) {
        match self.peek_type().unwrap() {
            token::Do => {
                self.consume();
                self.parse_block();
                self.expect(token::End);
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
                self.parse_prefixexp();

                while let Some(token::Comma) = self.peek_type() {
                    self.parse_prefixexp();
                }

                if let Some(token::Assign) = self.peek_type() {
                    self.consume();
                    self.parse_expression_list();
                }
            }
            _ => panic!(),
        }
    }

    fn parse_while(&mut self) {
        self.expect(token::While);
        self.parse_expression();
        self.expect(token::Do);
        self.parse_block();
        self.expect(token::End);
    }

    fn parse_repeat(&mut self) {
        self.expect(token::Repeat);
        self.parse_block();
        self.expect(token::Until);
        self.parse_expression();
    }

    fn parse_if(&mut self) {
        self.expect(token::If);
        self.parse_expression();
        self.expect(token::Then);
        self.parse_block();

        while let Some(token::ElseIf) = self.peek_type() {
            self.consume();
            self.parse_expression();
            self.expect(token::Then);
            self.parse_block();
        }

        if let Some(token::Else) = self.peek_type() {
            self.consume();
            self.parse_block();
        }

        self.expect(token::End);
    }

    fn parse_for(&mut self) {
        self.expect(token::For);
        self.expect(token::Name);

        match self.consume().ty {
            token::Comma => {
                self.parse_name_list();
                self.expect(token::In);
                self.parse_expression_list();
            }
            token::Assign => {
                self.parse_expression();
                self.expect(token::Comma);
                self.parse_expression();

                if let Some(token::Comma) = self.peek_type() {
                    self.consume();
                    self.parse_expression();
                }
            }
            _ => panic!(),
        }

        self.expect(token::Do);
        self.parse_block();
        self.expect(token::End);
    }

    fn parse_function(&mut self) {
        self.expect(token::Function);
        self.parse_funcname();
        self.parse_funcbody();
    }

    fn parse_funcbody(&mut self) {
        self.parse_parameter_list();
        self.parse_block();
        self.expect(token::End);
    }

    fn parse_funcname(&mut self) {
        self.expect(token::Name);

        while let Some(token::Period) = self.peek_type() {
            self.consume();
            self.expect(token::Name);
        }

        if let Some(token::Colon) = self.peek_type() {
            self.consume();
            self.expect(token::Name);
        }
    }

    fn parse_local(&mut self) {
        self.expect(token::Local);

        if let Some(token::Function) = self.peek_type() {
            self.consume();
            self.expect(token::Name);
            self.parse_funcbody();
            return;
        }

        self.parse_name_list();
        self.expect(token::Assign);
        self.parse_expression_list();
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

    fn parse_name_list(&mut self) {
        self.expect(token::Name);

        while let Some(token::Comma) = self.peek_type() {
            self.consume();
            self.expect(token::Name);
        }
    }

    fn parse_parameter_list(&mut self) {
        self.expect(token::LParen);

        loop {
            if let Some(token::Vararg) = self.peek_type() {
                self.consume();
                break;
            } else if let Some(token::Name) = self.peek_type() {
                self.consume();
            }

            if let Some(token::Comma) = self.peek_type() {
                self.consume();
                continue;
            }

            break;
        }

        self.expect(token::RParen);
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
                self.parse_funcbody();
                Expr::Function
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

pub fn parse<'a>(tokens: Peekable<impl Iterator<Item = Result<'a, Token<'a>>>>) {
    let mut parser = Parser { tokens };
    parser.parse_block();
    assert!(parser.tokens.next().is_none());
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_token_stream(
        tokens: &'static [Token],
    ) -> Peekable<impl Iterator<Item = Result<'static, Token<'static>>>> {
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
            "(function + nil)"
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
            parse_tokens!((Str, ['"', '"']), Concat, (Str, ['"', '"']), Concat, (Str, ['"', '"'])),
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
