use std::iter::Peekable;

use crate::token::{self, Token};

struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    tokens: Peekable<I>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    fn peek(&mut self) -> Option<&Token> {
        self.tokens.peek()
    }

    fn peek_type(&mut self) -> Option<token::Type> {
        self.tokens.peek().map(|token| token.ty)
    }

    fn consume(&mut self) -> Token {
        self.tokens
            .next()
            .expect("Unexpected end of input while parsing.")
    }

    fn expect(&mut self, expected: token::Type) -> Token {
        match self.tokens.next() {
            Some(Token { ty, .. }) if ty == expected => self.consume(),
            _ => panic!(),
        }
    }

    fn parse_chunk(&mut self) {
        while let Some(_) = self.tokens.peek() {
            self.parse_statement();
        }
    }

    fn parse_statement(&mut self) {
        match self.consume().ty {
            token::Do => self.parse_block(token::End),
            token::While => self.parse_while(),
            token::Repeat => self.parse_repeat(),
            token::If => self.parse_if(),
            token::For => self.parse_for(),
            token::Function => self.parse_function(),
            token::Local => self.parse_local(),
            token::Name => todo!(),
            _ => panic!(),
        }
    }

    fn parse_block(&mut self, end_token: token::Type) {
        loop {
            match self.peek_type() {
                Some(token::Return) => {
                    self.parse_expression_list();
                    self.expect(end_token);
                    break;
                }
                Some(token::Break) => {
                    self.consume();
                    self.expect(end_token);
                    break;
                }
                Some(ty) if ty == end_token => {
                    self.consume();
                    break;
                }
                _ => self.parse_statement(),
            }
        }
    }

    fn parse_while(&mut self) {
        self.parse_expression();
        self.expect(token::Do);
        self.parse_block(token::End);
    }

    fn parse_repeat(&mut self) {
        self.parse_block(token::Until);
        self.parse_expression();
    }

    fn parse_if(&mut self) {
        todo!()
    }

    fn parse_for(&mut self) {
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
        self.parse_block(token::End);
    }

    fn parse_function(&mut self) {
        self.expect(token::Name);
        self.expect(token::LParen);

        if let Some(token::Name) = self.peek_type() {
            self.parse_name_list();
        }
        self.expect(token::RParen);
        self.parse_block(token::End);
    }

    fn parse_local(&mut self) {
        if let Some(token::Function) = self.peek_type() {
            self.consume();
            self.parse_function();
            return;
        }

        self.parse_name_list();
        self.expect(token::Assign);
        self.parse_expression_list();
    }

    fn parse_name_list(&mut self) {
        self.expect(token::Name);

        while let Some(token::Comma) = self.peek_type() {
            self.consume();
            self.expect(token::Name);
        }
    }

    fn parse_expression_list(&mut self) {
        self.parse_expression();

        while let Some(token::Comma) = self.peek_type() {
            self.consume();
            self.parse_expression();
        }
    }

    fn parse_expression(&mut self) {
        self.pratt_parse(0);
    }

    fn pratt_parse(&mut self, min_prec: i32) {
        // Pratt expression parser inspired by
        // https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html.

        // Consume the left-hand side of the expression.
        let tok = self.consume();
        match tok.ty {
            token::LParen => {
                self.pratt_parse(0);
                self.expect(token::RParen);
            }
            // Match prefix operators.
            op if unary_precedence(op).is_some() => {
                self.pratt_parse(unary_precedence(op).unwrap());
            }
            _ => panic!("Invalid token {} in expresion.", tok),
        }

        loop {
            let op = match self.peek_type() {
                Some(ty) => ty,
                None => break,
            };

            if let Some(l_prec) = index_precedence(op) {
                if l_prec < min_prec {
                    break;
                }

                self.consume();
                continue;
            }

            if let Some((l_prec, r_prec)) = binary_precedence(op) {
                if l_prec < min_prec {
                    break;
                }

                self.consume();
                self.pratt_parse(r_prec);
                continue;
            }

            break;
        }
    }
}

fn unary_precedence(op: token::Type) -> Option<i32> {
    Some(match op {
        token::Sub | token::Hash | token::Not => 8,
        _ => return None,
    })
}

fn index_precedence(op: token::Type) -> Option<i32> {
    Some(match op {
        token::LParen | token::LBracket => 10,
        _ => return None,
    })
}

fn binary_precedence(op: token::Type) -> Option<(i32, i32)> {
    Some(match op {
        token::Pow => (9, 8),
        token::Mul | token::Div | token::Mod => (7, 8),
        token::Add | token::Sub => (6, 7),
        token::Concat => (5, 6),
        token::LT | token::GT | token::LTE | token::GTE | token::EQ | token::NEQ => (4, 5),
        token::And => (3, 4),
        token::Or => (2, 3),
        _ => return None,
    })
}

pub fn parse<'a>(tokens: Peekable<impl Iterator<Item = Token<'a>>>) {
    let mut parser = Parser { tokens };
    parser.parse_chunk();
}
