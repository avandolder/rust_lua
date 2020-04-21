use std::iter::Peekable;

use crate::error::Result;
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
                Some(token::End) | Some(token::Until) | Some(token::ElseIf) | Some(token::Else) => break,
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
            self.parse_block();
        }

        if let Some(token::Else) = self.peek_type() {
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

    fn parse_table(&mut self) {
        self.expect(token::LBrace);

        while self.peek_type().unwrap() != token::RBrace {
            self.parse_field();

            if let Some(token::Comma) | Some(token::Semicolon) = self.peek_type() {
                self.consume();
            } else {
                break;
            }
        }

        self.expect(token::RBrace);
    }

    fn parse_field(&mut self) {
        match self.peek_type().unwrap() {
            token::LBracket => {
                self.consume();
                self.parse_expression();
                self.expect(token::RBracket);
                self.expect(token::Assign);
                self.parse_expression();
            }
            token::Name => {
                self.consume();
                self.expect(token::Assign);
                self.parse_expression();
            }
            _ => self.parse_expression(),
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
            } if let Some(token::Name) = self.peek_type() {
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

    fn parse_prefixexp(&mut self) {
        match self.consume().ty {
            token::Name => (),
            token::LParen => {
                self.parse_expression();
                self.expect(token::RParen);
            }
            _ => panic!(),
        }
        self.parse_var_or_funccall();
    }

    fn parse_var_or_funccall(&mut self) {
        match self.peek_type().unwrap() {
            token::LParen | token::LBrace | token::Str => todo!("args"),
            token::LBracket => {
                self.consume();
                self.parse_expression();
                self.expect(token::RBracket);
            }
            token::Period => {
                self.consume();
                self.expect(token::Name);
            }
            token::Colon => {
                self.consume();
                self.expect(token::Name);
                todo!("args")
            }
            _ => return,
        }

        self.parse_var_or_funccall();
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
        let tok = self.peek().unwrap();
        match tok.ty {
            token::Nil
            | token::True
            | token::False
            | token::Num
            | token::Str
            | token::Vararg => {
                self.consume();
            }
            token::Function => {
                self.consume();
                self.parse_funcbody();
            }
            token::LBrace => self.parse_table(),
            // Match prefix operators.
            t => match unary_precedence(t) {
                Some(op) => {
                    self.consume();
                    self.pratt_parse(op);
                }
                None => self.parse_prefixexp(),
            },
        }

        while let Some(op) = self.peek_type() {
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
