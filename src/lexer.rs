use std::iter;

use crate::error::{self, LuaError, LuaResult};
use crate::token::{self, Token, KEYWORDS};

struct Lexer<'a> {
    src: &'a [char],
    line: usize,
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> LuaResult<Token<'a>> {
        // Skip leading comments or whitespace.
        loop {
            match self.src {
                ['-', '-', ..] => self.skip_comment(),
                [c, ..] if c.is_ascii_whitespace() => self.skip_whitespace(),
                _ => break,
            }
        }

        let (ty, length) = match self.src {
            ['0', 'X', ..] | ['0', 'x', ..] => todo!("Scan hexadecimal number"),
            [c, ..] if c.is_ascii_digit() => self.scan_number()?,
            ['.', c, ..] if c.is_ascii_digit() => self.scan_number()?,

            ['"', ..] | ['\'', ..] => self.scan_string()?,
            ['[', '[', ..] | ['[', '=', ..] => todo!("Scan long bracket string"),

            [c, ..] if c.is_alphabetic() || *c == '_' => self.scan_name(),

            ['=', '=', ..] => (token::EQ, 2),
            ['~', '=', ..] => (token::NEQ, 2),
            ['>', '=', ..] => (token::GTE, 2),
            ['<', '=', ..] => (token::LTE, 2),
            ['>', ..] => (token::GT, 1),
            ['<', ..] => (token::LT, 1),
            ['+', ..] => (token::Add, 1),
            ['-', ..] => (token::Sub, 1),
            ['/', ..] => (token::Div, 1),
            ['*', ..] => (token::Mul, 1),
            ['^', ..] => (token::Pow, 1),
            ['%', ..] => (token::Mod, 1),

            ['=', ..] => (token::Assign, 1),
            [':', ..] => (token::Colon, 1),
            [',', ..] => (token::Comma, 1),
            [';', ..] => (token::Semicolon, 1),
            ['#', ..] => (token::Hash, 1),
            ['.', '.', '.', ..] => (token::Vararg, 3),
            ['.', '.', ..] => (token::Concat, 2),
            ['.', ..] => (token::Period, 1),

            ['{', ..] => (token::LBrace, 1),
            ['}', ..] => (token::RBrace, 1),
            ['[', ..] => (token::LBracket, 1),
            [']', ..] => (token::RBracket, 1),
            ['(', ..] => (token::LParen, 1),
            [')', ..] => (token::RParen, 1),

            [c, ..] => {
                return Err(LuaError {
                    line: self.line,
                    ty: error::UnexpectedChar(*c),
                })
            }
            [] => (token::EOF, 0),
        };

        let raw = &self.src[..length];
        self.src = &self.src[length..];

        Ok(Token {
            ty,
            line: self.line,
            raw,
        })
    }

    fn skip_whitespace(&mut self) {
        let space_length = self
            .src
            .iter()
            .take_while(|&&c| {
                if c == '\n' {
                    self.line += 1;
                }
                c.is_ascii_whitespace()
            })
            .count();
        self.src = &self.src[space_length..];
    }

    fn skip_comment(&mut self) {
        let comment_length = self.src.iter().take_while(|&&c| c != '\n').count();
        self.src = &self.src[comment_length..];
    }

    fn scan_name(&self) -> (token::Type, usize) {
        let name = self
            .src
            .iter()
            .take_while(|&&c| c.is_alphanumeric() || c == '_')
            .collect::<String>();

        let ty = KEYWORDS.get(name.as_str()).cloned().unwrap_or(token::Name);
        (ty, name.len())
    }

    fn scan_string(&mut self) -> LuaResult<(token::Type, usize)> {
        let opening_symbol = self.src[0];
        let mut length = 1;
        loop {
            match self.src[length] {
                c if c == opening_symbol => {
                    length += 1;
                    break;
                }
                '\\' => {
                    match self.src[length + 1] {
                        'a' | 'b' | 'n' | 'r' | 't' | 'v' | '\\' | '"' | '\'' => (),
                        '\n' => self.line += 1,
                        d if d.is_ascii_digit() => (),
                        _ => {
                            return Err(LuaError {
                                ty: error::InvalidEscapeSequence(
                                    self.src[length..length + 2].iter().collect(),
                                ),
                                line: self.line,
                            })
                        }
                    }
                    length += 2;
                }
                '\n' => {
                    return Err(LuaError {
                        ty: error::UnexpectedNewlineInString,
                        line: self.line,
                    })
                }
                _ => length += 1,
            }
        }

        Ok((token::Str, length))
    }

    fn scan_number(&self) -> LuaResult<(token::Type, usize)> {
        let mut length = self.scan_digits(0);

        if self.src[length] == '.' {
            length += 1;
        }
        if self.src[length].is_ascii_digit() {
            length += self.scan_digits(length);
        }

        match self.src[length] {
            'e' | 'E' => {
                length += 1;
                if self.src[length] == '-' {
                    length += 1;
                }
                if !self.src[length].is_ascii_digit() {
                    return Err(LuaError {
                        ty: error::MalformedNumber,
                        line: self.line,
                    });
                }
                length += self.scan_digits(length);
            }
            c if c.is_alphanumeric() => {
                return Err(LuaError {
                    ty: error::MalformedNumber,
                    line: self.line,
                })
            }
            _ => (),
        }

        Ok((token::Num, length))
    }

    fn scan_digits(&self, offset: usize) -> usize {
        self.src[offset..]
            .iter()
            .take_while(|&&c| c.is_ascii_digit())
            .count()
    }
}

pub fn tokenize<'a>(
    src: &'a [char],
) -> iter::Peekable<impl Iterator<Item = LuaResult<Token>> + 'a> {
    let mut lexer: Lexer<'a> = Lexer { src, line: 1 };
    iter::from_fn(move || match lexer.next_token() {
        Ok(Token { ty: token::EOF, .. }) => None,
        result => Some(result),
    })
    .peekable()
}
