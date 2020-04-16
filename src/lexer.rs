use std::iter;

use crate::token::{self, Token, KEYWORDS};

struct Lexer<'a> {
    src: &'a [char],
    line: usize,
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> Option<Token<'a>> {
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
            [c, ..] if c.is_ascii_digit() => self.scan_number(),
            ['.', c, ..] if c.is_ascii_digit() => self.scan_number(),

            ['"', ..] | ['\'', ..] => self.scan_string(),
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

            [c, ..] => panic!("Unexpected character '{}' encountered", c),
            [] => return None,
        };

        let raw = &self.src[..length];
        self.src = &self.src[length..];

        Some(Token {
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
        self.line += 1;
    }

    fn scan_name(&self) -> (token::Type, usize) {
        let name = self
            .src
            .iter()
            .take_while(|&&c| c.is_alphanumeric() || c == '_')
            .collect::<String>();

        let ty = KEYWORDS
            .get(name.as_str())
            .cloned()
            .unwrap_or(token::Name);
        (ty, name.len())
    }

    fn scan_string(&mut self) -> (token::Type, usize) {
        // Build up the closing sequence for the string from the opening sequence.
        let closer = self.src[0];

        let mut string = vec![];
        let chars = &self.src[1..];
        let mut size = 0;
        while chars[size] != closer {
            string.push(match chars[size] {
                '\\' => {
                    let c = match chars[size + 1] {
                        'a' => 7 as char,
                        'b' => 8 as char,
                        '\n' => {
                            self.line += 1;
                            '\n'
                        },
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        'v' => 11 as char,
                        '\\' => '\\',
                        '"' => '"',
                        '\'' => '\'',
                        d if d.is_ascii_digit() => todo!("Numerical escape sequences."),
                        _ => panic!("Invalid escape sequence in string."),
                    };
                    size += 2;
                    c as u8
                }
                '\n' => panic!("Unescaped newline in string literal."),
                c => {
                    size += 1;
                    c as u8
                }
            });
        }

        // Add 2 to account for opening and closing quotes.
        (token::Str, size + 2)
    }

    fn scan_number(&self) -> (token::Type, usize) {
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
                    panic!("Malformed number near {}");
                }
                length += self.scan_digits(length);
            }
            c if c.is_alphanumeric() => panic!("Malformed number"),
            _ => (),
        }

        (token::Num, length)
    }

    fn scan_digits(&self, offset: usize) -> usize {
        self.src[offset..]
            .iter()
            .take_while(|&&c| c.is_ascii_digit())
            .count()
    }
}

pub fn tokenize<'a>(src: &'a [char]) -> iter::Peekable<impl Iterator<Item = Token> + 'a> {
    let mut lexer: Lexer<'a> = Lexer { src, line: 1 };
    iter::from_fn(move || lexer.next_token()).peekable()
}
