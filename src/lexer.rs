use std::iter;

use crate::token::{self, Token, KEYWORDS};

struct Lexer<'a> {
    src: &'a [char],
    line: usize,
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> Option<Token> {
        // Skip leading comments or whitespace.
        loop {
            match self.src {
                ['-', '-', ..] => self.skip_comment(),
                [c, ..] if c.is_ascii_whitespace() => self.skip_whitespace(),
                _ => break,
            }
        }

        let ty = match self.src {
            ['0', 'X', ..] | ['0', 'x', ..] => todo!("Scan hexadecimal number"),
            [c, ..] if c.is_ascii_digit() => self.scan_number(),
            ['.', c, ..] if c.is_ascii_digit() => self.scan_number(),

            ['"', ..] | ['\'', ..] => self.scan_string(),
            ['[', '[', ..] | ['[', '=', ..] => todo!("Scan long bracket string"),

            [c, ..] if c.is_alphabetic() || *c == '_' => self.scan_name(),

            ['=', '=', ..] => token::EQ,
            ['~', '=', ..] => token::NEQ,
            ['>', '=', ..] => token::GTE,
            ['<', '=', ..] => token::LTE,
            ['>', ..] => token::GT,
            ['<', ..] => token::LT,
            ['+', ..] => token::Add,
            ['-', ..] => token::Sub,
            ['/', ..] => token::Div,
            ['*', ..] => token::Mul,
            ['^', ..] => token::Pow,
            ['%', ..] => token::Mod,

            ['=', ..] => token::Assign,
            [':', ..] => token::Colon,
            [',', ..] => token::Comma,
            [';', ..] => token::Semicolon,
            ['#', ..] => token::Hash,
            ['.', '.', '.', ..] => token::Vararg,
            ['.', '.', ..] => token::Concat,
            ['.', ..] => token::Period,

            ['{', ..] => token::LBrace,
            ['}', ..] => token::RBrace,
            ['[', ..] => token::LBracket,
            [']', ..] => token::RBracket,
            ['(', ..] => token::LParen,
            [')', ..] => token::RParen,

            [c, ..] => panic!("Unexpected character '{}' encountered", c),
            [] => return None,
        };

        self.src = &self.src[ty.length()..];

        Some(Token {
            ty,
            line: self.line,
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

    fn scan_name(&self) -> token::Type {
        let name = self
            .src
            .iter()
            .take_while(|&&c| c.is_alphanumeric() || c == '_')
            .collect::<String>();

        KEYWORDS
            .get(name.as_str())
            .cloned()
            .unwrap_or(token::Name(name))
    }

    fn scan_string(&mut self) -> token::Type {
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
        token::Str(string, size + 2)
    }

    fn scan_number(&self) -> token::Type {
        let mut num = self.scan_digits(0);

        if self.src[num.len()] == '.' {
            num.push('.');
        }
        if self.src[num.len()].is_ascii_digit() {
            num.push_str(&self.scan_digits(num.len()));
        }

        match self.src[num.len()] {
            'e' | 'E' => {
                num.push('e');
                if self.src[num.len()] == '-' {
                    num.push('-');
                }
                if !self.src[num.len()].is_ascii_digit() {
                    panic!("Malformed number near {}", num);
                }
                num.push_str(&self.scan_digits(num.len()));
            }
            c if c.is_alphanumeric() => panic!("Malformed number near {}", num),
            _ => (),
        }

        token::Num(num.parse::<f64>().unwrap(), num.len())
    }

    fn scan_digits(&self, offset: usize) -> String {
        self.src[offset..]
            .iter()
            .take_while(|&&c| c.is_ascii_digit())
            .collect::<String>()
    }
}

pub fn tokenize<'a>(src: &'a [char]) -> iter::Peekable<impl Iterator<Item = Token> + 'a> {
    let mut lexer: Lexer<'a> = Lexer { src, line: 1 };
    iter::from_fn(move || lexer.next_token()).peekable()
}
