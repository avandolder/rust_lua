use std::iter;

use crate::token::{Kind, Token, KEYWORDS};

struct Lexer<'a> {
    src: &'a [char],
    line: usize,
}

impl<'a> Lexer<'a> {
    fn next_token(&mut self) -> Option<Token> {
        use Kind::*;

        // Skip leading comments or whitespace.
        loop {
            match self.src {
                ['-', '-', ..] => self.skip_comment(),
                [c, ..] if c.is_ascii_whitespace() => self.skip_whitespace(),
                _ => break,
            }
        }

        let kind = match self.src {
            ['0', 'X', ..] | ['0', 'x', ..] => todo!("Scan hexadecimal number"),
            [c, ..] if c.is_ascii_digit() => self.scan_number(),
            ['.', c, ..] if c.is_ascii_digit() => self.scan_number(),

            ['"', ..] | ['\'', ..] => self.scan_string(),
            ['[', '[', ..] | ['[', '=', ..] => todo!("Scan long bracket string"),

            [c, ..] if c.is_alphabetic() || *c == '_' => self.scan_name(),

            ['=', '=', ..] => EQ,
            ['~', '=', ..] => NEQ,
            ['>', '=', ..] => GTE,
            ['<', '=', ..] => LTE,
            ['>', ..] => GT,
            ['<', ..] => LT,
            ['+', ..] => Add,
            ['-', ..] => Sub,
            ['/', ..] => Div,
            ['*', ..] => Mul,
            ['^', ..] => Pow,
            ['%', ..] => Mod,

            ['=', ..] => Assign,
            [':', ..] => Colon,
            [',', ..] => Comma,
            [';', ..] => Semicolon,
            ['#', ..] => Hash,
            ['.', '.', '.', ..] => Vararg,
            ['.', '.', ..] => Concat,
            ['.', ..] => Period,

            ['{', ..] => LBrace,
            ['}', ..] => RBrace,
            ['[', ..] => LBracket,
            [']', ..] => RBracket,
            ['(', ..] => LParen,
            [')', ..] => RParen,

            [c, ..] => panic!("Unexpected character '{}' encountered", c),
            [] => return None,
        };

        self.src = &self.src[kind.length()..];

        Some(Token {
            kind,
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

    fn scan_name(&self) -> Kind {
        let name = self
            .src
            .iter()
            .take_while(|&&c| c.is_alphanumeric() || c == '_')
            .collect::<String>();

        KEYWORDS
            .get(name.as_str())
            .cloned()
            .unwrap_or(Kind::Name(name))
    }

    fn scan_string(&self) -> Kind {
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
                        '\n' => '\n',
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
        Kind::Str(string, size + 2)
    }

    fn scan_number(&self) -> Kind {
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
            },
            c if c.is_alphanumeric() => panic!("Malformed number near {}", num),
            _ => (),
        }

        Kind::Num(num.parse::<f64>().unwrap(), num.len())
    }

    fn scan_digits(&self, offset: usize) -> String {
        self.src[offset..].iter().take_while(|&&c| c.is_ascii_digit()).collect::<String>()
    }
}

pub fn tokenize<'a>(src: &'a [char]) -> iter::Peekable<impl Iterator<Item = Token> + 'a> {
    let mut lexer: Lexer<'a> = Lexer { src, line: 1 };
    iter::from_fn(move || lexer.next_token()).peekable()
}
