#[derive(Clone, Debug)]
pub enum Type<'a> {
    UnexpectedEOF,
    UnexpectedChar(char),
    MalformedNumber,
    InvalidEscapeSequence(&'a [char]),
    UnexpectedNewlineInString,
    InvalidExpressionInAssignment,
}

impl<'a> Type<'a> {
    pub fn as_error(self) -> Error<'a> {
        Error {
            ty: self,
            line: 0,
        }
    }
}

pub use Type::*;

#[derive(Clone, Debug)]
pub struct Error<'a> {
    pub ty: Type<'a>,
    pub line: usize,
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
