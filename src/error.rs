#[derive(Clone, Debug)]
pub enum Type<'a> {
    UnexpectedEOF,
    UnexpectedChar(char),
    MalformedNumber,
    InvalidEscapeSequence(&'a [char]),
    UnexpectedNewlineInString,
    InvalidExpressionInAssignment,
    BreakNotInsideLoop,
    VarargOutsideOfVarargFunction,
    IndexNonTableValue,
}

pub use Type::*;

#[derive(Clone, Debug)]
pub struct Error<'a> {
    pub ty: Type<'a>,
    pub line: usize,
}

impl<'a> Error<'a> {
    pub fn new(ty: Type<'a>) -> Self {
        Self {
            ty,
            line: 0,
        }
    }
}

pub type Result<'a, T> = std::result::Result<T, Error<'a>>;
