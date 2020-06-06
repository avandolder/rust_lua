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
pub struct LuaError<'a> {
    pub ty: Type<'a>,
    pub line: usize,
}

impl<'a> LuaError<'a> {
    pub fn new(ty: Type<'a>) -> Self {
        Self {
            ty,
            line: 0,
        }
    }
}

pub type LuaResult<'a, T> = std::result::Result<T, LuaError<'a>>;
