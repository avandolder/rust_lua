#[derive(Clone, Debug)]
pub enum Type {
    UnexpectedEOF,
    UnexpectedChar(char),
    MalformedNumber,
    InvalidEscapeSequence(String),
    UnexpectedNewlineInString,
    InvalidExpressionInAssignment,
    BreakNotInsideLoop,
    VarargOutsideOfVarargFunction,
    IndexNonTableValue,
    ExpectedEOF,
}

pub use Type::*;

#[derive(Clone, Debug)]
pub struct LuaError  {
    pub ty: Type ,
    pub line: usize,
}

impl LuaError {
    pub fn new<T>(ty: Type) -> LuaResult<T> {
        Err(Self {
            ty,
            line: 0,
        })
    }
}

pub type LuaResult<T> = std::result::Result<T, LuaError>;
