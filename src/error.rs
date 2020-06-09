use crate::token::{self, Token};

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
    ExpectingToken(token::Type, Token<'static>),
    UnexpectedEndOfInput(token::Type),
    InvalidBinaryOp(token::Type),
    InvalidUnaryOp(token::Type),
    InvalidName(String),
    InvalidStatement,
    TokenIsNotValidExpression(String),
    ExpectedOneOf(Vec<token::Type>, token::Type),
    ParseNumberError,
    ValueNotValidNumber,
    ValueNotValidString,
    ValueNotCallable,
    ValueHasNoLength,
    UnresolvableExpression,
    InvalidArguments,
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
