use std::{fmt::Debug, io::Write};

use colored::Colorize;

use crate::{
    functions::FuncID,
    lexer::{Position, TokenType},
    symbols::SymbolID,
    types::{TypeHandler, TypeID, TypedNode, TypedStatement},
};

pub type ResResult<T> = Result<T, ResError>;

pub enum Severity {
    Error,
    Warning,
    Info,
}

impl Debug for Severity {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Severity::Error => write!(f, "{}", " ERROR ".bold().white().on_red()),
            Severity::Warning => write!(f, "{}", " WARNING ".bold().black().on_yellow()),
            Severity::Info => write!(f, "{}", " INFO ".bold().on_blue()),
        }
    }
}

pub struct ResError {
    pub kind: ResErrorKind,
    pub position: Option<Position>,
    pub severity: Severity,
}

impl ResError {
    pub fn new_err(kind: ResErrorKind, position: Position) -> Self {
        Self {
            kind,
            position: Some(position),
            severity: Severity::Error,
        }
    }
    pub fn new_warn(kind: ResErrorKind, position: Position) -> Self {
        Self {
            kind,
            position: Some(position),
            severity: Severity::Warning,
        }
    }
}

pub fn unwrap_print<T>(res: ResResult<T>, file_contents: &String, type_handler: &TypeHandler) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            print_err(e, file_contents, type_handler);
            std::process::exit(-1);
        }
    }
}
pub fn print_err(err: ResError, file_contents: &String, type_handler: &TypeHandler) {
    if let Some(pos) = err.position {
        print!("{:?} at {}:", err.severity, pos);
    } else {
        print!("{:?}:", err.severity);
    }
    if let Ok(s) = err.kind.with_handlers(type_handler, err.position) {
        println!(" {}", s.bold());
    } else {
        println!(" {}", format!("{:?}", err.kind).bold());
    }
    if let Some(pos) = err.position {
        let lines: Vec<&str> = file_contents.lines().collect();
        let start = (pos.from.0 as isize - 2).max(0) as usize;
        let end = pos.to.0 + 1;

        let buffer = lines[start..end].join("\n");
        let mut line = start;
        let mut offset = 0;
        for c in buffer.chars() {
            if c == '\n' {
                line += 1;
                offset = 0;
            }
            if line >= (pos.from.0 - 1)
                && line <= (pos.to.0 - 1)
                && offset >= pos.from.1
                && offset <= pos.to.1 - 1
            {
                print!("{}", c.to_string().red());
            } else {
                print!("{}", c.to_string().italic());
            }
            offset += 1;
        }
        println!();
    }
}

impl Debug for ResError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(pos) = self.position {
            writeln!(f, "{:?} at {}:", self.severity, pos)?;
        } else {
            writeln!(f, "{:?}:", self.severity)?;
        }
        writeln!(f, "{:?}", self.kind)
    }
}

pub enum ResErrorKind {
    UnknownVariable(String),
    UnknownType(String),
    UnknownFunction(String),
    TypeMismatch(TypeID, TypeID),
    ExpectedNumerical(TypeID),
    ExpectedVariable(String, FuncID),
    ExpectedFunction(String, SymbolID),
    NoTypeInfo(TypeID, TypeID),
    NotInferableNode,
    NotInferableStmt,
    ExpectedInitializerList,
    ExpectedTypeIdentifier,
    ExpectedIdentifier,
    ExpectedExpression,
    UnexpectedEOF,
    UnescapedParenthesis,
    TokenToOperator(TokenType),
    ExpectedSemicolon,
}

impl ResErrorKind {
    fn with_handlers(&self, th: &TypeHandler, pos: Option<Position>) -> ResResult<String> {
        Ok(match self {
            Self::UnknownType(ident) => format!("cannot find type {} in this scope", ident),
            Self::UnknownVariable(ident) => {
                format!("cannot find variable {} in this scope", ident)
            }
            Self::UnknownFunction(ident) => {
                format!("cannot find function {} in this scope", ident)
            }
            Self::TypeMismatch(exp, got) => format!(
                "expected type {}, got {}",
                th.get(exp, pos)?.identifier.blue(),
                th.get(got, pos)?.identifier.blue()
            ),
            Self::ExpectedNumerical(got) => format!(
                "expected numerical type, got {}",
                th.get(got, pos)?.identifier.blue()
            ),
            Self::ExpectedVariable(ident, id) => {
                format!("expected variable {}, found function with {:?}", ident, id)
            }
            Self::ExpectedFunction(ident, id) => {
                format!("expected function {}, found variable with {:?}", ident, id)
            }
            Self::NoTypeInfo(t1, t2) => {
                format!("not possible to infer types between {:?} and {:?}", t1, t2)
            }
            el => format!("{:?}", el),
        })
    }
}

impl Debug for ResErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownType(ident) => write!(f, "cannot find type {} in this scope", ident),
            Self::UnknownVariable(ident) => {
                write!(f, "cannot find variable {} in this scope", ident)
            }
            Self::UnknownFunction(ident) => {
                write!(f, "cannot find function {} in this scope", ident)
            }
            Self::TypeMismatch(exp, got) => write!(f, "expected type {:?}, got {:?}", exp, got),
            Self::ExpectedNumerical(got) => write!(f, "expected numerical type, got {:?}", got),
            Self::ExpectedVariable(ident, id) => write!(
                f,
                "expected variable {}, found function with {:?}",
                ident, id
            ),
            Self::ExpectedFunction(ident, id) => write!(
                f,
                "expected function {}, found variable with {:?}",
                ident, id
            ),
            Self::NoTypeInfo(t1, t2) => write!(
                f,
                "not possible to infer types between {:?} and {:?}",
                t1, t2
            ),
            Self::NotInferableNode => {
                write!(f, "expression cant be infered")
            }
            Self::NotInferableStmt => {
                write!(f, "statement cant be infered")
            }
            Self::ExpectedInitializerList => {
                write!(f, "expected initializer list")
            }
            Self::ExpectedTypeIdentifier => {
                write!(f, "expected type identifier")
            }
            Self::ExpectedIdentifier => {
                write!(f, "expected identifier")
            }
            Self::UnexpectedEOF => {
                write!(f, "unexpected end of file")
            }
            Self::UnescapedParenthesis => {
                write!(f, "unescaped parenthesis")
            }
            Self::ExpectedExpression => {
                write!(f, "expected expression")
            }
            Self::TokenToOperator(token) => {
                write!(f, "cant convert token {:?} to operator", token)
            }
            Self::ExpectedSemicolon => {
                write!(f, "expected semicolon")
            }
        }
    }
}

pub struct SSAError {
    kind: SSAErrorKind,
    position: Position,
}

pub enum SSAErrorKind {
    UnknownStackSize,
}
