use std::{fmt::Debug, io::Write};

use colored::Colorize;

use crate::{
    functions::FuncID,
    lexer::Position,
    symbols::SymbolID,
    types::{TypeHandler, TypeID},
};

pub type ResResult<T> = Result<T, ResError>;

pub struct ResError {
    kind: ResErrorKind,
    position: Position,
}

impl ResError {
    pub fn new(kind: ResErrorKind, position: Position) -> Self {
        Self { kind, position }
    }
}

pub fn unwrap_print<T>(res: ResResult<T>, file_contents: &String, type_handler: &TypeHandler) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            print!("{} at {}:", " ERROR ".bold().on_red(), e.position);
            if let Ok(s) = e.kind.with_handlers(type_handler, e.position) {
                println!("\t{}", s.bold());
            } else {
                println!("\t{}", format!("{:?}", e.kind).bold());
            }
            let lines: Vec<&str> = file_contents.lines().collect();
            let start = (e.position.from.0 as isize - 2).max(0) as usize;
            let end = e.position.to.0 + 1;

            let buffer = lines[start..end].join("\n");
            let mut line = start;
            let mut offset = 0;
            for c in buffer.chars() {
                if c == '\n' {
                    line += 1;
                    offset = 0;
                }
                if line >= (e.position.from.0 - 1)
                    && line <= (e.position.to.0 - 1)
                    && offset >= e.position.from.1
                    && offset <= e.position.to.1 - 1
                {
                    print!("{}", c.to_string().red());
                } else {
                    print!("{}", c.to_string().italic());
                }
                offset += 1;
            }
            println!();
            std::process::exit(-1);
        }
    }
}

impl Debug for ResError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} at {}:", "ERROR".red(), self.position)?;
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
}

impl ResErrorKind {
    fn with_handlers(&self, th: &TypeHandler, pos: Position) -> ResResult<String> {
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
            Self::ExpectedNumerical(got) => format!("expected numerical type, got {}", th.get(got, pos)?.identifier.blue()),
            Self::ExpectedVariable(ident, id) => {
                format!("expected variable {}, found function with {:?}", ident, id)
            }
            Self::ExpectedFunction(ident, id) => {
                format!("expected function {}, found variable with {:?}", ident, id)
            }
            Self::NoTypeInfo(t1, t2) => {
                format!("not possible to infer types between {:?} and {:?}", t1, t2)
            }
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
        }
    }
}
