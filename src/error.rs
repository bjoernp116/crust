use std::{any::type_name, collections::{HashMap, hash_map::Keys}, fmt::Debug, hash::Hash, io::Write, ops::AddAssign};

use colored::Colorize;

use crate::{
    Command::Parse, asm::Register, functions::FuncID, lexer::{Position, Token, TokenType}, parser::Node, ssa::{RegisterSet, SlotID}, structs::FieldID, symbols::{Symbol, SymbolID}, types::{LoopContext, TypeHandler, TypeID, TypedNode, TypedStatement}
};

pub type CrustResult<T, E: CrustError> = Result<T, E>;
pub type ParserResult<T> = CrustResult<T, ParserError>;
pub type TypeResult<T> = CrustResult<T, TypeError>;
pub type IRResult<T> = CrustResult<T, IRError>;
pub type AnyResult<T> = Result<T, AnyError>;

pub enum AnyError {
    Parser(ParserError),
    Type(TypeError),
    IR(IRError),
}

impl<E: CrustError> From<E> for AnyError {
    fn from(value: E) -> Self {
        value.to_any()
    }
}

impl Debug for AnyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Parser(e) => write!(f, "{:?}", e),
            Self::Type(e) => write!(f, "{:?}", e),
            Self::IR(e) => write!(f, "{:?}", e),
        }
    }
}


#[derive(Clone)]
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

pub trait CrustError: Sized + Debug {
    type Kind;
    fn new_err(kind: Self::Kind, position: Position) -> Self;
    fn to_string(&self, th: &TypeHandler) -> String;
    fn severity(&self) -> Severity;
    fn position(&self) -> Option<Position>;
    fn to_any(self) -> AnyError;
    fn from_any(any: AnyError) -> Option<Self>;
}

pub struct IRError {
    pub kind: IRErrorKind,
    pub position: Option<Position>
}

impl Debug for IRError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(&TypeHandler::new()))
    }
}

impl CrustError for IRError {
    type Kind = IRErrorKind;
    fn severity(&self) -> Severity {
        Severity::Error
    }
    fn position(&self) -> Option<Position> {
        None
    }
    fn new_err(kind: Self::Kind, position: Position) -> Self {
        Self { kind, position: Some(position) }
    }
    fn to_any(self) -> AnyError {
        AnyError::IR(self)
    }
    fn from_any(any: AnyError) -> Option<Self> {
        if let AnyError::IR(e) = any { Some(e) } else { None }
    }
    fn to_string(&self, th: &TypeHandler) -> String {
        match &self.kind {
            IRErrorKind::ExpectedPlace(node) => {
                format!("expected place, got {:?}", node)
            }
            IRErrorKind::ExpectedScalar(node) => {
                format!("expected scalar value, got {:?}", node)
            }
            IRErrorKind::Unreachable => {
                format!("statement is unreachable")
            },
            IRErrorKind::ExpectedReturnValue(_) => {
                format!("expected return value")
            },
            IRErrorKind::ExpectedElse(_) => {
                format!("expected else if then block is open")
            }
            IRErrorKind::SlotNotFound(slot) => {
                format!("slot {:?} not found in stack frame", slot)
            },
        }
    }
}

pub enum IRErrorKind {
    Unreachable,
    ExpectedReturnValue(TypeID),
    ExpectedPlace(TypedNode),
    ExpectedScalar(TypedNode),
    ExpectedElse(TypeID),
    SlotNotFound(SlotID)
}


pub struct TypeError {
    pub kind: TypeErrorKind,
    pub position: Option<Position>,
}
impl Debug for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(&TypeHandler::new()))
    }
}

impl CrustError for TypeError {
    type Kind = TypeErrorKind;

    fn from_any(any: AnyError) -> Option<Self> {
        if let AnyError::Type(e) = any { Some(e) } else { None }
    }
    fn new_err(kind: Self::Kind, position: Position) -> Self {
        TypeError { kind, position: Some(position) }
    }
    fn to_any(self) -> AnyError {
        AnyError::Type(self)
    }
    fn to_string(&self, th: &TypeHandler) -> String {
        match &self.kind {
            Self::Kind::TypeMismatch(exp, got) => format!("expected type {:?}, got {:?}", exp, got),
            Self::Kind::ExpectedStruct => {
                format!("expected struct")
            }
            Self::Kind::UnknownType(ident) => format!("cannot find type {} in this scope", ident),
            Self::Kind::NotInferableNode => {
                format!("expression cant be infered")
            }
            Self::Kind::NotInferableStmt => {
                format!("statement cant be infered")
            }
            Self::Kind::UnknownFunction(ident) => {
                format!("cannot find function {} in this scope", ident)
            }
            Self::Kind::UnexpectedBreak(ctx) => {
                format!("cant break with value in a {:?} loop", ctx)
            },
            Self::Kind::ExpectedNonArray(exp) => format!(
                "expected type {}, got array constructor",
                th.err_string(exp),
            ),
            Self::Kind::ExpectedArray(exp) => format!(
                "expected array, got {}",
                th.err_string(exp),
            ),
            Self::Kind::CantDeref(ty) => format!("cannot dereference type {}", th.err_string(ty)),
            Self::Kind::UnknownVariable(ident) => {
                format!("cannot find variable {} in this scope", ident)
            }
            Self::Kind::ExpectedVariable(ident, id) => {
                format!("expected variable {}, found function with {:?}", ident, id)
            }
            Self::Kind::ExpectedFunction(ident, id) => {
                format!("expected function {}, found variable with {:?}", ident, id)
            }
            Self::Kind::NoTypeInfo(t1, t2) => {
                format!("not possible to infer types between {:?} and {:?}", t1, t2)
            }
            Self::Kind::CantAssignImmutable(sym) => {
                format!("cant assign to immutable symbol {}", sym.identifier.green())
            }
            Self::Kind::FieldNotFound(field, ty) => {
                format!("field {} not found on type {}", field, th.err_string(ty))
            }
            Self::Kind::UnknownField(ident, struc) => {
                format!("cannot find field {} in this struct {}", ident, struc)
            }
        }
    }
    fn position(&self) -> Option<Position> {
        self.position
    }
    fn severity(&self) -> Severity {
        Severity::Error
    }
}

pub enum TypeErrorKind {
    TypeMismatch(TypeID, TypeID),
    ExpectedStruct,
    UnknownType(String),
    NotInferableNode,
    NotInferableStmt, 
    UnknownFunction(String),
    UnexpectedBreak(LoopContext),
    ExpectedNonArray(TypeID),
    ExpectedArray(TypeID),
    CantDeref(TypeID),
    UnknownVariable(String),
    ExpectedVariable(String, FuncID),
    ExpectedFunction(String, SymbolID),
    NoTypeInfo(TypeID, TypeID),
    CantAssignImmutable(Symbol),
    FieldNotFound(String, TypeID),
    UnknownField(String, String)
}

pub struct ParserError {
    pub kind: ParserErrorKind,
    pub position: Position,
}

impl Debug for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_string(&TypeHandler::new()))
    }
}

impl CrustError for ParserError {
    type Kind = ParserErrorKind;
    fn new_err(kind: Self::Kind, position: Position) -> Self {
        Self { kind, position }
    }
    fn to_any(self) -> AnyError {
        AnyError::Parser(self)
    }
    fn from_any(any: AnyError) -> Option<Self> {
        if let AnyError::Parser(e) = any { Some(e) } else { None }
    }
    fn to_string(&self, th: &TypeHandler) -> String {
        match &self.kind {
            Self::Kind::ExpectedIdentifier => {
                format!("expected identifier")
            }
            Self::Kind::ExpectedNumerical(got) => format!(
                "expected numerical type, got {}",
                th.err_string(&got)
            ),
            Self::Kind::ExpectedInitializerList => {
                format!("expected initializer list")
            }
            Self::Kind::ExpectedTypeIdentifier => {
                format!("expected type identifier")
            }
            Self::Kind::UnexpectedEOF => {
                format!("unexpected end of file")
            }
            Self::Kind::ExpectedSeparator(got) => {
                format!("expected separator, got {}", got)
            }
            Self::Kind::ExpectedExpression => {
                format!("expected expression")
            }
            Self::Kind::TokenToOperator(token) => {
                format!("cant convert token {:?} to operator", token)
            }
            Self::Kind::ExpectedSemicolon(token) => {
                format!("expected semicolon, got {}", token.raw)
            }
            Self::Kind::UnescapedParenthesis => {
                format!("unescaped parenthesis")
            }
        }
    }
    fn severity(&self) -> Severity {
        Severity::Error
    }
    fn position(&self) -> Option<Position> {
        Some(self.position)
    }
}

pub enum ParserErrorKind {
    ExpectedIdentifier,
    ExpectedInitializerList,
    ExpectedTypeIdentifier,
    ExpectedNumerical(TypeID),
    ExpectedSemicolon(Token),
    UnexpectedEOF,
    ExpectedSeparator(Token),
    UnescapedParenthesis,
    ExpectedExpression,
    TokenToOperator(TokenType)
}


#[derive(Clone)]
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

pub fn unwrap_any<T>(res: AnyResult<T>, file_contents: &String, type_handler: &TypeHandler) -> T {
    match res {
        Ok(t) => return t,
        Err(AnyError::Parser(e)) => print_err(e, file_contents, type_handler),
        Err(AnyError::Type(e)) => print_err(e, file_contents, type_handler),
        Err(AnyError::IR(e)) => print_err(e, file_contents, type_handler),
    }
    std::process::exit(-1);
}

pub fn unwrap_print<T, E: CrustError>(res: CrustResult<T, E>, file_contents: &String, type_handler: &TypeHandler) -> T {
    match res {
        Ok(t) => t,
        Err(e) => {
            print_err(e, file_contents, type_handler);
            std::process::exit(-1);
        }
    }
}

pub fn print_err<E: CrustError>(err: E, file_contents: &String, type_handler: &TypeHandler) {
    if let Some(pos) = err.position() {
        print!("{:?} at {}:", err.severity(), pos);
    } else {
        print!("{:?}:", err.severity());
    }
    let s = err.to_string(type_handler);
    println!(" {}", s.bold());
    if let Some(pos) = err.position() {
        let lines: Vec<&str> = file_contents.lines().collect();
        let start = (pos.from.0 as isize - 2).max(0) as usize;
        let end = (pos.to.0 + 1).min(lines.len());

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

#[derive(Clone)]
pub enum ResErrorKind {
    UnknownVariable(String),
    UnknownType(String),
    UnknownFunction(String),
    UnknownField(String, String),
    TypeMismatch(TypeID, TypeID),
    ExpectedVariable(String, FuncID),
    ExpectedFunction(String, SymbolID),
    NoTypeInfo(TypeID, TypeID),
    NotInferableNode,
    NotInferableStmt,
    ExpectedStruct,
    ExpectedElse(TypeID),
    FieldNotFound(String, TypeID),
    SlotNotFound(SlotID),
    CantAssignImmutable(Symbol),
    CantDeref(TypeID),
    ExpectedPlace(TypedNode),
    ExpectedPlaceSymbol,
    ExpectedScalar(TypedNode),
    ControlFlowExited,
    Unreachable,
    ExpectedReturnValue(TypeID),
    ExpectedNonArray(TypeID),
    ExpectedArray(TypeID),
}

impl ResErrorKind {
    fn with_handlers(&self, th: &TypeHandler, pos: Option<Position>) -> CrustResult<String, ResError> {
        Ok(match self {
            Self::UnknownType(ident) => format!("cannot find type {} in this scope", ident),
            Self::UnknownField(ident, struc) => {
                format!("cannot find field {} in this struct {}", ident, struc)
            }
            Self::UnknownFunction(ident) => {
                format!("cannot find function {} in this scope", ident)
            }
            el => format!("{:?}", el),
        })
    }
}

impl Debug for ResErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnknownVariable(ident) => {
                write!(f, "cannot find variable {} in this scope", ident)
            }
            Self::UnknownField(ident, struc) => {
                write!(f, "cannot find field {} in this struct {}", ident, struc)
            }
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
            Self::FieldNotFound(field, ty) => {
                write!(f, "field {} not found on type {:?}", field, ty)
            }
            Self::SlotNotFound(slot) => {
                write!(f, "slot {:?} not found in stack frame", slot)
            },
            Self::CantDeref(ty) => {
                write!(f, "cannot dereference type {:?}", ty)
            }
            Self::ExpectedPlaceSymbol => {
                write!(f, "expected place for symbol")
            },
            Self::ControlFlowExited => {
                write!(f, "control flow has exited!")
            },
            _ => Ok(())
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


#[derive(Debug)]
pub struct Counter<K> {
    map: HashMap<K, usize>
}

impl<K: Eq + Hash + Clone> Counter<K> {
    pub fn new() -> Self {
        Self { map: HashMap::new() }
    }

    pub fn get(&self, key: &K) -> usize {
        if let Some(count) = self.map.get(key) {
            count.clone()
        } else {
            0
        }
    }

    pub fn increment(&mut self, key: &K) -> usize {
        if let Some(count) = self.map.get_mut(key) {
            *count += 1;
            *count
        } else {
            self.map.insert(key.clone(), 1);
            1
        }
    }

    pub fn keys(&self) -> Keys<'_, K, usize> {
        self.map.keys()
    }
}

#[derive(Debug)]
pub struct Setter<K> {
    order: Vec<K>,
    used: HashMap<K, bool>
}

impl<K: Eq + Hash + Clone + Debug> Setter<K> {
    pub fn new(order: Vec<K>) -> Self {
        let mut used = HashMap::new();
        for k in order.clone() {
            used.insert(k, false);
        }
        Self { order, used }
    }

    pub fn get_unused(&mut self) -> Option<K> {
        for key in &self.order {
            if let Some(used) = self.used.get_mut(key) {
                if !used.clone() {
                    *used = true;
                    return Some(key.clone());
                }
            } else {
                unreachable!()
            }
        }
        None
    }

    pub fn use_key(&mut self, key: &K) {
        *self.used.get_mut(key).unwrap() = true;
    }

    pub fn drop(&mut self, key: &K) {
        if let Some(used) = self.used.get_mut(key) {
            *used = false;
        } 
    }

    pub fn left(&self) -> usize {
        self.used.iter().filter(|(_, v)| !*v).count()
    }

    pub fn is_used(&self, key: &K) -> bool {
        self.used.get(key).unwrap_or(&false).clone()
    }

    pub fn get_used(&self) -> Vec<K> {
        self.used.iter().filter(|(_, v)| **v).map(|(k, _)| k.clone()).collect()
    }

}

impl Setter<Register> {
    pub fn to_set(&self) -> RegisterSet {
        RegisterSet::from(self.used.iter().filter(|(_, v)| !**v).map(|(k, _)| k).collect::<Vec<_>>())
    }
}
