use std::{any::Any, collections::HashMap, fmt::Display};

use crate::{
    error::{ResError, ResErrorKind, ResResult},
    functions::{FuncID, FuncSignature, FuncTable},
    lexer::Position,
    parser::{BinaryOperator, Litteral, Node, Statement, UnaryOperator},
    symbols::{Binding, ScopeTable, Symbol, SymbolID, SymbolTable},
};
use anyhow::anyhow;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeID(pub usize);

impl TypeID {
    pub const VOID: TypeID = TypeID(0);
    pub const U8: TypeID = TypeID(1);
    pub const U16: TypeID = TypeID(2);
    pub const U32: TypeID = TypeID(3);
    pub const U64: TypeID = TypeID(4);
    pub const I8: TypeID = TypeID(5);
    pub const I16: TypeID = TypeID(6);
    pub const I32: TypeID = TypeID(7);
    pub const I64: TypeID = TypeID(8);
    pub const BOOL: TypeID = TypeID(9);

    pub fn numerical(&self) -> bool {
        self.0 >= 1 && self.0 <= 8
    }

    pub fn strong(self) -> InferedType {
        InferedType {
            typeid: self,
            strong: true,
        }
    }
    pub fn weak(self) -> InferedType {
        InferedType {
            typeid: self,
            strong: false,
        }
    }
}

pub fn pick_infer(t1: InferedType, t2: InferedType, pos: Position) -> ResResult<InferedType> {
    match (t1.strong, t2.strong, t1.typeid == t2.typeid) {
        (true, true, true) => Ok(t1),
        (true, true, false) => Err(ResError::new(
            ResErrorKind::TypeMismatch(t1.typeid, t2.typeid),
            pos,
        )),
        (false, true, _) => Ok(t2),
        (true, false, _) => Ok(t1),
        (false, false, _) => Ok(t1),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub identifier: String,
    pub size: usize,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

#[derive(Default, Debug)]
pub struct TypeHandler {
    types: HashMap<TypeID, Type>,
}

impl TypeHandler {
    pub fn new() -> Self {
        let mut types = HashMap::new();
        types.insert(
            TypeID::BOOL,
            Type {
                identifier: String::from("bool"),
                size: 1,
            },
        );
        types.insert(
            TypeID::U8,
            Type {
                identifier: String::from("u8"),
                size: 1,
            },
        );
        types.insert(
            TypeID::U16,
            Type {
                identifier: String::from("u16"),
                size: 2,
            },
        );
        types.insert(
            TypeID::U32,
            Type {
                identifier: String::from("u32"),
                size: 4,
            },
        );
        types.insert(
            TypeID::U64,
            Type {
                identifier: String::from("u64"),
                size: 8,
            },
        );
        types.insert(
            TypeID::VOID,
            Type {
                identifier: String::from("null"),
                size: 0,
            },
        );
        Self { types }
    }

    pub fn get(&self, id: &TypeID, pos: Position) -> ResResult<&Type> {
        self.types
            .get(id)
            .ok_or(ResError::new(ResErrorKind::UnknownType(format!("{:?}", id)), pos))
    }

    pub fn lookup(&self, identifier: String) -> Option<TypeID> {
        for (k, v) in &self.types {
            if v.identifier == identifier {
                return Some(k.clone());
            }
        }
        None
    }
}

#[derive(Clone, Debug)]
pub enum TypedStatement {
    Expression(InferedType, TypedNode),
    Exit(InferedType, TypedNode),
    Return(InferedType, Option<TypedNode>),
    VarDecl(String, Option<String>, TypedNode),
    FuncDecl {
        identifier: String,
        args: Vec<(String, TypeID)>,
        ret: TypeID,
        body: Box<TypedStatement>,
    },
    Block(Vec<TypedStatement>),
    If(
        InferedType,
        TypedNode,
        Box<TypedStatement>,
        Option<Box<TypedStatement>>,
    ),
    While(TypedNode, Box<TypedStatement>),
    For(
        Option<Box<TypedStatement>>,
        Option<TypedNode>,
        Option<TypedNode>,
        Box<TypedStatement>,
    ),
}

impl TypedStatement {
    pub fn infered_type(&self) -> InferedType {
        match self.clone() {
            TypedStatement::If(t, _, _, _) => t,
            TypedStatement::Expression(t, _) => t,
            TypedStatement::Return(t, _) => t,
            _ => TypeID::VOID.weak(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypedNode {
    Binary {
        typeid: InferedType,
        left: Box<TypedNode>,
        right: Box<TypedNode>,
        operator: BinaryOperator,
        position: Position,
    },
    Parenthesis(InferedType, Box<TypedNode>),
    Unary(InferedType, UnaryOperator, Box<TypedNode>, Position),
    Litteral(InferedType, Litteral, Position),
    Identifier(InferedType, SymbolID, Position),
    FuncIdentifier(TypeID, String, Vec<TypedNode>, Position),
    Assignment(InferedType, String, Box<TypedNode>, Position),
}

impl TypedNode {
    pub fn infered_type(&self) -> InferedType {
        match self.clone() {
            Self::Unary(t, _, _, _) => t,
            Self::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => typeid,
            Self::Assignment(t, _, _, _) => t,
            Self::Litteral(t, _, _) => t,
            Self::Parenthesis(t, _) => t,
            Self::Identifier(t, _, _) => t,
            Self::FuncIdentifier(t, _, _, _) => t.strong(),
            _ => todo!(),
        }
    }

    pub fn position(&self) -> Position {
        #![allow(unused)]
        match self {
            Self::Binary {
                typeid,
                left,
                operator,
                right,
                position,
            } => position.clone(),
            Self::Unary(_, _, _, position) => position.clone(),
            Self::Litteral(_, _, position) => position.clone(),
            Self::Parenthesis(_, child) => child.position(),
            Self::Identifier(_, _, pos) => pos.clone(),
            Self::FuncIdentifier(_, _, _, pos) => pos.clone(),
            Self::Assignment(_, _, _, pos) => pos.clone(),
        }
    }

    pub fn downgrade(&self) -> Node {
        match self.clone() {
            TypedNode::Litteral(_, lit, p) => Node::Litteral(lit, p),
            TypedNode::Unary(_, op, c, p) => Node::Unary(op, Box::new(c.downgrade()), p),
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => Node::Binary {
                left: Box::new(left.downgrade()),
                right: Box::new(right.downgrade()),
                operator,
                position,
            },
            _ => todo!(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct InferedType {
    typeid: TypeID,
    strong: bool,
}

#[derive(Debug)]
pub struct Typer {
    scopes: ScopeTable,
    symbols: SymbolTable,
    functions: FuncTable,
}

impl Typer {
    pub fn new() -> Self {
        Self {
            scopes: ScopeTable::new(),
            symbols: SymbolTable::new(),
            functions: FuncTable::new(),
        }
    }

    pub fn resolve_stmts(
        &mut self,
        stmts: Vec<Statement>,
        type_handler: &mut TypeHandler,
    ) -> ResResult<Vec<TypedStatement>> {
        // TODO: Type Declerations here!
        self.collect_signatures(&stmts, type_handler);

        let mut resolved = Vec::new();
        for stmt in stmts {
            match stmt {
                Statement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => {
                    self.scopes.push_scope();
                    let collected_body: &Vec<Statement> = match body.as_ref() {
                        Statement::Block(stmts) => stmts,
                        stmt => &vec![stmt.clone()],
                    };
                    self.collect_signatures(&collected_body, type_handler);
                    if let Some(Binding::Function(id)) = self.scopes.lookup(identifier.clone()) {
                        let signature = self.functions.get(id).clone();
                        let args: Vec<(String, TypeID)> = args
                            .iter()
                            .zip(signature.params.clone())
                            .map(|((ident, _), t)| (ident.clone(), t))
                            .collect();
                        for (i, t) in &args {
                            let symbol = Symbol {
                                identifier: i.clone(),
                                typeid: InferedType {
                                    typeid: t.clone(),
                                    strong: true,
                                },
                            };
                            self.define_symbol(i.clone(), symbol);
                        }
                        self.functions.enter_func(id);
                        let typed_body = self.resolve_stmt(*body, type_handler)?;
                        self.functions.clear_current();
                        let function = TypedStatement::FuncDecl {
                            identifier,
                            args: args,
                            ret: signature.ret,
                            body: Box::new(typed_body),
                        };
                        resolved.push(function);
                    } else {
                        return Err(ResError::new(
                            ResErrorKind::UnknownFunction(identifier),
                            Position::new(0, 0, 0, 0),
                        ));
                    }
                    self.scopes.pop_scope();
                }
                _ => todo!(),
            }
        }
        Ok(resolved)
    }
    pub fn resolve_stmt(
        &mut self,
        stmt: Statement,
        type_handler: &mut TypeHandler,
    ) -> ResResult<TypedStatement> {
        match stmt {
            Statement::VarDecl(ident, ty, node) => {
                let typed_child = if let Some(type_ident) = ty.clone() {
                    let typeid = InferedType {
                        typeid: type_handler.lookup(type_ident).unwrap(),
                        strong: true,
                    };
                    self.resolve_node(node.clone(), type_handler, &typeid)?
                } else {
                    let weak_type = InferedType {
                        typeid: TypeID::VOID,
                        strong: false,
                    };
                    self.resolve_node(node.clone(), type_handler, &weak_type)?
                };
                let symbol = Symbol {
                    identifier: ident.clone(),
                    typeid: typed_child.infered_type(),
                };
                self.define_symbol(ident.clone(), symbol);
                Ok(TypedStatement::VarDecl(ident, ty, typed_child))
            }
            Statement::Block(stmts) => {
                self.scopes.push_scope();
                let mut typed_stmts = Vec::new();
                for stmt in stmts {
                    let typed_stmt = self.resolve_stmt(stmt, type_handler)?;
                    typed_stmts.push(typed_stmt);
                }
                self.scopes.pop_scope();
                Ok(TypedStatement::Block(typed_stmts))
            }
            Statement::Return(o_node) => {
                if let Some(sig) = self.functions.current() {
                    if let Some(node) = o_node {
                        let typed_node =
                            self.resolve_node(node, type_handler, &sig.ret.strong())?;
                        Ok(TypedStatement::Return(
                            typed_node.infered_type(),
                            Some(typed_node),
                        ))
                    } else {
                        Ok(TypedStatement::Return(TypeID::VOID.strong(), None))
                    }
                } else {
                    panic!("cant use return outside of a function");
                }
            }
            Statement::Expression(node) => {
                let child = self.resolve_node(node, type_handler, &TypeID::VOID.weak())?;
                Ok(TypedStatement::Expression(child.infered_type(), child))
            }
            Statement::If(pre, body, el) => {
                let pre = self.resolve_node(pre, type_handler, &TypeID::BOOL.strong())?;
                let body = self.resolve_stmt(*body, type_handler)?;
                if let Some(else_body) = el {
                    let el = self.resolve_stmt(*else_body, type_handler)?;
                    let t = pick_infer(body.infered_type(), el.infered_type(), pre.position())?;
                    Ok(TypedStatement::If(
                        t,
                        pre,
                        Box::new(body),
                        Some(Box::new(el)),
                    ))
                } else {
                    Ok(TypedStatement::If(
                        body.infered_type(),
                        pre,
                        Box::new(body),
                        None,
                    ))
                }
            }
            _ => todo!(),
        }
    }
    pub fn resolve_node(
        &mut self,
        node: Node,
        type_handler: &mut TypeHandler,
        expected: &InferedType,
    ) -> ResResult<TypedNode> {
        match node {
            Node::Litteral(lit, pos) => {
                if expected.strong {
                    return Ok(TypedNode::Litteral(expected.clone(), lit, pos));
                }
                let typeid = match lit {
                    Litteral::Nil => TypeID::VOID,
                    Litteral::Number(_) => TypeID::U64,
                    Litteral::Boolean(_) => TypeID::BOOL,
                    _ => todo!(),
                };
                Ok(TypedNode::Litteral(
                    InferedType {
                        typeid,
                        strong: false,
                    },
                    lit,
                    pos,
                ))
            }
            Node::Assignment(ident, child, pos) => {
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new(
                    ResErrorKind::UnknownVariable(ident.clone()),
                    pos,
                ))?;
                match binding {
                    Binding::Variable(id) => {
                        let symbol: Symbol = self.symbols.get(id).clone();
                        let node = self.resolve_node(*child, type_handler, &symbol.typeid)?;

                        let symbol: &mut Symbol = self.symbols.get_mut(id);

                        if !symbol.typeid.strong && node.infered_type().strong {
                            symbol.typeid = node.infered_type();
                        }
                        if symbol.typeid.typeid != node.infered_type().typeid {
                            Err(ResError::new(
                                ResErrorKind::TypeMismatch(
                                    symbol.typeid.typeid,
                                    node.infered_type().typeid,
                                ),
                                pos,
                            ))
                        } else {
                            Ok(TypedNode::Assignment(
                                symbol.typeid.clone(),
                                ident,
                                Box::new(node),
                                pos,
                            ))
                        }
                    }
                    Binding::Function(id) => Err(ResError::new(
                        ResErrorKind::ExpectedVariable(ident, id),
                        pos,
                    )),
                }
            }
            Node::Identifier(ident, pos) => {
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new(
                    ResErrorKind::UnknownVariable(ident.clone()),
                    pos,
                ))?;
                match binding {
                    Binding::Variable(id) => {
                        let symbol: &mut Symbol = self.symbols.get_mut(id);
                        if symbol.typeid.strong
                            && expected.strong
                            && symbol.typeid.typeid != expected.typeid
                        {
                            return Err(ResError::new(
                                ResErrorKind::TypeMismatch(expected.typeid, symbol.typeid.typeid),
                                pos,
                            ));
                        }
                        if !symbol.typeid.strong && expected.strong {
                            symbol.typeid = expected.clone();
                        }
                        Ok(TypedNode::Identifier(symbol.typeid.clone(), id, pos))
                    }
                    Binding::Function(id) => Err(ResError::new(
                        ResErrorKind::ExpectedVariable(ident, id),
                        pos,
                    )),
                }
            }
            Node::FuncIdentifier(ident, params, pos) => {
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new(
                    ResErrorKind::UnknownFunction(ident.clone()),
                    pos,
                ))?;
                match binding {
                    Binding::Function(id) => {
                        let signature = self.functions.get(id).clone();
                        let mut parsed_args = Vec::new();
                        for (node, ty) in params.iter().zip(signature.params.clone()) {
                            let typed_node =
                                self.resolve_node(node.clone(), type_handler, &ty.strong())?;
                            parsed_args.push(typed_node);
                        }
                        Ok(TypedNode::FuncIdentifier(
                            signature.ret,
                            ident,
                            parsed_args,
                            pos,
                        ))
                    }
                    Binding::Variable(id) => Err(ResError::new(
                        ResErrorKind::ExpectedFunction(ident, id),
                        pos,
                    )),
                }
            }
            Node::Unary(op, node, pos) => {
                let inner = self.resolve_node(*node, type_handler, expected)?;
                if inner.infered_type().strong
                    && expected.strong
                    && inner.infered_type().typeid != expected.typeid
                {
                    return Err(ResError::new(
                        ResErrorKind::TypeMismatch(expected.typeid, inner.infered_type().typeid),
                        pos,
                    ));
                }
                Ok(TypedNode::Unary(
                    inner.infered_type(),
                    op,
                    Box::new(inner),
                    pos,
                ))
            }
            Node::Parenthesis(node) => {
                let inner = self.resolve_node(*node, type_handler, expected)?;
                if inner.infered_type().strong
                    && expected.strong
                    && inner.infered_type().typeid != expected.typeid
                {
                    return Err(ResError::new(
                        ResErrorKind::TypeMismatch(expected.typeid, inner.infered_type().typeid),
                        inner.position(),
                    ));
                }
                Ok(TypedNode::Parenthesis(
                    inner.infered_type(),
                    Box::new(inner),
                ))
            }
            Node::Binary {
                left,
                right,
                operator,
                position,
            } => {
                let mut inner_left =
                    self.resolve_node(*left, type_handler, &expected.typeid.weak())?;
                let mut inner_right =
                    self.resolve_node(*right, type_handler, &expected.typeid.weak())?;
                if operator.is_boolean_in() {
                    self.correct_node(&mut inner_left, &TypeID::BOOL);
                    self.correct_node(&mut inner_right, &TypeID::BOOL);
                } else {
                    if expected.typeid.numerical() {
                        self.correct_node(&mut inner_left, &expected.typeid);
                        self.correct_node(&mut inner_right, &expected.typeid);
                    }
                }
                let type_left = inner_left.infered_type();
                let type_right = inner_right.infered_type();
                if type_left.typeid != type_right.typeid {
                    match (type_left.strong, type_right.strong) {
                        (true, true) => {
                            return Err(ResError::new(
                                ResErrorKind::TypeMismatch(type_left.typeid, type_right.typeid),
                                position,
                            ));
                        },
                        (true, false) => self.correct_node(&mut inner_right, &type_left.typeid),
                        (false, true) => self.correct_node(&mut inner_left, &type_right.typeid),
                        (false, false) => {
                            return Err(ResError::new(
                                ResErrorKind::NoTypeInfo(type_left.typeid, type_right.typeid),
                                position,
                            ));
                        }
                    }
                }
                let output_type = if operator.is_boolean_out() {
                    TypeID::BOOL.strong()
                } else {
                    expected.clone()
                };
                Ok(TypedNode::Binary {
                    typeid: output_type,
                    left: Box::new(inner_left),
                    right: Box::new(inner_right),
                    operator,
                    position,
                })
            }
            todo => {
                todo!()
            }
        }
    }

    pub fn correct_node(&mut self, mut node: &mut TypedNode, expected: &TypeID) {
        match &mut node {
            TypedNode::Identifier(t, _, _) => {
                if t.strong && t.typeid != expected.clone() {
                    panic!("Expected {:?}, got {:?}", expected, t);
                }
                t.strong = true;
                t.typeid = *expected;
            }
            TypedNode::Litteral(t, _, _) => {
                if t.strong && t.typeid != expected.clone() {
                    panic!("Expected {:?}, got {:?}", expected, t);
                }
                t.strong = true;
                t.typeid = *expected;
            }
            TypedNode::Unary(ty, _, child, _) => {
                if ty.strong && ty.typeid != expected.clone() {
                    panic!("Expected {:?}, got {:?}", expected, ty.typeid);
                }
                ty.strong = true;
                ty.typeid = expected.clone();
                self.correct_node(child, expected);
            }
            TypedNode::Parenthesis(ty, child) => {
                if ty.strong && ty.typeid != expected.clone() {
                    panic!("Expected {:?}, got {:?}", expected, ty.typeid);
                }
                ty.strong = true;
                ty.typeid = expected.clone();
                self.correct_node(child, expected);
            }
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator: _,
                position: _,
            } => {
                if typeid.strong && typeid.typeid != expected.clone() {
                    panic!("Expected {:?}, got {:?}", expected, typeid.typeid);
                }
                typeid.strong = true;
                typeid.typeid = expected.clone();
                self.correct_node(left, expected);
                self.correct_node(right, expected);
            }
            _ => todo!(),
        }
    }

    pub fn collect_signatures(&mut self, stmts: &Vec<Statement>, type_handler: &mut TypeHandler) {
        for stmt in stmts {
            match stmt {
                Statement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => {
                    let mut typed_args = Vec::new();
                    for (_, ty) in args {
                        let t = InferedType {
                            typeid: type_handler.lookup(ty.clone()).unwrap(),
                            strong: true,
                        };
                        typed_args.push(t.typeid);
                    }
                    let typed_ret = if let Some(ty) = ret {
                        type_handler.lookup(ty.clone()).unwrap()
                    } else {
                        TypeID::VOID
                    };
                    let signature = FuncSignature {
                        identifier: identifier.clone(),
                        params: typed_args,
                        ret: typed_ret,
                    };

                    self.define_func(identifier.clone(), signature);
                }
                _ => (),
            }
        }
    }

    pub fn define_symbol(&mut self, identifier: String, symbol: Symbol) -> SymbolID {
        let id = self.symbols.insert(symbol);

        self.scopes.insert(identifier, Binding::Variable(id));

        id
    }

    pub fn define_func(&mut self, identifier: String, func: FuncSignature) -> FuncID {
        let id = self.functions.insert(func);

        self.scopes.insert(identifier, Binding::Function(id));

        id
    }
}
