use std::{any::Any, collections::HashMap, fmt::Display};

use crate::{
    error::{ResError, ResErrorKind, ResResult, Severity, print_err, unwrap_print},
    functions::{FuncID, FuncSignature, FuncTable},
    lexer::Position,
    parser::{BinaryOperator, Block, Litteral, Node, Statement, TypeSyntax, UnaryOperator},
    structs::{FieldID, StructID, StructTable},
    symbols::{Binding, ScopeTable, Symbol, SymbolID, SymbolTable},
};

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
    pub const STR: TypeID = TypeID(10);
    pub const NEVER: TypeID = TypeID(11);

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
        (true, true, false) => Err(ResError::new_err(
            ResErrorKind::TypeMismatch(t1.typeid, t2.typeid),
            pos,
        )),
        (false, true, _) => Ok(t2),
        (true, false, _) => Ok(t1),
        (false, false, _) => Ok(t1),
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub identifier: String,
    pub size: usize,
    pub kind: TypeKind,
}

#[derive(Debug, Clone)]
pub enum TypeKind {
    Primitive,
    Structure(StructID),
    Reference(TypeID),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.identifier)
    }
}

#[derive(Default, Debug)]
pub struct TypeHandler {
    types: HashMap<TypeID, Type>,
    bindings: HashMap<TypeSyntax, TypeID>,
}

impl TypeHandler {
    pub fn new() -> Self {
        let mut handler = Self {
            types: HashMap::new(),
            bindings: HashMap::new(),
        };

        handler.define(Type {
            identifier: String::from("null"),
            size: 0,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("u8"),
            size: 1,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("u16"),
            size: 2,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("u32"),
            size: 4,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("u64"),
            size: 8,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("i8"),
            size: 1,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("i16"),
            size: 2,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("i32"),
            size: 4,
            kind: TypeKind::Primitive,
        });
        handler.define(Type {
            identifier: String::from("i64"),
            size: 8,
            kind: TypeKind::Primitive,
        });

        handler.define(Type {
            identifier: String::from("bool"),
            size: 1,
            kind: TypeKind::Primitive,
        });

        handler.define(Type {
            identifier: String::from("str"),
            size: 16,
            kind: TypeKind::Structure(StructID(0)),
        });
        handler.define(Type {
            identifier: String::from("never"),
            size: 0,
            kind: TypeKind::Primitive,
        });
        handler
    }

    pub fn is_scalar(&self, id: &TypeID) -> ResResult<bool> {
        match self.get(id, None)?.kind {
            TypeKind::Primitive | TypeKind::Reference(_) => Ok(true),
            _ => Ok(false),
        }
    }

    pub fn get(&self, id: &TypeID, pos: Option<Position>) -> ResResult<&Type> {
        self.types.get(id).ok_or(ResError {
            kind: ResErrorKind::UnknownType(format!("{:?}", id)),
            position: pos,
            severity: Severity::Error,
        })
    }

    pub fn get_mut(&mut self, id: &TypeID, pos: Option<Position>) -> ResResult<&mut Type> {
        self.types.get_mut(id).ok_or(ResError {
            kind: ResErrorKind::UnknownType(format!("{:?}", id)),
            position: pos,
            severity: Severity::Error,
        })
    }

    pub fn lookup(&self, identifier: TypeSyntax) -> Option<TypeID> {
        if let Some(id) = self.bindings.get(&identifier) {
            Some(id.clone())
        } else {
            None
        }
    }

    pub fn lookup_or_define(&mut self, identifier: TypeSyntax) -> Option<TypeID> {
        if let Some(id) = self.bindings.get(&identifier) {
            Some(id.clone())
        } else {
            if let TypeSyntax::Reference { mutable, pointee } = identifier {
                let pointee_id = self.lookup_or_define(*pointee.clone()).unwrap().clone();
                let ref_id = TypeID(self.types.len());
                let pointee_type = self.get(&pointee_id, None).unwrap();
                let ref_type = Type {
                    identifier: format!("&{}", pointee_type.identifier),
                    size: 8,
                    kind: TypeKind::Reference(pointee_id),
                };
                self.types.insert(ref_id, ref_type);
                self.bindings.insert(
                    TypeSyntax::Reference {
                        mutable: false,
                        pointee,
                    },
                    ref_id,
                );
                Some(ref_id)
            } else {
                None
            }
        }
    }

    pub fn u8_ref(&mut self) -> TypeID {
        let syntax = TypeSyntax::Reference {
            mutable: false,
            pointee: Box::new(TypeSyntax::Raw("u8".to_string())),
        };
        self.lookup_or_define(syntax).unwrap()
    }

    pub fn define(&mut self, ty: Type) -> TypeID {
        let id = TypeID(self.types.len());
        self.types.insert(id, ty.clone());
        let raw = TypeSyntax::Raw(ty.identifier);
        self.bindings.insert(raw.clone(), id);
        id
    }
}

#[derive(Clone, Debug)]
pub enum TypedStatement {
    Expression(InferedType, TypedNode),
    Exit(InferedType, TypedNode),
    Return(InferedType, Option<TypedNode>),
    Break(InferedType, Option<TypedNode>),
    VarDecl(String, SymbolID, TypedNode),
    FuncDecl {
        identifier: String,
        args: Vec<(SymbolID, TypeID)>,
        ret: TypeID,
        body: TypedBlock,
    },
    While(TypedNode, TypedBlock),
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
            TypedStatement::Expression(t, _) => t,
            TypedStatement::Return(t, _) => t,
            _ => TypeID::VOID.weak(),
        }
    }

    pub fn child_node(&self) -> Option<&TypedNode> {
        match self {
            Self::VarDecl(_, _, n) => Some(n),
            Self::Exit(_, n) => Some(n),
            Self::Return(_, n) => n.into(),
            Self::Expression(_, n) => Some(n),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypedBlock {
    pub stmts: Vec<TypedStatement>,
    pub tail: Option<Box<TypedNode>>,
    pub position: Position,
}

impl TypedBlock {
    pub fn infered_type(&self) -> InferedType {
        if let Some(tail) = &self.tail {
            tail.infered_type()
        } else {
            TypeID::VOID.weak()
        }
    }

    pub fn break_type(&self, loop_ctx: &LoopContext) -> ResResult<InferedType> {
        if let LoopContext::Loop(loop_type) = loop_ctx {
            let tail_type = self.infered_type();
            pick_infer(loop_type.clone(), tail_type, Position::new(0, 0, 0, 0))
        } else {
            Ok(self.infered_type())
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
    FieldAccess(InferedType, Box<TypedNode>, FieldID, Position),
    Address(InferedType, bool, Box<TypedNode>, Position),
    Deref(InferedType, Box<TypedNode>, Position),
    FuncIdentifier(TypeID, FuncID, Vec<TypedNode>, Position),
    Assignment(InferedType, SymbolID, Box<TypedNode>, Position),
    Block(InferedType, TypedBlock),
    Loop(InferedType, TypedBlock),
    If(
        InferedType,
        Box<TypedNode>,
        TypedBlock,
        Option<TypedBlock>,
        Position,
    ),
    Constructor(TypeID, Vec<(FieldID, TypedNode)>, Position),
}

impl TypedNode {
    pub fn infered_type(&self) -> InferedType {
        match self.clone() {
            Self::Unary(t, _, _, _) => t,
            Self::If(t, _, _, _, _) => t,
            Self::Binary {
                typeid,
                left: _,
                right: _,
                operator: _,
                position: _,
            } => typeid,
            Self::Assignment(t, _, _, _) => t,
            Self::Litteral(t, _, _) => t,
            Self::Parenthesis(t, _) => t,
            Self::Identifier(t, _, _) => t,
            Self::FuncIdentifier(t, _, _, _) => t.strong(),
            Self::Block(t, _) => t,
            Self::Loop(t, _) => t,
            Self::Constructor(t, _, _) => t.strong(),
            Self::FieldAccess(t, _, _, _) => t,
            Self::Address(t, _, _, _) => t,
            Self::Deref(t, _, _) => t,
            _ => todo!(),
        }
    }
    pub fn child_stmts(&self) -> Vec<&TypedStatement> {
        todo!()
    }
    pub fn child_nodes(&self) -> Vec<&TypedNode> {
        match self {
            TypedNode::Assignment(_, _, n, _) => vec![n],
            TypedNode::FuncIdentifier(_, _, nodes, _) => nodes.iter().collect(),
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => vec![left, right],
            TypedNode::Unary(_, _, n, _) => vec![n],
            TypedNode::Parenthesis(_, n) => vec![n],
            _ => vec![],
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
            Self::FieldAccess(_, _, _, pos) => pos.clone(),
            Self::Address(_, _, _, pos) => pos.clone(),
            Self::Deref(_, _, pos) => pos.clone(),
            Self::FuncIdentifier(_, _, _, pos) => pos.clone(),
            Self::Assignment(_, _, _, pos) => pos.clone(),
            Self::Block(_, block) => block.position.clone(),
            Self::Loop(_, block) => block.position.clone(),
            Self::If(_, _, _, _, pos) => pos.clone(),
            Self::Constructor(_, _, pos) => pos.clone(),
        }
    }

    pub fn downgrade(&self) -> Node {
        match self.clone() {
            TypedNode::Litteral(_, lit, p) => Node::Litteral(lit, p),
            TypedNode::Unary(_, op, c, p) => Node::Unary(op, Box::new(c.downgrade()), p),
            TypedNode::Binary {
                typeid: _,
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

    pub fn is_litt(&self) -> Option<Litteral> {
        match self {
            TypedNode::Litteral(_, litt, _) => Some(litt.clone()),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, Copy)]
pub struct InferedType {
    pub typeid: TypeID,
    pub strong: bool,
}

impl InferedType {
    pub fn assert(&self, pos: Position) -> ResResult<()> {
        if !self.strong && self.typeid == TypeID::VOID {
            Err(ResError::new_err(ResErrorKind::NotInferableNode, pos))
        } else {
            Ok(())
        }
    }
}

#[derive(Debug, Clone)]
pub enum LoopContext {
    Loop(InferedType),
    While,
    For,
}

#[derive(Debug)]
pub struct Typer {
    scopes: ScopeTable,
    symbols: SymbolTable,
    functions: FuncTable,
    structs: StructTable,
    loops: Vec<LoopContext>,
    optimize_map: HashMap<SymbolID, TypedNode>,
}

impl Typer {
    pub fn new(th: &mut TypeHandler) -> Self {
        Self {
            scopes: ScopeTable::new(),
            symbols: SymbolTable::new(),
            functions: FuncTable::new(th),
            structs: StructTable::new(th),
            loops: Vec::new(),
            optimize_map: HashMap::new(),
        }
    }

    pub fn resolve_stmts(
        &mut self,
        stmts: Vec<Statement>,
        type_handler: &mut TypeHandler,
        file_contents: &String,
    ) -> ResResult<Vec<TypedStatement>> {
        // TODO: Type Declerations here!
        self.collect_signatures(&stmts, type_handler);

        let mut resolved = Vec::new();
        for stmt in stmts {
            match stmt {
                Statement::Struct(identifier, args) => {
                    let type_id = type_handler.lookup(TypeSyntax::Raw(identifier)).unwrap();
                    let struc = type_handler.get(&type_id, None).unwrap();
                    if let TypeKind::Structure(id) = struc.kind {
                        self.structs.define_fields(id, args, type_handler);
                    }
                }
                Statement::FuncDecl {
                    identifier,
                    args,
                    ret: _,
                    body,
                } => {
                    self.scopes.push_scope();
                    let collected_body: Vec<Statement> = body.collect();
                    self.collect_signatures(&collected_body, type_handler);
                    if let Some(Binding::Function(id)) = self.scopes.lookup(identifier.clone()) {
                        let signature = self.functions.get(&id).clone();
                        let args: Vec<(String, TypeID)> = args
                            .iter()
                            .zip(signature.params.clone())
                            .map(|((ident, _), t)| (ident.clone(), t))
                            .collect();
                        let mut arg_symbols = Vec::new();
                        for (i, t) in &args {
                            let symbol = Symbol {
                                identifier: i.clone(),
                                typeid: InferedType {
                                    typeid: t.clone(),
                                    strong: true,
                                },
                                mutable: false,
                            };
                            arg_symbols.push((self.define_symbol(i.clone(), symbol), t.clone()));
                        }
                        self.functions.enter_func(id);
                        let typed_body =
                            self.resolve_block(body, type_handler, &signature.ret.strong())?;
                        /*while let Err(e) = self.strong_stmt(&typed_body) {
                            let _: () = print_err(e, file_contents, type_handler);
                            self.correct_stmt(&mut typed_body)?;
                        }*/
                        self.functions.clear_current();
                        let function = TypedStatement::FuncDecl {
                            identifier,
                            args: arg_symbols,
                            ret: signature.ret,
                            body: typed_body,
                        };
                        resolved.push(function);
                    } else {
                        return Err(ResError::new_err(
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
            Statement::VarDecl(ident, ty, mutable, node) => {
                let (typed_child, exp) = if let Some(type_ident) = ty.clone() {
                    let typeid = InferedType {
                        typeid: type_handler.lookup_or_define(type_ident).unwrap(),
                        strong: true,
                    };
                    (
                        self.resolve_node(node.clone(), type_handler, &typeid)?,
                        typeid,
                    )
                } else {
                    let weak_type = InferedType {
                        typeid: TypeID::VOID,
                        strong: false,
                    };
                    (
                        self.resolve_node(node.clone(), type_handler, &weak_type)?,
                        weak_type,
                    )
                };
                let symbol = Symbol {
                    identifier: ident.clone(),
                    typeid: typed_child.infered_type(),
                    mutable,
                };
                let symbol_id = self.define_symbol(ident.clone(), symbol);
                Ok(TypedStatement::VarDecl(ident, symbol_id, typed_child))
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
            Statement::Break(o_node) => {
                if let Some(loop_ctx) = self.loops.last() {
                    if let Some(node) = o_node {
                        if let LoopContext::Loop(t) = loop_ctx {
                            let typed_node = self.resolve_node(node, type_handler, &t.clone())?;
                            match self.loops.last_mut() {
                                Some(LoopContext::Loop(t)) => *t = typed_node.infered_type(),
                                _ => (),
                            };
                            Ok(TypedStatement::Break(
                                typed_node.infered_type(),
                                Some(typed_node),
                            ))
                        } else {
                            Err(ResError::new_err(
                                ResErrorKind::UnexpectedBreak(loop_ctx.clone()),
                                node.position(),
                            ))
                        }
                    } else {
                        Ok(TypedStatement::Break(TypeID::VOID.strong(), None))
                    }
                } else {
                    panic!("cant use break outside of a loop");
                }
            }
            Statement::Expression(node) => {
                let child = self.resolve_node(node, type_handler, &TypeID::VOID.weak())?;
                Ok(TypedStatement::Expression(child.infered_type(), child))
            }
            Statement::While(pre, body) => {
                let pre = self.resolve_node(pre, type_handler, &TypeID::BOOL.strong())?;
                self.loops.push(LoopContext::While);
                let body = self.resolve_block(body, type_handler, &TypeID::VOID.strong())?;
                self.loops.pop();
                Ok(TypedStatement::While(pre, body))
            }
            _ => todo!(),
        }
    }

    pub fn resolve_block(
        &mut self,
        block: Block,
        type_handler: &mut TypeHandler,
        expected: &InferedType,
    ) -> ResResult<TypedBlock> {
        let mut stmts = Vec::new();
        for stmt in block.stmts {
            stmts.push(self.resolve_stmt(stmt, type_handler)?);
        }
        if let Some(tail) = block.tail {
            Ok(TypedBlock {
                stmts,
                tail: Some(Box::new(self.resolve_node(
                    *tail,
                    type_handler,
                    expected,
                )?)),
                position: block.position,
            })
        } else {
            Ok(TypedBlock {
                stmts,
                tail: None,
                position: block.position,
            })
        }
    }
    pub fn resolve_node(
        &mut self,
        node: Node,
        type_handler: &mut TypeHandler,
        expected: &InferedType,
    ) -> ResResult<TypedNode> {
        let mut node = match node {
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
                TypedNode::Litteral(
                    InferedType {
                        typeid,
                        strong: false,
                    },
                    lit,
                    pos,
                )
            }
            Node::Block(block) => {
                self.scopes.push_scope();
                let typed_block = self.resolve_block(block, type_handler, expected)?;
                self.scopes.pop_scope();
                TypedNode::Block(typed_block.infered_type(), typed_block)
            }
            Node::Loop(block) => {
                self.scopes.push_scope();
                self.loops.push(LoopContext::Loop(expected.clone()));
                let typed_block = self.resolve_block(block, type_handler, expected)?;
                let type_id = typed_block.break_type(self.loops.last().unwrap())?;
                self.loops.pop();
                self.scopes.pop_scope();
                TypedNode::Loop(type_id, typed_block)
            }
            Node::If(pre, body, el, pos) => {
                let pre = self.resolve_node(*pre, type_handler, &TypeID::BOOL.strong())?;
                let body = self.resolve_block(body, type_handler, expected)?;
                if let Some(else_body) = el {
                    let el = self.resolve_block(else_body, type_handler, expected)?;
                    let mut t = pick_infer(body.infered_type(), el.infered_type(), pre.position())?;
                    if t.typeid == TypeID::VOID {
                        t.strong = true;
                    }

                    TypedNode::If(t, Box::new(pre), body, Some(el), pos)
                } else {
                    TypedNode::If(body.infered_type(), Box::new(pre), body, None, pos)
                }
            }
            Node::FieldAccess(base, field, pos) => {
                let typed_base = self.resolve_node(*base, type_handler, &TypeID::VOID.weak())?;
                let base_type = type_handler
                    .get(&typed_base.infered_type().typeid, Some(pos))
                    .unwrap();
                if let TypeKind::Structure(struct_id) = base_type.kind {
                    let struc = self.structs.get(struct_id).unwrap();
                    let field_id = struc.lookup(&field, Some(pos)).unwrap();
                    let field_type = struc.get_type(field_id);
                    TypedNode::FieldAccess(field_type.strong(), Box::new(typed_base), field_id, pos)
                } else {
                    return Err(ResError::new_err(
                        ResErrorKind::FieldNotFound(field, typed_base.infered_type().typeid),
                        pos,
                    ));
                }
            }
            Node::Address(mutable, node, pos) => {
                let typed_node = self.resolve_node(*node, type_handler, &TypeID::VOID.weak())?;
                typed_node.infered_type().assert(pos)?;
                let node_type_id = typed_node.infered_type().typeid;
                let node_type = type_handler.get(&node_type_id, Some(pos))?;
                let type_id = type_handler
                    .lookup_or_define(TypeSyntax::Reference {
                        mutable,
                        pointee: Box::new(TypeSyntax::Raw(node_type.identifier.clone())),
                    })
                    .unwrap();
                let mut typed_node =
                    TypedNode::Address(type_id.strong(), mutable, Box::new(typed_node), pos);
                fn referenced_idents(typer: &mut Typer, node: &mut TypedNode) {
                    match node {
                        TypedNode::Identifier(_, id, _) => {
                            typer.symbols.set_referenced(id.clone());
                        }
                        _ => (),
                    }
                }
                self.node_runner(&mut typed_node, &referenced_idents);
                typed_node
            }
            Node::Deref(node, pos) => {
                let typed_node = self.resolve_node(*node, type_handler, &TypeID::VOID.weak())?;
                typed_node.infered_type().assert(pos)?;
                let node_type_id = typed_node.infered_type().typeid;
                let node_type = type_handler.get(&node_type_id, Some(pos))?;
                match node_type.kind {
                    TypeKind::Reference(parent) => {
                        TypedNode::Deref(parent.strong(), Box::new(typed_node), pos)
                    }
                    _ => {
                        return Err(ResError::new_err(
                            ResErrorKind::CantDeref(node_type_id),
                            pos,
                        ));
                    }
                }
            }
            Node::Constructor(type_ident, fields, pos) => {
                let type_id = type_handler
                    .lookup(TypeSyntax::Raw(type_ident.clone()))
                    .unwrap();
                if let TypeKind::Structure(struct_id) =
                    type_handler.get(&type_id, Some(pos)).unwrap().kind
                {
                    let struc = self.structs.get(struct_id).unwrap();
                    let mut typed_fields = Vec::new();
                    for (field, node) in fields {
                        let field_id = struc.lookup(&field, Some(node.position()))?;
                        let field_type = struc.get_type(field_id);
                        let typed_node =
                            self.resolve_node(node, type_handler, &field_type.strong())?;
                        typed_fields.push((field_id, typed_node));
                    }
                    TypedNode::Constructor(type_id, typed_fields, pos)
                } else {
                    return Err(ResError::new_err(
                        ResErrorKind::UnknownType(type_ident),
                        pos,
                    ));
                }
            }
            Node::Assignment(node, child, pos) => {
                let ident = match *node {
                    Node::Identifier(ident, _) => ident,
                    Node::FieldAccess(_, _, _) => todo!(),
                    _ => unreachable!(),
                };
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new_err(
                    ResErrorKind::UnknownVariable(ident.clone()),
                    pos,
                ))?;
                match binding {
                    Binding::Variable(id) => {
                        let symbol: Symbol = self.symbols.get(&id).clone();
                        if !symbol.mutable {
                            return Err(ResError::new_err(
                                ResErrorKind::CantAssignImmutable(symbol),
                                pos,
                            ));
                        }
                        let node = self.resolve_node(*child, type_handler, &symbol.typeid)?;

                        let symbol: &mut Symbol = self.symbols.get_mut(id);

                        if !symbol.typeid.strong && node.infered_type().strong {
                            symbol.typeid = node.infered_type();
                        }
                        if symbol.typeid.typeid != node.infered_type().typeid {
                            return Err(ResError::new_err(
                                ResErrorKind::TypeMismatch(
                                    symbol.typeid.typeid,
                                    node.infered_type().typeid,
                                ),
                                pos,
                            ));
                        } else {
                            TypedNode::Assignment(symbol.typeid.clone(), id, Box::new(node), pos)
                        }
                    }
                    Binding::Function(id) => {
                        return Err(ResError::new_err(
                            ResErrorKind::ExpectedVariable(ident, id),
                            pos,
                        ));
                    }
                }
            }
            Node::Identifier(ident, pos) => {
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new_err(
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
                            return Err(ResError::new_err(
                                ResErrorKind::TypeMismatch(expected.typeid, symbol.typeid.typeid),
                                pos,
                            ));
                        }
                        if !symbol.typeid.strong && expected.strong {
                            symbol.typeid = expected.clone();
                        }
                        TypedNode::Identifier(symbol.typeid.clone(), id, pos)
                    }
                    Binding::Function(id) => {
                        return Err(ResError::new_err(
                            ResErrorKind::ExpectedVariable(ident, id),
                            pos,
                        ));
                    }
                }
            }
            Node::FuncIdentifier(ident, params, pos) => {
                let binding = self.scopes.lookup(ident.clone()).ok_or(ResError::new_err(
                    ResErrorKind::UnknownFunction(ident.clone()),
                    pos,
                ))?;
                match binding {
                    Binding::Function(id) => {
                        let signature = self.functions.get(&id).clone();
                        let mut parsed_args = Vec::new();
                        for (node, ty) in params.iter().zip(signature.params.clone()) {
                            let typed_node =
                                self.resolve_node(node.clone(), type_handler, &ty.strong())?;
                            parsed_args.push(typed_node);
                        }
                        TypedNode::FuncIdentifier(signature.ret, id, parsed_args, pos)
                    }
                    Binding::Variable(id) => {
                        return Err(ResError::new_err(
                            ResErrorKind::ExpectedFunction(ident, id),
                            pos,
                        ));
                    }
                }
            }
            Node::Unary(op, node, pos) => {
                let inner = self.resolve_node(*node, type_handler, expected)?;
                if inner.infered_type().strong
                    && expected.strong
                    && inner.infered_type().typeid != expected.typeid
                {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.typeid, inner.infered_type().typeid),
                        pos,
                    ));
                }
                TypedNode::Unary(inner.infered_type(), op, Box::new(inner), pos)
            }
            Node::Parenthesis(node) => {
                let inner = self.resolve_node(*node, type_handler, expected)?;
                if inner.infered_type().strong
                    && expected.strong
                    && inner.infered_type().typeid != expected.typeid
                {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.typeid, inner.infered_type().typeid),
                        inner.position(),
                    ));
                }
                TypedNode::Parenthesis(inner.infered_type(), Box::new(inner))
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
                    self.correct_node(&mut inner_left, &TypeID::BOOL)?;
                    self.correct_node(&mut inner_right, &TypeID::BOOL)?;
                } else {
                    if expected.typeid.numerical() {
                        self.correct_node(&mut inner_left, &expected.typeid)?;
                        self.correct_node(&mut inner_right, &expected.typeid)?;
                    }
                }
                let type_left = inner_left.infered_type();
                let type_right = inner_right.infered_type();
                if type_left.typeid != type_right.typeid {
                    match (type_left.strong, type_right.strong) {
                        (true, true) => {
                            return Err(ResError::new_err(
                                ResErrorKind::TypeMismatch(type_left.typeid, type_right.typeid),
                                position,
                            ));
                        }
                        (true, false) => self.correct_node(&mut inner_right, &type_left.typeid)?,
                        (false, true) => self.correct_node(&mut inner_left, &type_right.typeid)?,
                        (false, false) => {
                            return Err(ResError::new_err(
                                ResErrorKind::NoTypeInfo(type_left.typeid, type_right.typeid),
                                position,
                            ));
                        }
                    }
                }
                let output_type = if operator.is_boolean_out() {
                    TypeID::BOOL.strong()
                } else {
                    if !expected.strong && expected.typeid == TypeID::VOID {
                        type_left
                    } else {
                        expected.clone()
                    }
                };
                TypedNode::Binary {
                    typeid: output_type,
                    left: Box::new(inner_left),
                    right: Box::new(inner_right),
                    operator,
                    position,
                }
            }
        };
        Ok(node)
    }

    pub fn correct_stmt(&mut self, mut stmt: &mut TypedStatement) -> ResResult<()> {
        match &mut stmt {
            TypedStatement::VarDecl(_, id, n) => {
                let expected = self.symbols.get(&id).typeid.typeid;
                self.correct_node(n, &expected)?;
            }
            TypedStatement::Return(t, Some(n)) => self.correct_node(n, &t.typeid)?,
            TypedStatement::Expression(t, n) => self.correct_node(n, &t.typeid)?,
            _ => (),
        }
        Ok(())
    }

    pub fn correct_block(
        &mut self,
        mut block: &mut TypedBlock,
        expected: &TypeID,
    ) -> ResResult<()> {
        for stmt in &mut block.stmts {
            self.correct_stmt(stmt)?;
        }
        if let Some(tail) = &mut block.tail {
            self.correct_node(tail, expected)?;
        }
        Ok(())
    }

    pub fn correct_node(&mut self, mut node: &mut TypedNode, expected: &TypeID) -> ResResult<()> {
        match &mut node {
            TypedNode::Identifier(t, _, pos) => {
                if t.strong && t.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), t.typeid),
                        pos.clone(),
                    ));
                }
                t.strong = true;
                t.typeid = *expected;
            }
            TypedNode::Constructor(_, _, _) => {}
            TypedNode::FieldAccess(_, _, _, _) => {}
            TypedNode::Address(_, _, _, _) => {}
            TypedNode::Deref(_, _, _) => {}
            TypedNode::Litteral(t, _, pos) => {
                if t.strong && t.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), t.typeid),
                        pos.clone(),
                    ));
                }
                t.strong = true;
                t.typeid = *expected;
            }
            TypedNode::Block(ty, block) => {
                if ty.strong && ty.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), ty.typeid),
                        block.position.clone(),
                    ));
                }
                ty.strong = true;
                ty.typeid = expected.clone();

                for stmt in &mut block.stmts {
                    self.correct_stmt(stmt)?;
                }
                if let Some(tail) = &mut block.tail {
                    self.correct_node(tail, &ty.typeid)?;
                }
            }
            TypedNode::Loop(ty, block) => {
                if ty.strong && ty.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), ty.typeid),
                        block.position.clone(),
                    ));
                }
                ty.strong = true;
                ty.typeid = expected.clone();

                for stmt in &mut block.stmts {
                    self.correct_stmt(stmt)?;
                }
                if let Some(tail) = &mut block.tail {
                    self.correct_node(tail, &ty.typeid)?;
                }
            }
            TypedNode::If(ty, n, stmt1, stmt2, _) => {
                self.correct_node(n, &TypeID::BOOL)?;
                self.correct_block(stmt1, &ty.typeid)?;
                if let Some(stmt) = stmt2 {
                    self.correct_block(stmt, &ty.typeid)?;
                }
            }
            TypedNode::Unary(ty, _, child, pos) => {
                if ty.strong && ty.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), ty.typeid),
                        pos.clone(),
                    ));
                }
                ty.strong = true;
                ty.typeid = expected.clone();
                self.correct_node(child, expected)?;
            }
            TypedNode::Parenthesis(ty, child) => {
                if ty.strong && ty.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), ty.typeid),
                        child.position(),
                    ));
                }
                ty.strong = true;
                ty.typeid = expected.clone();
                self.correct_node(child, expected)?;
            }
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator: _,
                position,
            } => {
                if typeid.strong && typeid.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), typeid.typeid),
                        position.clone(),
                    ));
                }
                typeid.strong = true;
                typeid.typeid = expected.clone();
                match (left.infered_type().strong, right.infered_type().strong) {
                    (true, true) => {
                        self.correct_node(left, &right.infered_type().typeid)?;
                        self.correct_node(right, &left.infered_type().typeid)?;
                    }
                    (false, true) => {
                        self.correct_node(left, &right.infered_type().typeid)?;
                    }
                    (true, false) => {
                        self.correct_node(right, &left.infered_type().typeid)?;
                    }
                    (false, false) => {
                        if left.infered_type().typeid == right.infered_type().typeid {
                            self.correct_node(left, &right.infered_type().typeid)?;
                            self.correct_node(right, &left.infered_type().typeid)?;
                        } else {
                            return Err(ResError::new_warn(
                                ResErrorKind::NoTypeInfo(
                                    right.infered_type().typeid,
                                    left.infered_type().typeid,
                                ),
                                position.clone(),
                            ));
                        }
                    }
                }
            }
            TypedNode::FuncIdentifier(tid, fid, nodes, _) => {
                let sig = self.functions.get(fid);
                for (node, exp) in nodes.iter_mut().zip(sig.params.clone()) {
                    self.correct_node(node, &exp)?;
                }
            }
            TypedNode::Assignment(t, _, node, pos) => {
                if t.strong && t.typeid != expected.clone() {
                    return Err(ResError::new_err(
                        ResErrorKind::TypeMismatch(expected.clone(), t.typeid),
                        pos.clone(),
                    ));
                }
                t.strong = true;
                t.typeid = expected.clone();
                self.correct_node(node, expected)?;
            }
        }
        Ok(())
    }

    pub fn collect_signatures(&mut self, stmts: &Vec<Statement>, type_handler: &mut TypeHandler) {
        for stmt in stmts {
            match stmt {
                Statement::Struct(identifier, _) => {
                    let id = self.structs.define(identifier.clone());
                    let ty = Type {
                        identifier: identifier.clone(),
                        size: 0,
                        kind: TypeKind::Structure(id),
                    };
                    type_handler.define(ty);
                }
                Statement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body: _,
                } => {
                    let mut typed_args = Vec::new();
                    for (_, ty) in args {
                        let t = InferedType {
                            typeid: type_handler.lookup_or_define(ty.clone()).unwrap(),
                            strong: true,
                        };
                        typed_args.push(t.typeid);
                    }
                    let typed_ret = if let Some(ty) = ret {
                        type_handler.lookup(TypeSyntax::Raw(ty.clone())).unwrap()
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

    pub fn tables(self, type_handler: TypeHandler) -> Tables {
        Tables {
            symbol_table: self.symbols,
            type_handler,
            func_table: self.functions,
            scope_table: self.scopes,
            struct_table: self.structs,
        }
    }

    fn strong_stmt(&self, stmt: &TypedStatement) -> ResResult<()> {
        match stmt {
            TypedStatement::Return(t, Some(child)) => {
                self.strong_node(child)?;
                if !t.strong {
                    return Err(ResError::new_err(
                        ResErrorKind::NotInferableStmt,
                        child.position(),
                    ));
                }
            }
            TypedStatement::Return(t, None) => {
                if !t.strong {
                    return Err(ResError::new_err(
                        ResErrorKind::NotInferableStmt,
                        Position::new(0, 0, 0, 0),
                    ));
                }
            }
            TypedStatement::Expression(t, node) => {
                self.strong_node(node)?;
                if !t.strong {
                    return Err(ResError::new_err(
                        ResErrorKind::NotInferableStmt,
                        node.position(),
                    ));
                }
            }
            TypedStatement::VarDecl(_, _, node) => self.strong_node(node)?,
            TypedStatement::Exit(t, n) => {
                self.strong_node(n)?;
                if !t.strong {
                    return Err(ResError::new_err(
                        ResErrorKind::NotInferableStmt,
                        n.position(),
                    ));
                }
            }
            _ => todo!(),
        }
        Ok(())
    }

    fn strong_block(&self, block: &TypedBlock) -> ResResult<()> {
        for stmt in &block.stmts {
            self.strong_stmt(stmt);
        }
        if let Some(tail) = &block.tail {
            self.strong_node(tail);
        }
        Ok(())
    }

    fn strong_node(&self, node: &TypedNode) -> ResResult<()> {
        match node {
            TypedNode::Constructor(_, _, _) => todo!(),
            TypedNode::FieldAccess(_, _, _, _) => todo!(),
            TypedNode::Address(_, _, _, _) => todo!(),
            TypedNode::Deref(_, _, _) => todo!(),
            TypedNode::Unary(t, _, n, pos) => {
                self.strong_node(n)?;
                if !t.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        pos.clone(),
                    ));
                }
            }
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => {
                self.strong_node(left)?;
                self.strong_node(right)?;
                if !typeid.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        position.clone(),
                    ));
                }
            }
            TypedNode::Litteral(t, _, pos) => {
                if !t.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        pos.clone(),
                    ));
                }
            }
            TypedNode::Block(_, block) => {
                for stmt in &block.stmts {
                    self.strong_stmt(stmt)?;
                }
                if let Some(tail) = &block.tail {
                    self.strong_node(tail)?
                }
            }
            TypedNode::Loop(_, block) => {
                for stmt in &block.stmts {
                    self.strong_stmt(stmt)?;
                }
                if let Some(tail) = &block.tail {
                    self.strong_node(tail)?
                }
            }
            TypedNode::If(t, n, stmt, opt_stmt, pos) => {
                self.strong_node(n)?;
                self.strong_block(stmt)?;
                if let Some(stmt2) = opt_stmt {
                    self.strong_block(stmt2)?;
                }
                if !t.strong {
                    return Err(ResError::new_err(
                        ResErrorKind::NotInferableStmt,
                        pos.clone(),
                    ));
                }
            }
            TypedNode::Identifier(t, _, pos) => {
                if !t.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        pos.clone(),
                    ));
                }
            }
            TypedNode::Assignment(t, _, n, pos) => {
                self.strong_node(n)?;
                if !t.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        pos.clone(),
                    ));
                }
            }
            TypedNode::Parenthesis(t, n) => {
                self.strong_node(n)?;
                if !t.strong {
                    return Err(ResError::new_warn(
                        ResErrorKind::NotInferableNode,
                        n.position(),
                    ));
                }
            }
            TypedNode::FuncIdentifier(_, _, nodes, _) => {
                for node in nodes {
                    self.strong_node(node)?;
                }
            }
        }
        Ok(())
    }

    pub fn optimize_stmts(&mut self, stmts: &mut Vec<TypedStatement>) {
        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => {
                    self.optimize_block(body);
                }
                _ => (),
            }
        }
    }

    pub fn optimize_block(&mut self, block: &mut TypedBlock) {
        for stmt in &mut block.stmts {
            self.optimize_stmt(stmt);
        }
        if let Some(tail) = &mut block.tail {
            self.optimize_node(tail);
        }
    }

    pub fn optimize_stmt(&mut self, stmt: &mut TypedStatement) {
        match stmt {
            TypedStatement::VarDecl(_, id, node) => {
                let sym = self.symbols.get(&id);
                if !sym.mutable {
                    self.optimize_node(node);
                    self.optimize_map.insert(id.clone(), node.clone());
                }
            }
            TypedStatement::Return(_, Some(node)) => {
                self.optimize_node(node);
            }
            TypedStatement::Break(_, Some(node)) => {
                self.optimize_node(node);
            }
            TypedStatement::Expression(_, n) => {
                self.optimize_node(n);
            }
            _ => (),
        }
    }

    pub fn optimize_node(&mut self, node: &mut TypedNode) {
        match node {
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => {
                self.optimize_node(left);
                self.optimize_node(right);
                match (left.is_litt(), right.is_litt()) {
                    (Some(l), Some(r)) => {
                        if let Some(litt) = operator.interpret(l, r) {
                            *node = TypedNode::Litteral(typeid.clone(), litt, position.clone())
                        }
                    }
                    _ => (),
                }
            }
            TypedNode::If(_, pred, _, _, _) => {
                self.optimize_node(pred);
            }
            TypedNode::Unary(_, _, child, _) => {
                self.optimize_node(child);
            }
            TypedNode::Assignment(_, _, child, _) => {
                self.optimize_node(child);
            }
            TypedNode::Parenthesis(ty, child) => {
                self.optimize_node(child);
                if let Some(litt) = child.is_litt() {
                    *node = TypedNode::Litteral(child.infered_type(), litt, child.position())
                }
            }
            TypedNode::FuncIdentifier(_, _, nodes, _) => {
                for child in nodes {
                    self.optimize_node(child);
                }
            }
            TypedNode::Identifier(ty, id, pos) => {
                if let Some(child) = self.optimize_map.get(&id) {
                    let mut new_child = child.clone();
                    self.optimize_node(&mut new_child);
                    *node = new_child;
                }
            }
            _ => (),
        }
    }

    pub fn node_runner<T: Fn(&mut Self, &mut TypedNode) + 'static>(
        &mut self,
        node: &mut TypedNode,
        runner: &T,
    ) {
        runner(self, node);
        match node {
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position,
            } => {
                self.node_runner(left, runner);
                self.node_runner(right, runner);
            }
            TypedNode::Unary(_, _, node, _) => self.node_runner(node, runner),
            TypedNode::Parenthesis(_, node) => self.node_runner(node, runner),
            TypedNode::If(_, node, _, _, _) => self.node_runner(node, runner),
            TypedNode::Deref(_, node, _) => self.node_runner(node, runner),
            TypedNode::Address(_, _, node, _) => self.node_runner(node, runner),
            _ => (),
        }
    }
}

pub struct Tables {
    pub symbol_table: SymbolTable,
    pub type_handler: TypeHandler,
    pub func_table: FuncTable,
    pub scope_table: ScopeTable,
    pub struct_table: StructTable,
}
