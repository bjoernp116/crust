use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Add,
    sync::Arc,
};

use crate::{
    asm::Register,
    error::{Counter, ResError, ResErrorKind, ResResult, Severity},
    functions::FuncID,
    lexer::Position,
    locations::{Locator, StackLocation, ValueLocation},
    parser::{Litteral, TypeSyntax},
    structs::{FieldID, StructFrame, StructID, StructTable},
    symbols::{SymbolBinding, SymbolID, SymbolTable},
    types::{
        InferedType, Tables, TypeHandler, TypeID, TypeKind, TypedBlock, TypedNode, TypedStatement,
    },
};

#[derive(Debug)]
pub struct SSA {
    pub functions: Vec<Function>,
}

impl SSA {
    pub fn new(stmts: Vec<TypedStatement>, tables: &mut Tables) -> ResResult<SSA> {
        let mut functions = Vec::new();
        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => functions.push(Function::new(identifier, args, ret, body, tables)?),
                _ => (),
            }
        }
        Ok(Self { functions })
    }
}

pub enum OpOutput {
    Scalar(ValueID),
    Memory(Place),
    Unit,
    Divereged,
}

#[derive(Debug, Clone)]
pub struct LoopMerge {
    ty: TypeID,
    exit: BlockLabel,
    return_slot: Option<SlotID>,
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String,
    pub blocks: Vec<BasicBlock>,
    pub slots: SlotTable,
    pub args: Vec<SymbolID>,
    pub loops: Vec<LoopMerge>,
    pub locator: Locator,
}

impl Function {
    pub fn new(
        identifier: String,
        args: Vec<(SymbolID, TypeID)>,
        ret: TypeID,
        body: TypedBlock,
        tables: &mut Tables,
    ) -> ResResult<Function> {
        let mut blocks = Vec::new();
        let mut slot_table = SlotTable::new();
        let mut locator = Locator::new();
        let mut params = Vec::new();
        blocks.push(BasicBlock::new(BlockLabel(
            identifier.clone(),
            String::from("entry"),
            0,
        )));
        for (sy, ty) in args {
            if tables
                .symbol_table
                .get(&sy)
                .is_scalar(&tables.type_handler)?
            {
                locator.new_symbol(ty, sy);
                params.push(sy);
            } else {
                slot_table.new_var(ty, sy);
                params.push(sy);
            }
        }
        let mut func = Function {
            blocks,
            slots: slot_table,
            identifier,
            args: params,
            loops: Vec::new(),
            locator,
        };
        for stmt in body.stmts {
            func.parse_stmt(stmt, tables)?;
        }
        if let Some(tail) = body.tail {
            let value = func.parse_value(*tail, tables)?;
            func.set_terminator(Terminator::Return { value: Some(value) }, body.position)?;
        }
        //func.remove_dead_copies(tables);
        Ok(func)
    }

    pub fn parse_stmt(&mut self, stmt: TypedStatement, tables: &mut Tables) -> ResResult<()> {
        match stmt {
            TypedStatement::VarDecl(_, symbol_id, node) => {
                node.infered_type().assert(node.position())?;
                if tables
                    .symbol_table
                    .can_value_allocate(&symbol_id, &tables.type_handler)?
                {
                    println!("value");
                    let symbol = tables.symbol_table.get(&symbol_id).clone();
                    let value_id = self.locator.new_symbol(symbol.typeid.typeid, symbol_id);
                    self.parse_into(node, value_id, tables)?;
                } else {
                    let symbol = tables.symbol_table.get(&symbol_id).clone();
                    let node_value = self.parse_value(node, tables)?;
                    let slot_id = self.slots.new_var(symbol.typeid.typeid, symbol_id);
                    let destination = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 0,
                        typeid: symbol.typeid.typeid,
                    };
                    let op = Operation::Store(destination, node_value);
                    self.push(op);
                    self.locator.drop(node_value);
                }
            }
            TypedStatement::Return(_, opt) => {
                if let Some(child) = opt {
                    let value = self.parse_value(child.clone(), tables)?;
                    self.set_terminator(Terminator::Return { value: Some(value) }, child.position())?;
                } else {
                    self.set_terminator(Terminator::Return { value: None }, Position::new(0, 0, 0, 0))?;
                }
            }
            TypedStatement::Break(_, opt) => {
                let loop_merge = self.loops.last().unwrap().clone();
                if let Some(child) = opt {
                    if let Some(return_slot) = loop_merge.return_slot {
                        let child_type = child.infered_type().typeid;
                        let place = self.parse_place(child, tables)?;
                        todo!()
                    } else {
                        let place = self.parse_node(child, tables)?;
                        todo!()
                    }
                    self.set_terminator(Terminator::Jump { destination: loop_merge.exit.clone() }, child.position())?;
                }
            }
            TypedStatement::Expression(ty, node) => {
                node.infered_type().assert(node.position())?;
                self.parse_node(node, tables)?;
            }
            TypedStatement::While(pre, body) => {
                let predicate = self.parse_value(pre.clone(), tables)?;
                let loop_label = BlockLabel(
                    self.identifier.clone(),
                    format!("while_loop"),
                    self.blocks.len(),
                );
                let merge_label = BlockLabel(
                    self.identifier.clone(),
                    format!("while_merge"),
                    self.blocks.len() + 1,
                );
                self.set_terminator(Terminator::Branch { predicate, destination: loop_label.clone(), inverse: merge_label.clone() }, body.position)?;

                self.blocks.push(BasicBlock::new(loop_label.clone()));
                self.loops.push(LoopMerge {
                    ty: TypeID::VOID,
                    exit: merge_label.clone(),
                    return_slot: None,
                });
                self.parse_block(body.clone(), TypeID::VOID, tables)?;
                self.loops.pop();
                if let Terminator::Unknown = self.get_terminator() {
                    let continue_value = self.parse_value(pre, tables)?;
                    self.set_terminator(Terminator::Branch { predicate: continue_value, destination: loop_label.clone(), inverse: merge_label.clone() }, body.position)?;
                }

                self.blocks.push(BasicBlock::new(merge_label.clone()));
            }
            _ => (),
        }
        Ok(())
    }

    pub fn parse_block(
        &mut self,
        block: TypedBlock,
        ty: TypeID,
        tables: &mut Tables,
    ) -> ResResult<OpOutput> {
        let slot = self.locator.new_value(ty);
        for stmt in block.stmts {
            self.parse_stmt(stmt, tables)?;
        }
        if let Some(tail) = block.tail {
            Ok(self.parse_node(*tail, tables)?)
        } else {
            Ok(OpOutput::Unit)
        }
    }

    pub fn parse_place(&mut self, node: TypedNode, tables: &mut Tables) -> ResResult<Place> {
        match node {
            TypedNode::Identifier(ty, sym, _) => {
                if tables
                    .symbol_table
                    .can_value_allocate(&sym, &tables.type_handler)?
                {
                    let destination = Place {
                        kind: PlaceKind::Slot(self.slots.new_var(ty.typeid, sym)),
                        offset: 0,
                        typeid: ty.typeid,
                    };
                    let src = self.locator.lookup(&sym).unwrap();
                    let op = Operation::Store(destination.clone(), src.clone());
                    self.push(op);
                    Ok(destination)
                } else {
                    Ok(Place {
                        kind: PlaceKind::Slot(self.slots.lookup(&sym).unwrap().clone()),
                        offset: 0,
                        typeid: ty.typeid,
                    })
                }
            }
            TypedNode::FieldAccess(ty, base, field, pos) => {
                let base_place = self.parse_place(*base, tables)?;
                if let TypeKind::Structure(struct_id) =
                    tables.type_handler.get(&ty.typeid, Some(pos)).unwrap().kind
                {
                    let struc = tables.struct_table.get(struct_id).unwrap();
                    let field_type = struc.get_type(field);
                    let field_offset = struc.get_offset(field, &tables.type_handler);
                    Ok(Place {
                        kind: base_place.kind,
                        offset: base_place.offset + field_offset,
                        typeid: field_type,
                    })
                } else {
                    Err(ResError::new_err(ResErrorKind::ExpectedStruct, pos))
                }
            }
            TypedNode::Deref(ty, node, _) => {
                let pointer: ValueID = self.parse_value(*node, tables)?;

                Ok(Place {
                    kind: PlaceKind::Pointer(pointer),
                    offset: 0,
                    typeid: ty.typeid,
                })
            }
            _ => Err(ResError::new_err(
                ResErrorKind::ExpectedPlace(node.clone()),
                node.position(),
            )),
        }
    }
    pub fn parse_value(&mut self, node: TypedNode, tables: &mut Tables) -> ResResult<ValueID> {
        match self.parse_node(node.clone(), tables)? {
            OpOutput::Scalar(value) => Ok(value),
            OpOutput::Memory(place) => {
                if !tables.type_handler.is_scalar(&place.typeid)? {
                    return Err(ResError::new_err(
                        ResErrorKind::ExpectedScalar(node.clone()),
                        node.position(),
                    ));
                }

                let destination = self.locator.new_value(place.typeid);

                self.push(Operation::Load(destination, place));

                Ok(destination)
            }
            OpOutput::Unit => Ok(self.locator.new_value(TypeID::VOID)),
            _ => todo!()
        }
    }
    pub fn parse_node(&mut self, node: TypedNode, tables: &mut Tables) -> ResResult<OpOutput> {
        match node.clone() {
            TypedNode::Constructor(id, fields, pos) => {
                if let TypeKind::Structure(struct_id) =
                    tables.type_handler.get(&id, Some(pos)).unwrap().kind
                {
                    let struc = tables.struct_table.get(struct_id).unwrap();
                    let destination = self.slots.new_temp(id);
                    let destination_place = Place {
                        kind: PlaceKind::Slot(destination),
                        offset: 0,
                        typeid: id,
                    };
                    for (field_id, node) in fields {
                        let field_type = struc.get_type(field_id);
                        let field_offset = struc.get_offset(field_id, &tables.type_handler);
                        let out_slot = self.parse_place(node, tables)?;
                        let field = Place {
                            kind: PlaceKind::Slot(destination),
                            offset: field_offset,
                            typeid: field_type,
                        };
                        self.copy_or_load(&field_type, field, out_slot, tables);
                    }
                    Ok(OpOutput::Memory(destination_place))
                } else {
                    Err(ResError::new_err(ResErrorKind::ExpectedStruct, pos))
                }
            }
            TypedNode::Litteral(ty, litt, _) => match litt {
                Litteral::String(string) => {
                    let slot_id = self.slots.new_temp(ty.typeid);
                    let slot_base = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 0,
                        typeid: ty.typeid,
                    };

                    let buffer_value = self.locator.new_value(ty.typeid);
                    let op = Operation::ConstStr(buffer_value, string.clone());
                    self.push(op);
                    let buffer_field = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 0,
                        typeid: tables.type_handler.u8_ref(),
                    };
                    let op = Operation::Store(buffer_field, buffer_value);
                    self.locator.drop(buffer_value);
                    self.push(op);
                    let length_value = self.locator.new_value(TypeID::U64);
                    let op = Operation::Const(length_value, Litteral::Number(string.len()));
                    self.push(op);
                    let length_field = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 8,
                        typeid: TypeID::U64,
                    };
                    let op = Operation::Store(length_field, length_value);
                    self.locator.drop(length_value);
                    self.push(op);
                    let place = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 0,
                        typeid: ty.typeid,
                    };
                    Ok(OpOutput::Memory(place))
                }
                _ => {
                    let value = self.locator.new_value(ty.typeid);
                    let op = Operation::Const(value, litt);
                    self.push(op);
                    Ok(OpOutput::Scalar(value))
                }
            },
            TypedNode::Block(ty, block) => {
                self.parse_block(block, ty.typeid, tables)?;
                todo!()
            }
            TypedNode::Loop(ty, block) => {
                let loop_label = BlockLabel(
                    self.identifier.clone(),
                    format!("loop_loop"),
                    self.blocks.len(),
                );
                let merge_label = BlockLabel(
                    self.identifier.clone(),
                    format!("loop_merge"),
                    self.blocks.len() + 1,
                );
                self.set_terminator(Terminator::Jump { destination: loop_label.clone() }, block.position)?;
                let return_slot = self.slots.new_temp(ty.typeid);
                self.loops.push(LoopMerge {
                    ty: ty.typeid,
                    exit: merge_label.clone(),
                    return_slot: Some(return_slot),
                });
                self.blocks.push(BasicBlock::new(loop_label.clone()));
                let _ = self.parse_block(block.clone(), ty.typeid, tables)?;
                self.loops.pop();
                if let Terminator::Unknown = self.get_terminator() {
                    self.set_terminator(Terminator::Jump { destination: loop_label }, block.position)?;
                }

                self.blocks.push(BasicBlock::new(merge_label.clone()));
                //Ok(PlaceKind::Slot(return_slot));
                todo!()
            }
            TypedNode::If(ty, predicate, body, el, pos) => {
                let predicate = self.parse_value(*predicate, tables)?;
                self.locator.drop(predicate);
                let then_label = BlockLabel(
                    self.identifier.clone(),
                    format!("if_then"),
                    self.blocks.len(),
                );
                let merge_label = BlockLabel(
                    self.identifier.clone(),
                    format!("if_merge"),
                    self.blocks.len() + 2,
                );
                if let Some(else_body) = el {
                    let else_label = BlockLabel(
                        self.identifier.clone(),
                        format!("if_else"),
                        self.blocks.len() + 1,
                    );
                    self.set_terminator(Terminator::Branch { predicate, destination: then_label.clone(), inverse: else_label.clone() }, pos)?;
                    self.blocks.push(BasicBlock::new(then_label.clone()));
                    let then_res = self.parse_block(body.clone(), ty.typeid, tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump { destination: merge_label.clone() }, body.position)?;
                    }

                    self.blocks.push(BasicBlock::new(else_label.clone()));
                    let else_res = self.parse_block(else_body.clone(), ty.typeid, tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump { destination: merge_label.clone() }, else_body.position)?;
                    }
                    self.blocks.push(BasicBlock::new(merge_label));
                    match (then_res, else_res) {
                        (OpOutput::Scalar(then_value), OpOutput::Scalar(else_value)) => {
                            let then_type = self.locator.get(&then_value).typeid;
                            let else_type = self.locator.get(&else_value).typeid;
                            if then_type != else_type {
                                return Err(ResError::new_err(
                                    ResErrorKind::TypeMismatch(then_type, else_type),
                                    pos,
                                ));
                            }
                            let out_value = self.locator.new_value(then_type);
                            let op = Operation::Move(out_value, then_value);
                            self.blocks[then_label.2].operations.push(op);

                            let op = Operation::Move(out_value, else_value);
                            self.blocks[else_label.2].operations.push(op);

                            Ok(OpOutput::Scalar(out_value))
                        }
                        (
                            OpOutput::Memory(then_place),
                            OpOutput::Memory(else_place),
                        ) => {
                            if &then_place.typeid != &else_place.typeid || &then_place.offset != &else_place.offset {
                                return Err(ResError::new_err(
                                    ResErrorKind::TypeMismatch(then_place.typeid, else_place.typeid),
                                    pos,
                                ));
                            }

                            let out_place = Place {
                                kind: PlaceKind::Slot(self.slots.new_temp(then_place.typeid)),
                                offset: then_place.offset,
                                typeid: then_place.typeid
                                
                            };
                            let op = Operation::Copy(out_place.clone(), then_place.clone(), then_place.typeid);
                            self.blocks[then_label.2].operations.push(op);

                            let op = Operation::Copy(out_place.clone(), else_place.clone(), else_place.typeid);
                            self.blocks[else_label.2].operations.push(op);

                            Ok(OpOutput::Memory(out_place))
                        }
                        (OpOutput::Unit, OpOutput::Unit) => Ok(OpOutput::Unit),
                        _ => todo!()
                    }
                } else {
                    self.set_terminator(Terminator::Branch { predicate, destination: then_label.clone(), inverse: merge_label.clone() }, pos)?;

                    let out_slot_id = self.slots.new_temp(ty.typeid);
                    self.blocks.push(BasicBlock::new(then_label));
                    if let Some(tail) = body.tail.clone() {
                        if !tail.infered_type().strong || tail.infered_type().typeid == TypeID::VOID
                        {
                            return Err(ResError::new_err(
                                ResErrorKind::ExpectedElse(tail.infered_type().typeid),
                                pos,
                            ));
                        }
                    }
                    let then_res = self.parse_block(body, ty.typeid, tables)?;
                    if let Terminator::Unknown = self.get_terminator(){
                        self.blocks.push(BasicBlock::new(merge_label));
                    }
                    Ok(OpOutput::Unit)
                }
            }
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position: _,
            } => {
                let left_slot = self.parse_value(*left, tables)?;
                let right_slot = self.parse_value(*right, tables)?;
                let slot_id = self.locator.new_value(typeid.typeid);
                let op = operator.to_operation(typeid.typeid, slot_id, left_slot, right_slot);
                self.push(op);
                self.locator.drop(left_slot);
                self.locator.drop(right_slot);
                Ok(OpOutput::Scalar(slot_id))
            }
            TypedNode::Identifier(ty, sym, _) => {
                if tables
                    .symbol_table
                    .can_value_allocate(&sym, &tables.type_handler)?
                {
                    if let Some(value) = self.locator.lookup(&sym) {
                        Ok(OpOutput::Scalar(value.clone()))
                    } else {
                        let source: Place = self.parse_place(node, tables)?;
                        let destination: ValueID = self.locator.new_value(ty.typeid);

                        self.push(Operation::Load(destination, source));
                        Ok(OpOutput::Scalar(destination))
                    }
                } else {
                    let place = self.parse_place(node, tables)?;
                    Ok(OpOutput::Memory(place))
                }
            }
            TypedNode::FieldAccess(ty, base, field_id, _) => {
                if tables.type_handler.is_scalar(&ty.typeid)? {
                    let source: Place = self.parse_place(node, tables)?;
                    let destination: ValueID = self.locator.new_value(ty.typeid);

                    self.push(Operation::Load(destination, source));
                    Ok(OpOutput::Scalar(destination))
                } else {
                    let place = self.parse_place(node, tables)?;
                    Ok(OpOutput::Memory(place))
                }
            }
            TypedNode::Deref(ty, _, pos) => {
                if tables.type_handler.is_scalar(&ty.typeid)? {
                    let source: Place = self.parse_place(node, tables)?;
                    let destination: ValueID = self.locator.new_value(ty.typeid);

                    self.push(Operation::Load(destination, source));
                    Ok(OpOutput::Scalar(destination))
                } else {
                    let place = self.parse_place(node, tables)?;
                    Ok(OpOutput::Memory(place))
                }
            }
            TypedNode::Address(ty, _, child, _) => {
                let source: Place = self.parse_place(*child, tables)?;
                let destination: ValueID = self.locator.new_value(ty.typeid);

                self.push(Operation::AddressOf(destination, source));
                Ok(OpOutput::Scalar(destination))
            }
            TypedNode::Unary(ty, operator, child, _) => {
                let child_slot = self.parse_value(*child, tables)?;
                let slot_id = self.locator.new_value(ty.typeid);
                let op = operator.to_operation(ty.typeid, slot_id, child_slot);
                self.push(op);
                Ok(OpOutput::Scalar(slot_id))
            }
            TypedNode::Assignment(ty, sym, child, _) => {
                let value = self.parse_value(*child, tables)?;
                let slot_id = self.slots.lookup(&sym).unwrap().clone();
                let place = Place {
                    kind: PlaceKind::Slot(slot_id),
                    offset: 0,
                    typeid: ty.typeid,
                };
                self.push(Operation::Store(place.clone(), value));
                self.locator.drop(value);
                Ok(OpOutput::Memory(place))
            }
            TypedNode::Parenthesis(ty, child) => self.parse_node(*child, tables),
            TypedNode::FuncIdentifier(ty, id, args, pos) => {
                let mut ids = Vec::new();
                for arg in args {
                    let value_id = self.parse_value(arg, tables)?;
                    ids.push(value_id);
                }
                let value = self.locator.new_value(ty);
                let op = Operation::Call(value, id, ids);
                self.push(op);
                if ty == TypeID::NEVER {
                    self.set_terminator(Terminator::Unreachable, pos)?;
                }
                Ok(OpOutput::Scalar(value))
            }
        }
    }

    fn parse_into(
        &mut self,
        node: TypedNode,
        value: ValueID,
        tables: &mut Tables,
    ) -> ResResult<()> {
        match node {
            TypedNode::Litteral(ty, litt, pos) => {
                if let Litteral::String(_) = litt {
                    panic!("expected number");
                }
                let op = Operation::Const(value, litt);
                self.push(op);
            },
            TypedNode::If(ty, predicate, body, el, pos) => {
                let predicate = self.parse_value(*predicate, tables)?;
                let then_label = BlockLabel(
                    self.identifier.clone(),
                    format!("if_then"),
                    self.blocks.len(),
                );
                let merge_label = BlockLabel(
                    self.identifier.clone(),
                    format!("if_merge"),
                    self.blocks.len() + 2,
                );
                if let Some(else_body) = el {
                    let else_label = BlockLabel(
                        self.identifier.clone(),
                        format!("if_else"),
                        self.blocks.len() + 1,
                    );
                    self.set_terminator(Terminator::Branch { predicate, destination: then_label.clone(), inverse: else_label.clone() }, pos)?;
                    self.blocks.push(BasicBlock::new(then_label.clone()));
                    let then_res = self.parse_block(body.clone(), ty.typeid, tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump { destination: merge_label.clone() }, body.position)?;
                    }

                    self.blocks.push(BasicBlock::new(else_label.clone()));
                    let else_res = self.parse_block(else_body.clone(), ty.typeid, tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump { destination: merge_label.clone() }, else_body.position)?;
                    }
                    self.blocks.push(BasicBlock::new(merge_label));
                    match (then_res, else_res) {
                        (OpOutput::Scalar(then_value), OpOutput::Scalar(else_value)) => {
                            let then_type = self.locator.get(&then_value).typeid;
                            let else_type = self.locator.get(&else_value).typeid;
                            if then_type != else_type {
                                return Err(ResError::new_err(
                                    ResErrorKind::TypeMismatch(then_type, else_type),
                                    pos,
                                ));
                            }
                            let op = Operation::Move(value, then_value);
                            self.blocks[then_label.2].operations.push(op);

                            let op = Operation::Move(value, else_value);
                            self.blocks[else_label.2].operations.push(op);
                        },
                        _ => todo!()
                    }
                }
            }

            _ => todo!(),
        }

        Ok(())
    }

    fn push(&mut self, operation: Operation) {
        let block: &mut BasicBlock = self.blocks.last_mut().unwrap();
        block.operations.push(operation);
    }

    pub fn set_terminator(&mut self, terminator: Terminator, pos: Position) -> ResResult<()> {
        let block: &mut BasicBlock = self.blocks.last_mut().unwrap();
        match block.terminator {
            Terminator::Unknown => block.terminator = terminator,
            Terminator::Return { value: _ }=> {
                return Err(ResError::new_warn(ResErrorKind::ControlFlowExited, pos));
            }
            _ => (),
        };
        Ok(())
    }

    pub fn get_terminator(&self) -> Terminator {
        let block: &BasicBlock = self.blocks.last().unwrap();
        block.terminator.clone()
    }

    pub fn copy_or_load(
        &mut self,
        type_id: &TypeID,
        destination: Place,
        source: Place,
        tables: &mut Tables,
    ) {
        let ty = tables.type_handler.get(type_id, None).unwrap();
        if ty.size <= 8 {
            let value = self.locator.new_value(type_id.clone());
            self.push(Operation::Load(value, source));
            self.push(Operation::Store(destination, value));
            self.locator.drop(value);
        } else {
            self.push(Operation::Copy(destination, source, type_id.clone()))
        }
    }
}

#[derive(Debug, Clone)]
pub enum Terminator {
    Unknown,
    Unreachable,
    Jump { destination: BlockLabel },
    Branch { predicate: ValueID, destination: BlockLabel, inverse: BlockLabel },
    Return { value: Option<ValueID> },
}

pub struct BasicBlock {
    pub label: BlockLabel,
    pub operations: Vec<Operation>,
    pub terminator: Terminator,
}

impl Debug for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:?}:", self.label)?;
        for op in &self.operations {
            writeln!(f, "\t{:?}", op)?;
        }
        writeln!(f, "\t{:?}", self.terminator)?;

        Ok(())
    }
}

impl BasicBlock {
    pub fn new(label: BlockLabel) -> Self {
        Self {
            label,
            operations: Vec::new(),
            terminator: Terminator::Unknown,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    AddressOf(ValueID, Place),
    Load(ValueID, Place),
    Store(Place, ValueID),
    Move(ValueID, ValueID),
    Deref(ValueID, ValueID),
    Copy(Place, Place, TypeID),
    Const(ValueID, Litteral),
    ConstStr(ValueID, String),
    Eq(TypeID, ValueID, ValueID, ValueID),
    G(TypeID, ValueID, ValueID, ValueID),
    L(TypeID, ValueID, ValueID, ValueID),
    GEq(TypeID, ValueID, ValueID, ValueID),
    LEq(TypeID, ValueID, ValueID, ValueID),
    NEq(TypeID, ValueID, ValueID, ValueID),
    Add(TypeID, ValueID, ValueID, ValueID),
    Sub(TypeID, ValueID, ValueID, ValueID),
    Div(TypeID, ValueID, ValueID, ValueID),
    Mul(TypeID, ValueID, ValueID, ValueID),
    Not(TypeID, ValueID, ValueID),
    Neg(TypeID, ValueID, ValueID),
    Call(ValueID, FuncID, Vec<ValueID>),
}

#[derive(Debug, Clone)]
pub struct Place {
    pub kind: PlaceKind,
    pub offset: usize,
    pub typeid: TypeID,
}

impl Place {
    fn with_offset(&self, offset: usize, typeid: TypeID) -> Self {
        Place {
            kind: self.kind.clone(),
            offset,
            typeid,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PlaceKind {
    Slot(SlotID),
    Pointer(ValueID),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SlotID(pub usize);

impl SlotID {
    pub const MAX: SlotID = SlotID(usize::MAX);
}

#[derive(Debug, Clone)]
pub struct StackFrame {
    pub stack_map: HashMap<SlotID, StackLocation>,
    pub size: usize,
}

#[derive(Debug)]
pub struct SlotTable {
    slots: Vec<Slot>,
    bindings: HashMap<SymbolID, SlotID>,
    value_counter: usize,
}

impl SlotTable {
    pub fn new() -> SlotTable {
        SlotTable {
            slots: Vec::new(),
            value_counter: 0,
            bindings: HashMap::new(),
        }
    }

    pub fn new_var(&mut self, type_id: TypeID, symbol_id: SymbolID) -> SlotID {
        let slot_id = SlotID(self.slots.len());
        self.slots.push(Slot {
            ty: type_id,
            kind: SlotKind::Local(symbol_id),
        });
        self.bindings.insert(symbol_id, slot_id);
        slot_id
    }

    pub fn new_temp(&mut self, type_id: TypeID) -> SlotID {
        let slot_id = SlotID(self.slots.len());
        self.slots.push(Slot {
            ty: type_id,
            kind: SlotKind::Temporary,
        });
        slot_id
    }

    pub fn get(&self, id: SlotID) -> &Slot {
        &self.slots[id.0]
    }

    pub fn lookup(&self, id: &SymbolID) -> Option<&SlotID> {
        self.bindings.get(id)
    }

    pub fn stack_frame(&self, type_handler: &TypeHandler) -> StackFrame {
        let mut map = HashMap::new();
        let mut used = 0;
        for (i, slot) in self.slots.iter().enumerate() {
            if slot.ty == TypeID::VOID {
                continue;
            }
            let ty = type_handler.get(&slot.ty, None).unwrap();

            let alignment = ty.size;

            used = align_up(used, alignment);

            used += ty.size;
            let location = StackLocation {
                offset: used,
                size: ty.size,
                pointer: false,
            };
            map.insert(SlotID(i), location);
        }
        used += 16 - (used % 16);
        StackFrame {
            stack_map: map,
            size: used,
        }
    }

    pub fn place_location(
        &self,
        place: PlaceKind,
        structs: &StructTable,
        type_handler: &TypeHandler,
    ) -> ResResult<StackLocation> {
        let frame = self.stack_frame(type_handler);
        match place {
            PlaceKind::Slot(s) => frame
                .stack_map
                .get(&s)
                .ok_or(ResError {
                    kind: ResErrorKind::SlotNotFound(s),
                    position: None,
                    severity: Severity::Error,
                })
                .map(|s| s.clone()),
            PlaceKind::Pointer(place) => {
                todo!()
            }
        }
    }
}

pub fn align_up(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

#[derive(Debug)]
pub struct Slot {
    pub ty: TypeID,
    //pub alignment: u32,
    pub kind: SlotKind,
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub struct ValueID(pub usize);

#[derive(Debug)]
pub enum SlotKind {
    Local(SymbolID),
    Temporary,
}

#[derive(Clone)]
pub struct BlockLabel(String, String, usize);

impl BlockLabel {
    pub fn entry(&self) -> bool {
        self.2 == 0
    }

    pub fn ident(&self) -> &String {
        &self.0
    }
}

impl Debug for BlockLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}
