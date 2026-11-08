mod flow;
mod generator;
mod operations;
mod places;
mod values;

pub use operations::Operation;
pub use places::Place;
pub use places::PlaceKind;
pub use places::SlotID;
pub use places::SlotTable;
pub use places::StackFrame;
pub use places::StackLocation;
pub use places::align_up;
pub use values::Locator;
pub use values::RegisterSet;
pub use values::ValueID;
pub use values::ValueLocation;

use std::{
    any::Any,
    collections::HashMap,
    fmt::{Debug, Display},
    ops::Add,
    sync::Arc,
};

use crate::error::AnyResult;
use crate::error::IRError;
use crate::error::IRErrorKind;
use crate::error::TypeError;
use crate::error::TypeErrorKind;
use crate::error::TypeResult;
use crate::symbols::Symbol;
use crate::{
    asm::Register,
    error::{Counter, CrustError, Severity, print_err, unwrap_print},
    functions::FuncID,
    lexer::Position,
    parser::{Litteral, TypeSyntax},
    structs::{FieldID, StructFrame, StructID, StructTable},
    symbols::{SymbolBinding, SymbolID, SymbolTable},
    types::{
        Assignee, InferedType, Tables, TypeHandler, TypeID, TypeKind, TypedBlock, TypedNode,
        TypedStatement,
    },
};

#[derive(Debug)]
pub struct SSA {
    pub functions: Vec<Function>,
}

impl SSA {
    pub fn new(stmts: Vec<TypedStatement>, tables: &mut Tables) -> AnyResult<SSA> {
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
    pub ret_place: Option<Place>,
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
    ) -> AnyResult<Function> {
        let mut blocks = Vec::new();
        let mut slot_table = SlotTable::new();
        let mut locator = Locator::new();
        let mut params = Vec::new();
        blocks.push(BasicBlock::new(BlockLabel(
            identifier.clone(),
            String::from("entry"),
            0,
        )));
        let ret_place = if !tables.type_handler.is_scalar(&ret)? {
            let ret_ty = tables.type_handler.get(&ret, None)?;
            let syntax = TypeSyntax::Reference {
                mutable: true,
                pointee: Box::new(TypeSyntax::Raw(ret_ty.identifier.clone())),
            };
            let ret_ref = tables.type_handler.lookup_or_define(syntax).unwrap();
            let id = tables.symbol_table.insert(Symbol {
                identifier: "@".to_owned(),
                typeid: ret_ref.strong(),
                mutable: false,
            });
            let value = locator.new_symbol(ret_ref, id, &mut slot_table);
            params.push(id);
            Some(Place {
                kind: PlaceKind::Pointer(value),
                offset: 0,
                typeid: ret,
            })
        } else {
            None
        };
        for (sy, ty) in args {
            if tables
                .symbol_table
                .get(&sy)
                .is_scalar(&tables.type_handler)?
            {
                locator.new_symbol(ty, sy, &mut slot_table);
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
            ret_place,

            args: params,
            loops: Vec::new(),
            locator,
        };
        for stmt in body.stmts {
            if let Terminator::Unknown = func.get_terminator() {
            } else {
                let _: () = print_err(
                    IRError::new_err(IRErrorKind::Unreachable, stmt.position()),
                    &tables.file_contents,
                    &tables.type_handler,
                );
            }
            func.parse_stmt(stmt, tables)?;
        }
        if let Some(tail) = body.tail {
            let value = func.parse_value(*tail, tables)?;
            func.set_terminator(Terminator::ReturnValue { value: Some(value) })?;
        } else {
            if let Terminator::Unknown = func.get_terminator() {
                if ret != TypeID::VOID {
                    return Err(IRError::new_err(
                        IRErrorKind::ExpectedReturnValue(ret),
                        body.position,
                    ).to_any());
                }
            }
        }
        //func.remove_dead_copies(tables);
        Ok(func)
    }

    pub fn parse_stmt(&mut self, stmt: TypedStatement, tables: &mut Tables) -> AnyResult<()> {
        match stmt {
            TypedStatement::VarDecl(ident, symbol_id, node) => {
                node.infered_type().assert(node.position())?;
                if tables
                    .symbol_table
                    .can_value_allocate(&symbol_id, &tables.type_handler)?
                {
                    let symbol = tables.symbol_table.get(&symbol_id).clone();
                    let value_id =
                        self.locator
                            .new_symbol(symbol.typeid.typeid, symbol_id, &mut self.slots);
                    self.parse_into(node, value_id, tables)?;
                } else {
                    let symbol = tables.symbol_table.get(&symbol_id).clone();
                    let node_place = self.parse_place(node, tables)?;
                    let slot_id = self.slots.new_var(symbol.typeid.typeid, symbol_id);
                    let destination = Place {
                        kind: PlaceKind::Slot(slot_id),
                        offset: 0,
                        typeid: symbol.typeid.typeid,
                    };
                    let op = Operation::Copy(destination, node_place, symbol.typeid.typeid);
                    self.push(op);
                }
            }
            TypedStatement::Return(_, opt) => {
                if let Some(child) = opt {
                    if let Some(ret_place) = self.ret_place.clone() {
                        let src = self.parse_place(child, tables)?;
                        let op = Operation::Copy(ret_place.clone(), src, ret_place.typeid);
                        self.push(op);
                        self.set_terminator(Terminator::ReturnValue { value: None })?;
                    } else {
                        let value = self.parse_value(child.clone(), tables)?;
                        self.set_terminator(Terminator::ReturnValue { value: Some(value) })?;
                    }
                } else {
                    self.set_terminator(Terminator::ReturnValue { value: None })?;
                }
            }
            TypedStatement::Break(_, opt) => {
                let loop_merge = self.loops.last().unwrap().clone();
                if let Some(child) = opt {
                    if let Some(return_slot) = loop_merge.return_slot {
                        let child_type = child.infered_type().typeid;
                    } else {
                        let place = self.parse_node(child, tables)?;
                        todo!()
                    }
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
                self.set_terminator(Terminator::Branch {
                    predicate,
                    destination: loop_label.clone(),
                    inverse: merge_label.clone(),
                })?;
                self.locator.drop(predicate);
                self.blocks.push(BasicBlock::new(loop_label.clone()));
                self.loops.push(LoopMerge {
                    ty: TypeID::VOID,
                    exit: merge_label.clone(),
                    return_slot: None,
                });
                self.parse_block(body.clone(), tables)?;
                self.loops.pop();
                if let Terminator::Unknown = self.get_terminator() {
                    let continue_value = self.parse_value(pre, tables)?;
                    self.set_terminator(Terminator::Branch {
                        predicate: continue_value,
                        destination: loop_label.clone(),
                        inverse: merge_label.clone(),
                    })?;
                }
                self.locator.drop(predicate);

                self.blocks.push(BasicBlock::new(merge_label.clone()));
            }
            _ => (),
        }
        Ok(())
    }

    pub fn parse_block(&mut self, block: TypedBlock, tables: &mut Tables) -> AnyResult<OpOutput> {
        for stmt in block.stmts {
            if let Terminator::Unknown = self.get_terminator() {
            } else {
                let _: () = print_err(
                    IRError::new_err(IRErrorKind::Unreachable, stmt.position()),
                    &tables.file_contents,
                    &tables.type_handler,
                );
            }
            self.parse_stmt(stmt, tables)?;
        }
        if let Some(tail) = block.tail {
            Ok(self.parse_node(*tail, tables)?)
        } else {
            Ok(OpOutput::Unit)
        }
    }

    pub fn parse_place(&mut self, node: TypedNode, tables: &mut Tables) -> AnyResult<Place> {
        match node.clone() {
            TypedNode::FuncIdentifier(ty, id, args, pos) => {
                let func = tables.func_table.get(&id).clone();
                let mut ids = Vec::new();
                if !func.big_ret {
                    panic!("Expected big return type!");
                }
                let ret_ty = tables.type_handler.get(&func.ret, None)?;
                let syntax = TypeSyntax::Reference {
                    mutable: true,
                    pointee: Box::new(TypeSyntax::Raw(ret_ty.identifier.clone())),
                };
                let ret_ref = 
                tables.type_handler.lookup_or_define(syntax).unwrap();
                let value = self.locator.new_value(
                    ret_ref,
                    &mut self.slots,
                );
                let place = Place {
                    kind: PlaceKind::Slot(self.slots.new_temp(func.ret)),
                    offset: 0,
                    typeid: func.ret,
                };
                let op = Operation::AddressOf(value, place.clone());
                self.push(op);
                ids.push(value);
                for arg in args {
                    let value_id = self.parse_value(arg, tables)?;
                    ids.push(value_id);
                }
                let op = Operation::Call(None, id, ids);
                self.push(op);
                Ok(place)
            },
            TypedNode::StructConstructor(id, fields, pos) => {
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
                    Ok(destination_place)
                } else {
                    Err(TypeError::new_err(TypeErrorKind::ExpectedStruct, pos).to_any())
                }
            }
            TypedNode::ArrayConstructor(ty, nodes, pos) => {
                let array_type = tables.type_handler.get(&ty, Some(pos))?;
                if let TypeKind::Array(element_id, length) = array_type.kind {
                    let element_size = tables.type_handler.get(&element_id, Some(pos))?.size;
                    let slot = self.slots.new_temp(ty);
                    if nodes.iter().all(|n| n.equal_const(&nodes[0])) {
                        let place = Place {
                            kind: PlaceKind::Slot(slot),
                            offset: 0,
                            typeid: ty,
                        };
                        let op = Operation::HugeConst(place, nodes[0].unwrap_const());
                        if array_type.size >= 8 {
                            self.push(op)
                        }
                        for i in 0..(array_type.size % 8) {
                            let value = self.locator.new_value(element_id, &mut self.slots);
                            let place = Place {
                                kind: PlaceKind::Slot(slot),
                                offset: array_type.size - (array_type.size % 8) + i * element_size,
                                typeid: element_id,
                            };
                            let op = Operation::Const(value, nodes[0].unwrap_const());
                            self.push(op);
                            let op = Operation::Store(place, value);
                            self.push(op);
                        }
                    } else {
                        for (i, node) in nodes.iter().enumerate() {
                            let value = self.parse_value(node.clone(), tables)?;
                            let place = Place {
                                kind: PlaceKind::Slot(slot),
                                offset: i * element_size,
                                typeid: element_id,
                            };
                            let op = Operation::Store(place, value);
                            self.push(op);
                        }
                    }
                    Ok(Place {
                        kind: PlaceKind::Slot(slot),
                        offset: 0,
                        typeid: ty,
                    })
                } else {
                    unreachable!()
                }
            }
            TypedNode::Index(ty, base, index, _) => {
                if tables.type_handler.is_scalar(&ty.typeid)? {
                    let base = self.parse_place(*base, tables)?;
                    let index = self.parse_value(*index, tables)?;

                    let place = Place {
                        kind: PlaceKind::Index {
                            base: Box::new(base),
                            index,
                        },
                        offset: 0,
                        typeid: ty.typeid,
                    };
                    Ok(place)
                } else {
                    todo!()
                }
            }
            TypedNode::Litteral(ty, litt, _) => match litt {
                Litteral::String(string) => {
                    let slot_id = self.slots.new_temp(TypeID::STR);
                    let buffer_value = self
                        .locator
                        .new_value(tables.type_handler.u8_ref(), &mut self.slots);
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
                    let length_value = self.locator.new_value(TypeID::U64, &mut self.slots);
                    let op = Operation::Const(
                        length_value,
                        Litteral::Number(
                            string.len() - string.chars().filter(|c| *c == '\\').count(),
                        ),
                    );
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
                    Ok(place)
                }
                _ => {
                    let value = self.parse_value(node, tables)?;
                    let place = Place {
                        kind: PlaceKind::Slot(self.slots.new_temp(ty.typeid)),
                        offset: 0,
                        typeid: ty.typeid,
                    };
                    self.push(Operation::Store(place.clone(), value));
                    Ok(place)
                }
            },
            TypedNode::Binary {
                typeid,
                left,
                right,
                operator,
                position: _,
            } => {
                let left_slot = self.parse_value(*left, tables)?;
                let right_slot = self.parse_value(*right, tables)?;
                let value_id = self.locator.new_value(typeid.typeid, &mut self.slots);
                let op = operator.to_operation(typeid.typeid, value_id, left_slot, right_slot);
                self.push(op);
                self.locator.drop(left_slot);
                self.locator.drop(right_slot);
                let slot_id = self.slots.new_temp(typeid.typeid);
                let place = Place {
                    kind: PlaceKind::Slot(slot_id),
                    offset: 0,
                    typeid: typeid.typeid,
                };
                let op = Operation::Store(place.clone(), value_id);
                self.push(op);
                Ok(place)
            }
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
                if let TypeKind::Structure(struct_id) = tables
                    .type_handler
                    .get(&base_place.typeid, Some(pos))
                    .unwrap()
                    .kind
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
                    println!("{:?}", &ty.typeid);
                    Err(TypeError::new_err(TypeErrorKind::ExpectedStruct, pos).to_any())
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
            TypedNode::Address(ty, mutable, node, _) => {
                todo!()
            }
            TypedNode::Parenthesis(_, node) => self.parse_place(*node, tables),
            _ => Err(IRError::new_err(
                IRErrorKind::ExpectedPlace(node.clone()),
                node.position(),
            ).to_any()),
        }
    }
    pub fn parse_value(&mut self, node: TypedNode, tables: &mut Tables) -> AnyResult<ValueID> {
        match self.parse_node(node.clone(), tables)? {
            OpOutput::Scalar(value) => Ok(value),
            OpOutput::Memory(place) => {
                if !tables.type_handler.is_scalar(&place.typeid)? {
                    return Err(IRError::new_err(
                        IRErrorKind::ExpectedScalar(node.clone()),
                        node.position(),
                    ).to_any());
                }

                let destination = self.locator.new_value(place.typeid, &mut self.slots);

                self.push(Operation::Load(destination, place));

                Ok(destination)
            }
            OpOutput::Unit => Ok(self.locator.new_value(TypeID::VOID, &mut self.slots)),
            _ => todo!(),
        }
    }
    pub fn resolve_place(&mut self, assignee: Assignee, tables: &mut Tables) -> AnyResult<Place> {
        println!("{:?}", assignee);
        match &assignee {
            Assignee::Symbol(sym) => {
                let slot_id = self.slots.lookup(&sym).unwrap().clone();
                println!(
                    "{:?} -> {:?}",
                    tables.symbol_table.get(&sym).identifier,
                    slot_id
                );
                let slot = self.slots.get(slot_id);
                let place = Place {
                    kind: PlaceKind::Slot(slot_id),
                    offset: 0,
                    typeid: slot.ty,
                };
                Ok(place)
            }
            Assignee::Index { base, index } => {
                let base_place = self.resolve_place(*base.clone(), tables)?;
                let index_value = self.parse_value(*index.clone(), tables)?;
                let ty = tables.type_handler.get(&base_place.typeid, None)?;
                if let TypeKind::Array(typeid, _) = ty.kind {
                    let place = Place {
                        kind: PlaceKind::Index {
                            base: Box::new(base_place),
                            index: index_value,
                        },
                        offset: 0,
                        typeid: typeid,
                    };
                    Ok(place)
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::ExpectedArray(base_place.typeid),
                        position: None,
                    }.to_any())
                }
            }
            Assignee::Field { base, field } => {
                let base_place = self.resolve_place(*base.clone(), tables)?;
                let ty = tables.type_handler.get(&base_place.typeid, None)?;
                if let TypeKind::Structure(struct_id) = ty.kind {
                    let struc = tables.struct_table.get(struct_id).unwrap();
                    let field_offset = struc.get_offset(field.clone(), &tables.type_handler);
                    let field_type = struc.get_type(field.clone());
                    Ok(base_place.offset(field_offset, field_type))
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::ExpectedStruct,
                        position: None,
                    }.to_any())
                }
            }
            _ => todo!(),
        }
    }

    pub fn parse_node(&mut self, node: TypedNode, tables: &mut Tables) -> AnyResult<OpOutput> {
        match node.clone() {
            TypedNode::ArrayConstructor(_, _, _) => todo!(),
            TypedNode::StructConstructor(id, fields, pos) => {
                panic!("node");
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
                    Err(TypeError::new_err(TypeErrorKind::ExpectedStruct, pos).to_any())
                }
            }
            TypedNode::Litteral(ty, litt, _) => match litt {
                Litteral::String(string) => {
                    let slot_id = self.slots.new_temp(TypeID::STR);
                    let buffer_value = self
                        .locator
                        .new_value(tables.type_handler.u8_ref(), &mut self.slots);
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
                    let length_value = self.locator.new_value(TypeID::U64, &mut self.slots);
                    let op = Operation::Const(
                        length_value,
                        Litteral::Number(
                            string.len() - string.chars().filter(|c| *c == '\\').count(),
                        ),
                    );
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
                    let value = self.locator.new_value(ty.typeid, &mut self.slots);
                    let op = Operation::Const(value, litt);
                    self.push(op);
                    Ok(OpOutput::Scalar(value))
                }
            },
            TypedNode::Block(ty, block) => self.parse_block(block, tables),
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
                self.set_terminator(Terminator::Jump {
                    destination: loop_label.clone(),
                })?;
                let return_slot = self.slots.new_temp(ty.typeid);
                self.loops.push(LoopMerge {
                    ty: ty.typeid,
                    exit: merge_label.clone(),
                    return_slot: Some(return_slot),
                });
                self.blocks.push(BasicBlock::new(loop_label.clone()));
                let _ = self.parse_block(block.clone(), tables)?;
                self.loops.pop();
                if let Terminator::Unknown = self.get_terminator() {
                    self.set_terminator(Terminator::Jump {
                        destination: loop_label,
                    })?;
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
                    self.set_terminator(Terminator::Branch {
                        predicate,
                        destination: then_label.clone(),
                        inverse: else_label.clone(),
                    })?;
                    self.locator.drop(predicate);
                    self.blocks.push(BasicBlock::new(then_label.clone()));
                    let then_res = self.parse_block(body.clone(), tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump {
                            destination: merge_label.clone(),
                        })?;
                    }

                    self.blocks.push(BasicBlock::new(else_label.clone()));
                    let else_res = self.parse_block(else_body.clone(), tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump {
                            destination: merge_label.clone(),
                        })?;
                    }
                    self.blocks.push(BasicBlock::new(merge_label));
                    match (then_res, else_res) {
                        (OpOutput::Scalar(then_value), OpOutput::Scalar(else_value)) => {
                            let then_type = self.locator.get(&then_value).typeid;
                            let else_type = self.locator.get(&else_value).typeid;
                            if then_type != else_type {
                                return Err(TypeError::new_err(
                                    TypeErrorKind::TypeMismatch(then_type, else_type),
                                    pos,
                                ).to_any());
                            }
                            let out_value = self.locator.new_value(then_type, &mut self.slots);
                            let op = Operation::Move(out_value, then_value);
                            self.locator.drop(then_value);
                            self.blocks[then_label.2].operations.push(op);

                            let op = Operation::Move(out_value, else_value);
                            self.locator.drop(else_value);
                            self.blocks[else_label.2].operations.push(op);

                            Ok(OpOutput::Scalar(out_value))
                        }
                        (OpOutput::Memory(then_place), OpOutput::Memory(else_place)) => {
                            if &then_place.typeid != &else_place.typeid
                                || &then_place.offset != &else_place.offset
                            {
                                return Err(TypeError::new_err(
                                    TypeErrorKind::TypeMismatch(
                                        then_place.typeid,
                                        else_place.typeid,
                                    ),
                                    pos,
                                ).to_any());
                            }

                            let out_place = Place {
                                kind: PlaceKind::Slot(self.slots.new_temp(then_place.typeid)),
                                offset: then_place.offset,
                                typeid: then_place.typeid,
                            };
                            let op = Operation::Copy(
                                out_place.clone(),
                                then_place.clone(),
                                then_place.typeid,
                            );
                            self.blocks[then_label.2].operations.push(op);

                            let op = Operation::Copy(
                                out_place.clone(),
                                else_place.clone(),
                                else_place.typeid,
                            );
                            self.blocks[else_label.2].operations.push(op);

                            Ok(OpOutput::Memory(out_place))
                        }
                        (OpOutput::Unit, OpOutput::Unit) => Ok(OpOutput::Unit),
                        _ => todo!(),
                    }
                } else {
                    self.set_terminator(Terminator::Branch {
                        predicate,
                        destination: then_label.clone(),
                        inverse: merge_label.clone(),
                    })?;
                    self.locator.drop(predicate);
                    self.blocks.push(BasicBlock::new(then_label));
                    if let Some(tail) = body.tail.clone() {
                        if !tail.infered_type().strong || tail.infered_type().typeid == TypeID::VOID
                        {
                            return Err(IRError::new_err(
                                IRErrorKind::ExpectedElse(tail.infered_type().typeid),
                                pos,
                            ).to_any());
                        }
                    }
                    let then_res = self.parse_block(body, tables)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump {
                            destination: merge_label.clone(),
                        })?;
                    }
                    self.blocks.push(BasicBlock::new(merge_label));
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
                let slot_id = self.locator.new_value(typeid.typeid, &mut self.slots);
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
                        let destination: ValueID =
                            self.locator.new_value(ty.typeid, &mut self.slots);

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
                    let destination: ValueID = self.locator.new_value(ty.typeid, &mut self.slots);

                    self.push(Operation::Load(destination, source));
                    Ok(OpOutput::Scalar(destination))
                } else {
                    let place = self.parse_place(node, tables)?;
                    Ok(OpOutput::Memory(place))
                }
            }
            TypedNode::Index(ty, base, index, _) => {
                if tables.type_handler.is_scalar(&ty.typeid)? {
                    let base = self.parse_place(*base, tables)?;
                    let array_type_syntax = tables
                        .type_handler
                        .get(&base.typeid, None)?
                        .syntax(&tables.type_handler)?;
                    let ref_array_syntax = TypeSyntax::Reference {
                        mutable: false,
                        pointee: Box::new(array_type_syntax),
                    };
                    let ref_array_type = tables
                        .type_handler
                        .lookup_or_define(ref_array_syntax)
                        .unwrap();
                    let base_ptr = self.locator.new_value(ref_array_type, &mut self.slots);
                    let op = Operation::AddressOf(base_ptr, base);
                    self.push(op);
                    let index_value = self.parse_value(*index, tables)?;
                    let destination = self.locator.new_value(ty.typeid, &mut self.slots);
                    let op = Operation::PointerOffset(destination, base_ptr, index_value);
                    self.push(op);
                    Ok(OpOutput::Scalar(destination))
                } else {
                    todo!()
                }
            }
            TypedNode::Deref(ty, _, pos) => {
                if tables.type_handler.is_scalar(&ty.typeid)? {
                    let source: Place = self.parse_place(node, tables)?;
                    let destination: ValueID = self.locator.new_value(ty.typeid, &mut self.slots);

                    self.push(Operation::Load(destination, source));
                    Ok(OpOutput::Scalar(destination))
                } else {
                    let place = self.parse_place(node, tables)?;
                    Ok(OpOutput::Memory(place))
                }
            }
            TypedNode::Address(ty, _, child, _) => {
                let source: Place = self.parse_place(*child, tables)?;
                let _address_type = tables.type_handler.get(&ty.typeid, None);
                let destination: ValueID = self.locator.new_value(ty.typeid, &mut self.slots);
                self.push(Operation::AddressOf(destination, source));
                Ok(OpOutput::Scalar(destination))
            }
            TypedNode::Unary(ty, operator, child, _) => {
                let child_slot = self.parse_value(*child, tables)?;
                let slot_id = self.locator.new_value(ty.typeid, &mut self.slots);
                let op = operator.to_operation(ty.typeid, slot_id, child_slot);
                self.push(op);
                Ok(OpOutput::Scalar(slot_id))
            }
            TypedNode::Cast(ty, node, pos) => {
                let value = self.parse_value(*node, tables)?;
                self.locator.drop(value);
                let new_value = self.locator.new_value(ty, &mut self.slots);
                let op = Operation::Cast(new_value, value);
                self.push(op);
                Ok(OpOutput::Scalar(new_value))
            }
            TypedNode::Assignment(ty, assignee, child, _) => {
                let value = self.parse_value(*child, tables)?;
                let place = self.resolve_place(assignee, tables)?;
                let op = Operation::Store(place.clone(), value);
                self.push(op.clone());
                self.locator.drop(value);
                Ok(OpOutput::Memory(place))
            }
            TypedNode::Parenthesis(ty, child) => self.parse_node(*child, tables),
            TypedNode::FuncIdentifier(ty, id, args, pos) => {
                let func = tables.func_table.get(&id).clone();
                let mut ids = Vec::new();
                if func.big_ret {
                    let ret_ty = tables.type_handler.get(&func.ret, None)?;
                    let syntax = TypeSyntax::Reference {
                        mutable: true,
                        pointee: Box::new(TypeSyntax::Raw(ret_ty.identifier.clone())),
                    };
                    let ret_ref = 
                        tables.type_handler.lookup_or_define(syntax).unwrap();
                    let value = self.locator.new_value(
                        ret_ref,
                        &mut self.slots,
                    );
                    let place = Place {
                        kind: PlaceKind::Slot(self.slots.new_temp(func.ret)),
                        offset: 0,
                        typeid: func.ret,
                    };
                    let op = Operation::AddressOf(value, place);
                    self.push(op);
                    ids.push(value);
                }
                for arg in args {
                    let value_id = self.parse_value(arg, tables)?;
                    ids.push(value_id);
                }
                let return_type = tables.type_handler.get(&func.ret, Some(pos))?;
                if return_type.size > 0 {
                    let value = self.locator.new_value(ty, &mut self.slots);
                    let op = Operation::Call(Some(value), id, ids);
                    self.push(op);
                    Ok(OpOutput::Scalar(value))
                } else {
                    let op = Operation::Call(None, id, ids);
                    self.push(op);
                    if ty == TypeID::NEVER {
                        self.set_terminator(Terminator::Unreachable)?;
                    }
                    Ok(OpOutput::Unit)
                }
            }
        }
    }

    pub fn parse_block_into(
        &mut self,
        block: TypedBlock,
        tables: &mut Tables,
        value: ValueID,
    ) -> AnyResult<()> {
        for stmt in block.stmts {
            if let Terminator::Unknown = self.get_terminator() {
            } else {
                let _: () = print_err(
                    IRError::new_err(IRErrorKind::Unreachable, stmt.position()),
                    &tables.file_contents,
                    &tables.type_handler,
                );
            }
            self.parse_stmt(stmt, tables)?;
        }
        if let Some(tail) = block.tail {
            self.parse_into(*tail, value, tables)?;
        }
        Ok(())
    }

    fn parse_into(
        &mut self,
        node: TypedNode,
        value: ValueID,
        tables: &mut Tables,
    ) -> AnyResult<()> {
        match node {
            TypedNode::FuncIdentifier(ty, id, args, pos) => {
                let mut ids = Vec::new();
                for arg in args {
                    let value_id = self.parse_value(arg, tables)?;
                    ids.push(value_id);
                }
                let func = tables.func_table.get(&id);
                let return_type = tables.type_handler.get(&func.ret, Some(pos))?;
                if return_type.size > 0 {
                    let op = Operation::Call(Some(value), id, ids);
                    self.push(op);
                } else {
                    let op = Operation::Call(None, id, ids);
                    self.push(op);
                    if ty == TypeID::NEVER {
                        self.set_terminator(Terminator::Unreachable)?;
                    }
                }
            }
            TypedNode::Litteral(ty, litt, pos) => {
                if let Litteral::String(_) = litt {
                    panic!("expected number");
                }
                let op = Operation::Const(value, litt);
                self.push(op);
            }
            TypedNode::If(ty, predicate, body, el, pos) => {
                println!("if into");
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
                    self.set_terminator(Terminator::Branch {
                        predicate,
                        destination: then_label.clone(),
                        inverse: else_label.clone(),
                    })?;
                    self.locator.drop(predicate);
                    self.blocks.push(BasicBlock::new(then_label.clone()));
                    let then_res = self.parse_block_into(body.clone(), tables, value)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump {
                            destination: merge_label.clone(),
                        })?;
                    }

                    self.blocks.push(BasicBlock::new(else_label.clone()));
                    let else_res = self.parse_block_into(else_body.clone(), tables, value)?;
                    if let Terminator::Unknown = self.get_terminator() {
                        self.set_terminator(Terminator::Jump {
                            destination: merge_label.clone(),
                        })?;
                    }
                    self.blocks.push(BasicBlock::new(merge_label));
                }
            }
            n => {
                let src = self.parse_value(n, tables)?;
                self.push(Operation::Move(value, src));
                self.locator.drop(src);
            }
        }

        Ok(())
    }

    fn push(&mut self, operation: Operation) {
        let block: &mut BasicBlock = self.blocks.last_mut().unwrap();
        block.operations.push(operation);
    }

    pub fn set_terminator(&mut self, terminator: Terminator) -> TypeResult<()> {
        let block: &mut BasicBlock = self.blocks.last_mut().unwrap();
        match block.terminator {
            Terminator::Unknown => block.terminator = terminator,
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
            let value = self.locator.new_value(type_id.clone(), &mut self.slots);
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
    Jump {
        destination: BlockLabel,
    },
    Branch {
        predicate: ValueID,
        destination: BlockLabel,
        inverse: BlockLabel,
    },
    ReturnValue {
        value: Option<ValueID>,
    },
}

pub struct BasicBlock {
    pub label: BlockLabel,
    pub operations: Vec<Operation>,
    pub terminator: Terminator,
}

impl BasicBlock {
    pub fn nop_count(&self) -> usize {
        self.operations
            .iter()
            .filter(|op| if let Operation::NOP = op { true } else { false })
            .count()
    }
    pub fn remove_nop(&mut self) {
        self.operations = self
            .operations
            .iter()
            .filter(|op| if let Operation::NOP = op { false } else { true })
            .map(|op| op.clone())
            .collect();
    }
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

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct BlockLabel(String, String, usize);

impl BlockLabel {
    pub fn entry(&self) -> bool {
        self.2 == 0
    }

    pub fn ident(&self) -> &String {
        &self.0
    }
}

fn values_to_string(mut v: Vec<ValueID>) -> String {
    let mut out = String::new();
    let last = v.pop().unwrap();
    for id in v {
        out.push_str(format!("{}, ", id).as_str());
    }
    out.push_str(format!("{}", last).as_str());
    out
}

impl Debug for BlockLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}.{}", self.0, self.1, self.2)
    }
}
