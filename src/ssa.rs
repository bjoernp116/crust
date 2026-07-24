use std::{any::Any, collections::HashMap, fmt::Debug, sync::Arc};

use crate::{
    functions::FuncID, lexer::Position, parser::Litteral, symbols::{SymbolID, SymbolTable}, types::{Tables, TypeHandler, TypeID, TypedNode, TypedStatement}
};

#[derive(Debug)]
pub struct SSA {
    pub functions: Vec<Function>,
}

impl SSA {
    pub fn new(stmts: Vec<TypedStatement>, tables: &mut Tables) -> SSA {
        let mut functions = Vec::new();
        for stmt in stmts {
            match stmt {
                TypedStatement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => functions.push(Function::new(identifier, args, ret, *body, tables)),
                _ => (),
            }
        }
        Self { functions }
    }
}

#[derive(Debug)]
pub struct Function {
    pub identifier: String,
    pub blocks: Vec<Block>,
    pub slots: SlotTable,
    pub args: Vec<(SlotID, TypeID)>,
}

impl Function {
    pub fn new(
        identifier: String,
        args: Vec<(SymbolID, TypeID)>,
        ret: TypeID,
        body: TypedStatement,
        tables: &mut Tables,
    ) -> Function {
        let mut blocks = Vec::new();
        let mut slot_table = SlotTable::new();
        let mut params = Vec::new();
        blocks.push(Block::new(BlockLabel(identifier.clone(), String::from("entry"), 0)));
        for (sy, ty) in args {
            params.push((slot_table.new_var(ty, sy), ty)); 
        }
        let mut func = Function {
            blocks,
            slots: slot_table,
            identifier,
            args: params
        };
        func.parse_stmt(body, tables);
        //println!("{:#?}", func.slots.stack_frame(&tables.type_handler));
        func
    }

    pub fn parse_stmt(&mut self, stmt: TypedStatement, tables: &mut Tables) {
        match stmt {
            TypedStatement::VarDecl(ident, symbol_id, node) => {
                let node_slot_id = self.parse_node(node);
                let symbol = tables.symbol_table.get(symbol_id);
                let slot_id = self.slots.new_var(symbol.typeid.typeid, symbol_id);
                let op = Operation::Copy(slot_id, node_slot_id);
                self.push(op)
            },
            TypedStatement::If(_, predicate, body, el) => {
                let predicate_slot_id = self.parse_node(predicate);
                if let Some(else_body) = el {
                    let then_label = BlockLabel(self.identifier.clone(), format!("if_then"), self.blocks.len());
                    let else_label = BlockLabel(self.identifier.clone(), format!("if_else"), self.blocks.len());
                    let merge_label = BlockLabel(self.identifier.clone(), format!("if_merge"), self.blocks.len());
                    let op = Operation::Branch(predicate_slot_id, then_label.clone(), else_label.clone());
                    self.push(op);

                    self.blocks.push(Block::new(then_label));
                    self.parse_stmt(*body, tables);
                    let then_term = self.terminated();
                    if !then_term {
                        let op = Operation::Jump(merge_label.clone());
                        self.push(op);
                    }
                    
                    self.blocks.push(Block::new(else_label));
                    self.parse_stmt(*else_body, tables);
                    let else_term = self.terminated();
                    if !else_term {
                        let op = Operation::Jump(merge_label.clone());
                        self.push(op);
                    }
                    
                    if !(then_term && else_term) {
                        self.blocks.push(Block::new(merge_label));
                    }
                } else {
                    let then_label = BlockLabel(self.identifier.clone(), format!("if_then"), self.blocks.len());
                    let merge_label = BlockLabel(self.identifier.clone(), format!("if_merge"), self.blocks.len());
                    let op = Operation::Branch(predicate_slot_id, then_label.clone(), merge_label.clone());
                    self.push(op);

                    self.blocks.push(Block::new(then_label));
                    self.parse_stmt(*body, tables);
                    let then_term = self.terminated();
                    if !then_term {
                        let op = Operation::Jump(merge_label.clone());
                        self.push(op);
                        self.blocks.push(Block::new(merge_label));
                    }
                }
            }
            TypedStatement::Block(t, stmts, tail) => {
                for stmt in stmts {
                    self.parse_stmt(stmt, tables);
                }
                if let Some(tail_stmt) = tail {
                    self.parse_tail_stmt(*tail_stmt, tables, t.typeid);
                }
            }
            TypedStatement::Return(_, opt) => {
                if let Some(child) = opt {
                    let child_slot_id = self.parse_node(child);
                    let op = Operation::Return(Some(child_slot_id));
                    self.push(op);
                } else {
                    let op = Operation::Return(None);
                    self.push(op);
                }
            }
            TypedStatement::Expression(ty, node) => {
                self.parse_node(node);
            }
            _ => (),
        }
    }

    pub fn parse_node(&mut self, node: TypedNode) -> SlotID {
        match node {
            TypedNode::Litteral(ty, litt, _) => {
                let slot_id = self.slots.new_temp(ty.typeid);
                let op = Operation::Const(slot_id, litt);
                self.push(op);
                slot_id
            },
            TypedNode::Binary { typeid, left, right, operator, position: _ } => {
                let left_slot = self.parse_node(*left);
                let right_slot = self.parse_node(*right);
                let slot_id = self.slots.new_temp(typeid.typeid);
                let op = operator.to_operation(typeid.typeid, slot_id, left_slot, right_slot);
                self.push(op);
                slot_id
            }
            TypedNode::Identifier(ty, sym, _) => {
                self.slots.lookup(&sym).unwrap().clone()
            }
            TypedNode::Unary(ty, operator, child, _) => {
                let child_slot = self.parse_node(*child);
                let slot_id = self.slots.new_temp(ty.typeid);
                let op = operator.to_operation(ty.typeid, slot_id, child_slot);
                self.push(op);
                slot_id
            }
            TypedNode::Assignment(ty, sym, child, _) => {
                let child_slot = self.parse_node(*child);
                let slot_id = self.slots.lookup(&sym).unwrap().clone();
                let op = Operation::Copy(slot_id, child_slot);
                self.push(op);
                slot_id
            }
            TypedNode::Parenthesis(ty, child) => {
                self.parse_node(*child)
            }
            TypedNode::FuncIdentifier(ty, id, args, _) => {
                let mut ids = Vec::new();
                for arg in args {
                    ids.push(self.parse_node(arg));
                }
                let slot_id = self.slots.new_temp(ty);
                let op = Operation::Call(slot_id, id, ids);
                self.push(op);
                slot_id
            }
        }
    }

    fn parse_tail_stmt(&mut self, stmt: TypedStatement, tables: &mut Tables) {
        match stmt {
            
        }
    }

    fn push(&mut self, operation: Operation) {
        let block: &mut Block = self.blocks.last_mut().unwrap();
        block.operations.push(operation);
    }

    fn peek(&self) -> Operation {
        let block = self.blocks.last().unwrap();
        block.operations.last().unwrap().clone()
    }

    fn terminated(&self) -> bool {
        match self.peek() {
            Operation::Return(_) => true,
            _ => false
         }
    }
}

#[derive(Debug)]
pub struct Block {
    pub label: BlockLabel,
    pub operations: Vec<Operation>,
}

impl Block {
    pub fn new(label: BlockLabel) -> Self {
        Self {
            label,
            operations: Vec::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Operation {
    Copy(SlotID, SlotID),
    Const(SlotID, Litteral),
    Phi {
        dest: SlotID,
        label1: String,
        slot1: SlotID,
        label2: String,
        slot2: SlotID,
    },
    Eq(TypeID, SlotID, SlotID, SlotID),
    G(TypeID, SlotID, SlotID, SlotID),
    L(TypeID, SlotID, SlotID, SlotID),
    GEq(TypeID, SlotID, SlotID, SlotID),
    LEq(TypeID, SlotID, SlotID, SlotID),
    NEq(TypeID, SlotID, SlotID, SlotID),
    Add(TypeID, SlotID, SlotID, SlotID),
    Sub(TypeID, SlotID, SlotID, SlotID),
    Div(TypeID, SlotID, SlotID, SlotID),
    Mul(TypeID, SlotID, SlotID, SlotID),
    Not(TypeID, SlotID, SlotID),
    Neg(TypeID, SlotID, SlotID),
    Jump(BlockLabel),
    Branch(SlotID, BlockLabel, BlockLabel),
    Return(Option<SlotID>),
    Call(SlotID, FuncID, Vec<SlotID>),
}

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct SlotID(pub usize);

#[derive(Clone)]
pub struct StackLocation {
    pub offset: usize,
    pub size: usize
}

impl Debug for StackLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let size = match self.size {
            1 => "byte",
            2 => "word",
            4 => "dword",
            8 => "qword",
            _ => todo!()
        };
        write!(f, "{} ptr [rbp - {}]", size, self.offset)
    }
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

    pub fn new_temp(&mut self, type_id: TypeID) -> SlotID {
        self.slots.push(Slot {
            ty: type_id,
            kind: SlotKind::Temporary(ValueID(self.value_counter)),
        });
        self.value_counter += 1;
        SlotID(self.slots.len() - 1)
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
            let ty = type_handler.get(&slot.ty, None).unwrap();
            
            let alignment = ty.size;
            
            used = align_up(used, alignment);

            used += ty.size;
            let location = StackLocation {
                offset: used,
                size: ty.size
            };
            map.insert(SlotID(i), location);
        }
        used += 16 - (used % 16);
        StackFrame { stack_map: map, size: used }
    }
}

fn align_up(value: usize, alignment: usize) -> usize {
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
    Argument(SymbolID),
    Temporary(ValueID),
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
