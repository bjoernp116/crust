/*
use crate::{
    parser::{BinaryOperator, Litteral, Node, Statement, UnaryOperator},
    types::{REGISTER_STACK, Register, Type, TypeHandler, TypeValue, TypedStatement},
};
use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
};

#[derive(Debug, Clone)]
pub enum Operation {
    Copy(Slot, Slot),
    Const(Slot, Litteral),
    Phi {
        dest: Slot,
        label1: String,
        slot1: Slot,
        label2: String,
        slot2: Slot,
    },
    Eq(Type, Slot, Operand, Operand),
    Add(Type, Slot, Operand, Operand),
    Sub(Type, Slot, Operand, Operand),
    Div(Type, Slot, Operand, Operand),
    Mul(Type, Slot, Operand, Operand),
    Not(Type, Slot, Operand),
    Neg(Type, Slot, Operand),
    Jump(String),
    Branch(Slot, String, String),
    Return(Type, Option<Operand>),
    Call(Slot, String, Vec<Slot>),
}

impl Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operation::Eq(t, slot, op1, op2) => write!(f, "{} = eq<{}> {}, {}", slot, t, op1, op2),
            Operation::Add(t, slot, op1, op2) => {
                write!(f, "{} = add<{}> {}, {}", slot, t, op1, op2)
            }
            Operation::Sub(t, slot, op1, op2) => {
                write!(f, "{} = sub<{}> {}, {}", slot, t, op1, op2)
            }
            Operation::Mul(t, slot, op1, op2) => {
                write!(f, "{} = mul<{}> {}, {}", slot, t, op1, op2)
            }
            Operation::Div(t, slot, op1, op2) => {
                write!(f, "{} = div<{}> {}, {}", slot, t, op1, op2)
            }
            Operation::Copy(slot, op1) => write!(f, "{} = copy {}", slot, op1),
            Operation::Const(slot, op1) => write!(f, "{} = const {}", slot, op1),
            Operation::Jump(label) => write!(f, "jump {}", label),
            Operation::Return(t, Some(op1)) => write!(f, "return<{}> {}", t, op1),
            Operation::Return(_, None) => write!(f, "return<void> null"),
            Operation::Branch(slot, then, el) => write!(f, "br {}, {}, {}", slot, then, el),
            Operation::Phi {
                dest,
                label1,
                slot1,
                label2,
                slot2,
            } => {
                write!(
                    f,
                    "{} = phi({}: {}, {}: {})",
                    dest, label1, slot1, label2, slot2
                )
            }
            Operation::Call(slot, identifier, args) => {
                let mut args = args.clone();
                write!(f, "{} = call {}(", slot, identifier)?;
                if let Some(last) = args.pop() {
                    for arg in args {
                        write!(f, "{}, ", arg)?;
                    }
                    write!(f, "{})", last)
                } else {
                    write!(f, ")")
                }
            }
            Operation::Not(t, slot, child) => write!(f, "{} = not<{}> {}", slot, t, child),
            Operation::Neg(t, slot, child) => write!(f, "{} = neg<{}> {}", slot, t, child),
        }
    }
}

impl Operation {
    fn operands(&self) -> Vec<Operand> {
        match self {
            Operation::Add(_, _, op1, op2) => vec![op1.clone(), op2.clone()],
            Operation::Sub(_, _, op1, op2) => vec![op1.clone(), op2.clone()],
            Operation::Mul(_, _, op1, op2) => vec![op1.clone(), op2.clone()],
            Operation::Div(_, _, op1, op2) => vec![op1.clone(), op2.clone()],
            Operation::Eq(_, _, op1, op2) => vec![op1.clone(), op2.clone()],
            Operation::Neg(_, _, op1) => vec![op1.clone()],
            Operation::Not(_, _, op1) => vec![op1.clone()],
            Operation::Copy(_, slot) => vec![Operand::Slot(slot.clone())],
            _ => Vec::new(),
        }
    }

    fn set_rhs(&self, operand: Operand) -> Operation {
        match self.clone() {
            Operation::Add(t, s, other, _) => Operation::Add(t, s, other, operand),
            Operation::Sub(t, s, other, _) => Operation::Sub(t, s, other, operand),
            Operation::Mul(t, s, other, _) => Operation::Mul(t, s, other, operand),
            Operation::Div(t, s, other, _) => Operation::Div(t, s, other, operand),
            Operation::Eq(t, s, other, _) =>  Operation::Eq(t, s, other, operand),
            _ => self.clone()
        }
    }

    fn set_lhs(&self, operand: Operand) -> Operation {
        match self.clone() {
            Operation::Add(t, s, _, other) => Operation::Add(t, s, other, operand),
            Operation::Sub(t, s, _, other) => Operation::Sub(t, s, other, operand),
            Operation::Mul(t, s, _, other) => Operation::Mul(t, s, other, operand),
            Operation::Div(t, s, _, other) => Operation::Div(t, s, other, operand),
            Operation::Eq(t, s, _, other) =>  Operation::Eq(t, s, other, operand),
            _ => self.clone()
        }
    }

    fn destination(&self) -> Option<Slot> {
        match self {
            Operation::Add(_, slot, _, _) => Some(slot.clone()),
            Operation::Sub(_, slot, _, _) => Some(slot.clone()),
            Operation::Mul(_, slot, _, _) => Some(slot.clone()),
            Operation::Div(_, slot, _, _) => Some(slot.clone()),
            Operation::Eq(_, slot, _, _) => Some(slot.clone()),
            Operation::Neg(_, slot, _) => Some(slot.clone()),
            Operation::Not(_, slot, _) => Some(slot.clone()),
            Operation::Copy(slot, _) => Some(slot.clone()),
            Operation::Const(slot, _) => Some(slot.clone()),
            Operation::Phi{dest: slot, ..} => Some(slot.clone()),
            _ => None
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub label: String,
    pub operations: Vec<Operation>,
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}:", self.label)?;
        for op in &self.operations {
            writeln!(f, "\t{}", op)?;
        }
        Ok(())
    }
}

impl Block {
    pub fn new(label: impl ToString) -> Self {
        Self {
            label: label.to_string(),
            operations: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Func {
    pub identifier: String,
    args: Vec<(String, String)>,
    ret: Option<String>,
    pub slots: Slots,
    pub blocks: Vec<Block>,
    branches: usize,
    joins: usize,
}

impl Func {
    pub fn new(
        identifier: String,
        args: Vec<(String, String)>,
        ret: Option<String>,
        type_handler: &mut TypeHandler,
    ) -> Func {
        let entry = Block::new("entry");
        let mut slots = Slots::new();
        for (ident, t) in args.clone() {
            slots.new_variable(ident.clone());
            //slots.set_type(slot, type_handler.get(t).unwrap().clone());
        }
        Func {
            identifier,
            blocks: vec![entry],
            slots,
            args,
            ret,
            branches: 0,
            joins: 0,
        }
    }

    pub fn parse_stmt(
        &mut self,
        stmt: TypedStatement,
        type_handler: &mut TypeHandler,
        globals: &HashMap<String, Declaration>,
    ) {
        match stmt {
            TypedStatement::Block(stmts) => {
                for s in stmts {
                    self.parse_stmt(s, type_handler, globals);
                }
            }
            TypedStatement::Expression(expr) => {
                self.parse_node(expr, type_handler, globals);
            }
            TypedStatement::VarDecl(identifier, type_option, node) => {
                if let Some(t) = type_option {
                    //self.slots
                    //    .set_type(identifier.clone(), type_handler.get(t).unwrap().clone());
                }
                let slot = self.slots.new_variable(identifier);
                let op1 = self.parse_node(node, type_handler, globals);
                
                self.push(Operation::Copy(slot, op1))
            }
            TypedStatement::Return(node_option) => {
                if let Some(node) = node_option {
                    let op1 = self.parse_node(node, type_handler, globals);
                    let operation = Operation::Return(
                        type_handler
                            .get(self.ret.clone().unwrap_or(String::from("null")))
                            .unwrap()
                            .clone(),
                        Some(Operand::Slot(op1)),
                    );
                    self.push(operation);
                } else {
                    self.push(Operation::Return(
                        type_handler.get(String::from("null")).unwrap().clone(),
                        None,
                    ));
                }
            }
            TypedStatement::If(condition, then_body, else_body_option) => {
                let res = self.parse_node(condition, type_handler, globals);
                if let Some(else_body) = else_body_option {
                    let entry_label = self.blocks.last().unwrap().label.clone();
                    let then_label = self.new_branch_label();
                    let else_label = self.new_branch_label();
                    let join_label = self.new_join_label();
                    self.push(Operation::Branch(
                        res,
                        then_label.clone(),
                        else_label.clone(),
                    ));
                    let entry_slots = self.slots.get_snapshot();

                    self.blocks.push(Block::new(then_label.clone()));
                    self.parse_stmt(*then_body, type_handler, globals);
                    self.push(Operation::Jump(join_label.clone()));

                    let then_slots = self.slots.get_snapshot();

                    self.blocks.push(Block::new(else_label.clone()));
                    self.parse_stmt(*else_body, type_handler, globals);
                    self.push(Operation::Jump(join_label.clone()));

                    let else_slots = self.slots.get_snapshot();

                    self.blocks.push(Block::new(join_label));
                    self.generate_phis(
                        entry_label,
                        entry_slots,
                        then_label.clone(),
                        then_slots.clone(),
                    );
                    self.generate_phis(
                        then_label.clone(),
                        then_slots.clone(),
                        else_label,
                        else_slots,
                    );
                } else {
                    let entry_label = self.blocks.last().unwrap().label.clone();
                    let then_label = self.new_branch_label();
                    let join_label = self.new_join_label();
                    self.push(Operation::Branch(
                        res,
                        then_label.clone(),
                        join_label.clone(),
                    ));

                    let entry_slots = self.slots.get_snapshot();

                    self.blocks.push(Block::new(then_label.clone()));
                    self.parse_stmt(*then_body, type_handler, globals);

                    let then_slots = self.slots.get_snapshot();

                    self.push(Operation::Jump(join_label.clone()));
                    let then_label = self.blocks.last().unwrap().label.clone();
                    self.blocks.push(Block::new(join_label));
                    self.generate_phis(entry_label, entry_slots, then_label, then_slots);
                }
            }
            todo => {
                println!("- {:?}", todo);
                todo!()
            }
        }
    }

    pub fn parse_node(
        &mut self,
        node: Node,
        type_handler: &mut TypeHandler,
        globals: &HashMap<String, Declaration>,
    ) -> Slot {
        match node {
            Node::Litteral(lit, _) => {
                let slot = self.slots.new_constant(None, lit.clone());
                self.push(Operation::Const(slot.clone(), lit));
                slot
            }
            Node::Binary {
                left,
                right,
                operator,
                position: _,
            } => {
                let op1 = self.parse_node(*left.clone(), type_handler, globals);
                let op1_type = infer_type(*left, type_handler, globals, &self.slots);
                let op2 = self.parse_node(*right.clone(), type_handler, globals);
                let op2_type = infer_type(*right, type_handler, globals, &self.slots);
                let dest_type = (op1_type | op2_type).t;

                let dest = self.slots.new_temp();
                self.slots.set_type(dest.clone(), dest_type.clone());
                let operation = match operator {
                    BinaryOperator::Add => Operation::Add(
                        dest_type,
                        dest.clone(),
                        Operand::Slot(op1),
                        Operand::Slot(op2),
                    ),
                    BinaryOperator::Sub => Operation::Sub(
                        dest_type,
                        dest.clone(),
                        Operand::Slot(op1),
                        Operand::Slot(op2),
                    ),
                    BinaryOperator::Mul => Operation::Mul(
                        dest_type,
                        dest.clone(),
                        Operand::Slot(op1),
                        Operand::Slot(op2),
                    ),
                    BinaryOperator::Div => Operation::Div(
                        dest_type,
                        dest.clone(),
                        Operand::Slot(op1),
                        Operand::Slot(op2),
                    ),
                    BinaryOperator::Eq => Operation::Eq(
                        dest_type,
                        dest.clone(),
                        Operand::Slot(op1),
                        Operand::Slot(op2),
                    ),
                    _ => todo!(),
                };
                self.push(operation);
                dest
            }
            Node::Identifier(identifier, _) => self.slots.last_variable(identifier),
            Node::Assignment(identifier, node, _) => {
                let op1 = self.parse_node(*node, type_handler, globals);
                let slot = self.slots.new_variable(identifier);
                self.push(Operation::Copy(slot.clone(), op1));
                slot
            }
            Node::FuncIdentifier(identifier, args, _) => {
                let mut slots = Vec::new();
                for node in args {
                    slots.push(self.parse_node(node, type_handler, globals));
                }
                let slot = self.slots.new_temp();
                self.push(Operation::Call(slot.clone(), identifier, slots));
                slot
            }
            Node::Unary(operator, expr, _) => {
                let child = self.parse_node(*expr.clone(), type_handler, globals);
                let slot = self.slots.new_temp();
                let child_type = infer_type(*expr, type_handler, globals, &self.slots);
                let operation = match operator {
                    UnaryOperator::Not => {
                        Operation::Not(child_type.t, slot.clone(), Operand::Slot(child))
                    }
                    UnaryOperator::Neg => {
                        Operation::Neg(child_type.t, slot.clone(), Operand::Slot(child))
                    }
                };
                self.push(operation);
                slot
            }
            Node::Parenthesis(node) => self.parse_node(*node, type_handler, globals),
        }
    }

    fn push(&mut self, operation: Operation) {
        let block: &mut Block = self.blocks.last_mut().unwrap();
        block.operations.push(operation);
    }

    fn generate_phis(
        &mut self,
        p_label: String,
        p_slots: Vec<Slot>,
        c_label: String,
        c_slots: Vec<Slot>,
    ) {
        for parent in &p_slots {
            for child in &c_slots {
                match (parent, child) {
                    (Slot::Variable(p_ident, p_i), Slot::Variable(c_ident, c_i)) => {
                        if p_ident == c_ident && p_i != c_i {
                            let dest = self.slots.new_variable(p_ident.clone());
                            self.push(Operation::Phi {
                                dest,
                                label1: p_label.clone(),
                                slot1: parent.clone(),
                                label2: c_label.clone(),
                                slot2: child.clone(),
                            })
                        }
                    }
                    _ => continue,
                }
            }
        }
    }
    fn new_branch_label(&mut self) -> String {
        self.branches += 1;
        format!("b{}", self.branches - 1)
    }

    fn new_join_label(&mut self) -> String {
        self.joins += 1;
        format!("j{}", self.joins - 1)
    }

    pub fn constant_fold_func(&mut self) {
        for block in &mut self.blocks {
            for operation in &mut block.operations {
                match operation {
                    Operation::Add(..) => {
                        if let Some(new_operation) =
                            self.slots
                                .n2_to_n(operation.clone(), |f1, f2| f1 + f2)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Sub(..) => {
                        if let Some(new_operation) =
                            self.slots
                                .n2_to_n(operation.clone(), |f1, f2| f1 - f2)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Mul(..) => {
                        if let Some(new_operation) =
                            self.slots
                                .n2_to_n(operation.clone(), |f1, f2| f1 * f2)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Div(..) => {
                        if let Some(new_operation) =
                            self.slots
                                .n2_to_n(operation.clone(), |f1, f2| f1 / f2)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Eq(_, slot, op1, op2) => {
                        if let Some(new_operation) =
                            self.slots
                                .n2_to_b(slot.clone(), op1.clone(), op2.clone(), |f1, f2| f1 == f2)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Not(_, slot, op) => {
                        if let Some(new_operation) =
                            self.slots.b_to_b(slot.clone(), op.clone(), |b| !b)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Neg(_, slot, op) => {
                        if let Some(new_operation) =
                            self.slots.n_to_n(slot.clone(), op.clone(), |b| -b)
                        {
                            *operation = new_operation
                        }
                    }
                    Operation::Copy(dest, slot) => {
                        if let Some(lit) = self.slots.constants.get(slot) {
                            //self.slots.constants.insert(dest.clone(), lit.clone())
                            *operation = Operation::Const(dest.clone(), lit.clone());
                        }
                    }
                    _ => (),
                }
            }
        }
    }
    pub fn eliminate_dead_code(&mut self) {
        let mut references = self.slots.temporaries();
        for block in &self.blocks {
            for (_i, operation) in block.operations.iter().enumerate() {
                for op in operation.operands() {
                    match op {
                        Operand::Slot(s) => {
                            references.remove(&s);
                        }
                        _ => (),
                    }
                }
            }
        }
        for block in &mut self.blocks {
            block.operations = block.operations.clone().into_iter().filter(|op| {
                if let Some(destination) = op.destination() {
                    println!("slot {} is {}", destination.clone(), !references.contains(&destination));
                    !references.contains(&destination)
                } else {
                    true 
                }
            }).collect();
        }
    }
}

impl Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.identifier)?;
        let mut args = self.args.clone();
        if let Some((last, _)) = args.pop() {
            for (ident, _) in args {
                write!(f, "{}0, ", ident)?;
            }
            write!(f, "{}0", last)?;
        }
        if let Some(ret) = self.ret.clone() {
            writeln!(f, ") -> {}", ret)?;
        } else {
            writeln!(f, ")")?;
        }
        writeln!(f, "locals:")?;
        for (slot, t) in &self.slots.slot_types {
            writeln!(
                f,
                "\t{}: {} \"{}\"",
                slot,
                t.identifier,
                self.slots.get_asm(slot.clone())
            )?;
        }
        writeln!(f, "temps:")?;
        for (slot, constant) in &self.slots.constants {
            writeln!(f, "\t{}: {}", slot, constant)?;
        }
        for block in &self.blocks {
            writeln!(f, "{}", block)?;
        }
        Ok(())
    }
}

#[allow(unused)]
#[derive(Debug)]
pub struct IR {
    type_handler: TypeHandler,
    pub functions: Vec<Func>,
}

impl IR {
    pub fn new(ast: Vec<Statement>) -> Self {
        let mut type_handler = TypeHandler::new();
        let mut functions: Vec<Func> = Vec::new();
        let declarations: HashMap<String, Declaration> = Declaration::map(&ast, &type_handler);
        for statement in ast {
            match statement {
                Statement::FuncDecl {
                    identifier,
                    args,
                    ret,
                    body,
                } => {
                    let mut function = Func::new(identifier, args, ret, &mut type_handler);
                    function.parse_stmt(*body, &mut type_handler, &declarations);
                    functions.push(function);
                }
                _stmt => (),
            }
        }
        IR {
            functions,
            type_handler,
        }
    }

    pub fn optimize(&mut self) {
        for func in &mut self.functions {
            func.constant_fold_func();
            func.constant_fold_func();
            func.constant_fold_func();
            func.eliminate_dead_code();
        }
    }
}

impl Display for IR {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in &self.functions {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Slot {
    Variable(String, usize),
    Temp(usize),
}

impl Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Slot::Variable(identifier, i) => write!(f, "{}{}", identifier, i),
            Slot::Temp(i) => write!(f, "t{}", i),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Location {
    Offset(isize),
    Register(String),
}

#[derive(Clone, Debug)]
pub enum Operand {
    Const(Litteral),
    Slot(Slot),
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Operand::Slot(s) => write!(f, "{}", s),
            Operand::Const(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug)]
pub struct Slots {
    temp_index: usize,
    slot_counters: HashMap<String, usize>,
    slot_types: HashMap<Slot, Type>,
    stack: HashMap<Slot, usize>,
    registers: HashMap<Slot, Register>,
    constants: HashMap<Slot, Litteral>,
}

impl Slots {
    pub fn new() -> Self {
        Self {
            slot_counters: HashMap::new(),
            temp_index: 0,
            slot_types: HashMap::new(),
            stack: HashMap::new(),
            registers: HashMap::new(),
            constants: HashMap::new(),
        }
    }

    pub fn temporaries(&self) -> HashSet<Slot> {
        let mut temporaries = HashSet::new();
        for i in 0..self.temp_index {
            temporaries.insert(Slot::Temp(i));
        }
        temporaries
    }

    fn n2_to_n<F>(&mut self, operation: Operation, func: F) -> Option<Operation>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let slot = operation.destination().unwrap();
        let op1 = operation.operands()[0].clone();
        let op2 = operation.operands()[1].clone();
        let res1 = self.operand_to_const(op1.clone());
        let res2 = self.operand_to_const(op2.clone());
        match (res1, res2) {
            (Some(Litteral::Number(f1)), Some(Litteral::Number(f2))) => {
                let res = func(f1, f2);
                self.constants.insert(slot.clone(), Litteral::Number(res));
                return Some(Operation::Const(slot.clone(), Litteral::Number(res)));
            },
            (None, Some(Litteral::Number(f2))) => {
                let op = operation.set_rhs(Operand::Const(Litteral::Number(f2)));
                return Some(op);
            },
            (Some(Litteral::Number(f1)), None) => {
                let op = operation.set_lhs(Operand::Const(Litteral::Number(f1)));
                return Some(op);
            },
            _ => None
        }
    }

    fn n2_to_b<F>(&mut self, slot: Slot, op1: Operand, op2: Operand, func: F) -> Option<Operation>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        if let Some(Litteral::Number(f1)) = self.operand_to_const(op1.clone()) {
            if let Some(Litteral::Number(f2)) = self.operand_to_const(op2.clone()) {
                let res = func(f1, f2);
                self.constants.insert(slot.clone(), Litteral::Boolean(res));
                return Some(Operation::Const(slot.clone(), Litteral::Boolean(res)));
            }
        }
        None
    }
    fn n_to_n<F>(&mut self, slot: Slot, op1: Operand, func: F) -> Option<Operation>
    where
        F: FnOnce(f64) -> f64,
    {
        if let Some(Litteral::Number(f1)) = self.operand_to_const(op1.clone()) {
            let res = func(f1);
            self.constants.insert(slot.clone(), Litteral::Number(res));
            return Some(Operation::Const(slot.clone(), Litteral::Number(res)));
        }
        None
    }
    fn b_to_b<F>(&mut self, slot: Slot, op1: Operand, func: F) -> Option<Operation>
    where
        F: FnOnce(bool) -> bool,
    {
        if let Some(Litteral::Boolean(f1)) = self.operand_to_const(op1.clone()) {
            let res = func(f1);
            self.constants.insert(slot.clone(), Litteral::Boolean(res));
            return Some(Operation::Const(slot.clone(), Litteral::Boolean(res)));
        }
        None
    }
    fn operand_to_const(&self, operand: Operand) -> Option<Litteral> {
        match operand {
            Operand::Const(lit) => Some(lit),
            Operand::Slot(slot) => self.constants.get(&slot).map(|t| t.clone()),
        }
    }

    pub fn last_temp(&self) -> Slot {
        Slot::Temp(self.temp_index - 1)
    }

    pub fn set_type(&mut self, slot: Slot, t: Type) {
        self.slot_types.insert(slot, t);
    }

    pub fn get_type(&self, slot: Slot) -> Type {
        self.slot_types[&slot].clone()
    }

    pub fn last_variable(&self, identifier: String) -> Slot {
        if let Some(slot_index) = self.slot_counters.get(&identifier) {
            Slot::Variable(identifier, slot_index - 1)
        } else {
            panic!("Identifier {} is undefined!", identifier);
        }
    }

    pub fn new_temp(&mut self) -> Slot {
        let slot = Slot::Temp(self.temp_index);
        self.temp_index += 1;
        slot
    }

    pub fn new_variable(&mut self, identifier: String) -> Slot {
        println!("{:#?}", self);
        let slot = if let Some(slot_index) = self.slot_counters.get_mut(&identifier) {
            let slot = Slot::Variable(identifier.clone(), slot_index.clone());
            *slot_index += 1;
            slot
        } else {
            self.slot_counters.insert(identifier.clone(), 1);
            println!("{}", identifier.clone());
            //let _slot_type = &self.slot_types[&identifier];
            let slot = Slot::Variable(identifier.clone(), 0);
            self.allocate_slot(slot.clone());
            slot
        };
        slot
    }

    pub fn new_constant(&mut self, identifier: Option<String>, constant: Litteral) -> Slot {
        let slot = if let Some(ident) = identifier {
            self.new_variable(ident)
        } else {
            self.new_temp()
        };
        self.constants.insert(slot.clone(), constant);
        slot
    }

    pub fn get_snapshot(&self) -> Vec<Slot> {
        let mut out = Vec::new();
        for (identifier, _) in &self.slot_counters {
            out.push(self.last_variable(identifier.clone()));
        }
        out
    }

    pub fn allocate_slot(&mut self, slot: Slot) {
        if self.stack.contains_key(&slot) || self.registers.contains_key(&slot) {
            return;
        }
        match &slot {
            Slot::Variable(identifier, _) => {
                let slot_type = self.slot_types[&slot].clone();
                if self.registers.len() <= 7 {
                    self.registers
                        .insert(slot, REGISTER_STACK[self.registers.len()].clone());
                } else {
                    self.stack.insert(slot, self.stack_size() + slot_type.size);
                }
            }
            Slot::Temp(_i) => {
                if self.registers.len() < 7 {
                    self.registers
                        .insert(slot, REGISTER_STACK[self.registers.len()].clone());
                } else {
                    self.stack.insert(slot, self.stack_size() + 16);
                }
            }
        }
    }

    pub fn stack_size(&self) -> usize {
        self.stack.len() * 16
    }

    pub fn get_asm(&self, slot: Slot) -> String {
        let register_opt = self.registers.get(&slot);
        let offset_opt = self.stack.get(&slot);
        match (&slot, register_opt, offset_opt) {
            (Slot::Variable(ident, _), Some(reg), None) => {
                let slot_type = self.get_type(slot);
                reg.with(slot_type)
            }
            (Slot::Temp(_), Some(reg), None) => reg.with(Type {
                identifier: "u64".to_owned(),
                size: 8,
            }),
            (_, None, Some(offset)) => {
                format!("[rsp - {}]", offset)
            }
            (_, Some(_), Some(_)) => panic!("slot stored multiple places!"),
            (s, None, None) => panic!("slot {} not stored anywhere!", s),
        }
    }
}

pub fn infer_type(
    node: Node,
    type_handler: &mut TypeHandler,
    globals: &HashMap<String, Declaration>,
    locals: &Slots,
) -> TypeValue {
    match node {
        Node::Binary {
            left,
            right,
            operator,
            position,
        } => match operator {
            BinaryOperator::L
            | BinaryOperator::G
            | BinaryOperator::Eq
            | BinaryOperator::Or
            | BinaryOperator::LEq
            | BinaryOperator::GEq
            | BinaryOperator::And
            | BinaryOperator::NEq => type_handler.get("bool").unwrap().clone().strong(position),
            _ => {
                let tleft = infer_type(*left, type_handler, globals, locals);
                let tright = infer_type(*right, type_handler, globals, locals);
                tleft | tright
            }
        },
        Node::Litteral(Litteral::Number(_), position) => Type::default().weak(position),
        Node::FuncIdentifier(ident, _, position) => {
            let func = globals[&ident].clone();
            func.ret_type.strong(position)
        }
        Node::Litteral(Litteral::Boolean(_), position) => {
            type_handler.get("bool").unwrap().clone().strong(position)
        }
        Node::Assignment(_, node, _) => infer_type(*node, type_handler, globals, locals),
        Node::Parenthesis(node) => infer_type(*node, type_handler, globals, locals),
        Node::Unary(UnaryOperator::Not, _, position) => {
            type_handler.get("bool").unwrap().clone().strong(position)
        }
        Node::Unary(UnaryOperator::Neg, _, position) => {
            type_handler.get("i32").unwrap().clone().weak(position)
        }
        Node::Identifier(identifier, position) => locals.get_type(Slot::Variable(identifier, 999)).strong(position),
        _ => todo!(),
    }
}

#[derive(Clone)]
pub struct Declaration {
    ret_type: Type,
    arg_types: Vec<Type>,
}

impl Declaration {
    fn map(stmts: &Vec<Statement>, type_handler: &TypeHandler) -> HashMap<String, Declaration> {
        let mut map = HashMap::new();
        for stmt in stmts {
            if let Some((identifier, declaration)) =
                Declaration::from_stmt(stmt.clone(), type_handler)
            {
                map.insert(identifier, declaration);
            }
        }
        map
    }
    fn from_stmt(stmt: Statement, type_handler: &TypeHandler) -> Option<(String, Declaration)> {
        match stmt {
            Statement::FuncDecl {
                identifier,
                args,
                ret,
                body: _,
            } => {
                let arg_types: Vec<Type> = args
                    .into_iter()
                    .map(|(_, t)| type_handler.get(t).unwrap().clone())
                    .collect();
                let ret_type = type_handler
                    .get(ret.unwrap_or(String::from("null")))
                    .unwrap()
                    .clone();
                let decl = Declaration {
                    arg_types,
                    ret_type,
                };
                Some((identifier, decl))
            }
            _ => None,
        }
    }
}
*/
