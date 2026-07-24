use crate::{functions::{self, FuncID, FuncSignature}, lexer::Position, ssa::{Block, Function, Operation, SSA, SlotID, SlotTable, StackFrame, StackLocation}, types::{Tables, TypeID}};


pub struct AsmWriter {
    tables: Tables,
    pub buffer: String,
}

impl AsmWriter {
    pub fn new(tables: Tables) -> AsmWriter {
        
        Self { tables, buffer: String::new() }
    }

    pub fn generate_funcs(&mut self, functions: Vec<Function>) {
        self.buffer.push_str(".intel_syntax noprefix\n.section .text\n.global main\n");
        for func in functions.iter() {
            for block in &func.blocks {
                self.generate_block(block, &func.slots.stack_frame(&self.tables.type_handler), &func.args);
            }
        }
    }

    pub fn generate_block(&mut self, block: &Block, frame: &StackFrame, args: &Vec<(SlotID, TypeID)>) {
        if block.label.entry() {
            self.buffer.push_str(&format!(".type {}, @function\n", block.label.ident()));
            self.buffer.push_str(&format!("{}:\n", block.label.ident()));
            self.buffer.push_str(&format!(".L{:?}:\n", block.label));
            self.push("push rbp");
            self.push("mov rbp, rsp");
            self.push(format!("sub rsp, {}\n", frame.size));

            for (i, (slot, typeid)) in args.iter().enumerate() {
                let reg = &ARG_REGISTERS[i];
                let l = frame.stack_map.get(&slot).unwrap();
                let ty = self.tables.type_handler.get(typeid, None).unwrap().clone();
                self.push(store(reg, l, ty.size))
            }
        } else {
            self.buffer.push_str(&format!(".L{:?}:\n", block.label));
        }

        for operation in &block.operations {
            self.generate_op(operation, frame);
        }
    }

    pub fn generate_op(&mut self, operation: &Operation, frame: &StackFrame) {
        match operation {
            Operation::Const(slot, litt) => {
                let location = frame.stack_map.get(&slot).unwrap();
                self.push(format!("mov {:?}, {}\n", location, litt));
            },
            Operation::Copy(sdest, ssrc) => {
                let ldest = frame.stack_map.get(&sdest).unwrap();
                let lsrc = frame.stack_map.get(&ssrc).unwrap();
                self.push(load(&Register::A, lsrc, lsrc.size));
                self.push(store(&Register::A, ldest, ldest.size));
                self.push("");
            },
            Operation::Eq(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("sete {:?}\n", ldest));
            },
            Operation::NEq(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setne {:?}\n", ldest));
            },
            Operation::G(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("seta {:?}\n", ldest));
            },
            Operation::GEq(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setae {:?}\n", ldest));
            },
            Operation::L(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setb {:?}\n", ldest));
            },
            Operation::LEq(_, sdest, s1, s2) => {
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setbe {:?}\n", ldest));
            },
            Operation::Add(ty, sdest, s1, s2) => {
                let ldest = frame.stack_map.get(&sdest).unwrap();
                let l1 = frame.stack_map.get(&s1).unwrap();
                let l2 = frame.stack_map.get(&s2).unwrap();
                let ty = self.tables.type_handler.get(&ty, None).unwrap().clone();
                self.push(load(&Register::A, l1, l1.size));
                self.push(load(&Register::C, l2, l2.size));
                self.push("add eax, ecx");
                self.push(store(&Register::A, ldest, ty.size));
                self.push("");
            }
            Operation::Sub(ty, sdest, s1, s2) => {
                let ldest = frame.stack_map.get(&sdest).unwrap();
                let l1 = frame.stack_map.get(&s1).unwrap();
                let l2 = frame.stack_map.get(&s2).unwrap();
                let ty = self.tables.type_handler.get(&ty, None).unwrap().clone();
                self.push(load(&Register::A, l1, l1.size));
                self.push(load(&Register::C, l2, l2.size));
                self.push("sub eax, ecx");
                self.push(store(&Register::A, ldest, ty.size));
                self.push("");
            }
            Operation::Mul(ty, sdest, s1, s2) => {
                let ldest = frame.stack_map.get(&sdest).unwrap();
                let l1 = frame.stack_map.get(&s1).unwrap();
                let l2 = frame.stack_map.get(&s2).unwrap();
                let ty = self.tables.type_handler.get(&ty, None).unwrap().clone();
                self.push(load(&Register::A, l1, l1.size));
                self.push(load(&Register::C, l2, l2.size));
                self.push("imul eax, ecx");
                self.push(store(&Register::A, ldest, ty.size));
                self.push("");
            }
            Operation::Return(opt_slot) => {
                if let Some(slot) = opt_slot {
                    let l = frame.stack_map.get(&slot).unwrap();
                    self.push(load(&Register::A, l, l.size));
                }
                self.push("mov rsp, rbp");
                self.push("pop rbp");
                self.push("ret\n");
            }
            Operation::Branch(slot, then_block, else_block) => {
                let l = frame.stack_map.get(&slot).unwrap();
                self.push(format!("cmp {:?}, 0", l));
                self.push(format!("je .L{:?}", else_block));
                self.push(format!("jmp .L{:?}\n", then_block));
            }
            Operation::Call(dest, id, slots) => {
                let func = self.tables.func_table.get(id).clone();
                for (i, (slot, type_id)) in slots.iter().zip(func.params).enumerate() {
                    let t = self.tables.type_handler.get(&type_id, None).unwrap();
                    let l = frame.stack_map.get(slot).unwrap();
                    let reg = &ARG_REGISTERS[i];
                    self.push(load(reg, l, t.size));
                }
                self.push(format!("call {}", func.identifier));
                if func.ret != TypeID::VOID {
                    let l = frame.stack_map.get(&dest).unwrap();
                    self.push(store(&Register::A, l, l.size));
                } 
                self.push("");
            }
            Operation::Jump(block) => {
                self.push(format!("jmp .L{:?}\n", block));
            }
            _ => todo!()
        }
    }

    fn boolean_operator_prefix(&mut self, frame: &StackFrame, sdest: &SlotID, s1: &SlotID, s2: &SlotID) -> StackLocation {
        let ldest = frame.stack_map.get(sdest).unwrap();
        let l1 = frame.stack_map.get(s1).unwrap();
        let l2 = frame.stack_map.get(s2).unwrap();
        self.push(load(&Register::A, l1, l1.size));
        self.push(format!("cmp {}, {:?}", Register::A.with_size(l1.size), l2));
        ldest.clone()
    }


    fn push(&mut self, str: impl ToString) {
        self.buffer.push('\t');
        self.buffer.push_str(&str.to_string());
        self.buffer.push('\n');
    }
}

fn zx_mov(size: usize) -> (String, String) {
    let eax = "eax".to_owned();
    let rax = "rax".to_owned();
    let mov = "mov".to_owned();
    let movzx = "movzx".to_owned();
    match size {
        1 | 2 => (movzx, eax),
        4 => (mov, eax),
        8 => (mov, rax),
        _ => todo!()
    }
}

#[derive(Debug, Clone)]
pub enum Register {
    A,
    B,
    C,
    D,
    SI,
    DI,
    R8,
    R9,
    R10,
    R11,
}

pub const ARG_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::D,
    Register::C,
    Register::R8,
    Register::R9,
];

pub const REGISTER_STACK: [Register; 7] = [
    Register::A,
    Register::C,
    Register::D,
    Register::R8,
    Register::R9,
    Register::R10,
    Register::R11,
];

impl Register {
    pub fn with_size(&self, size: usize) -> String {
        use self::*;
        let base = match &self {
            Register::A => "a",
            Register::B => "b",
            Register::C => "c",
            Register::D => "d",
            Register::R8 => "r8",
            Register::R9 => "r9",
            Register::R10 => "r10",
            Register::R11 => "r11",
            Register::SI => "si",
            Register::DI => "di",
        };
        match &self {
            Register::A | Register::B | Register::C | Register::D => {
                match size {
                    1 => format!("{}l", base),
                    2 => format!("{}x", base),
                    4 => format!("e{}x", base),
                    8 => format!("r{}x", base),
                    _ => unreachable!(),
                }
            }
            Register::R8 | Register::R9 | Register::R10 | Register::R11 => {
                match size {
                    1 => format!("{}b", base),
                    2 => format!("{}w", base),
                    4 => format!("{}d", base),
                    8 => format!("{}", base),
                    _ => unreachable!(),
                }
            }
            Register::SI | Register::DI => {
                match size {
                    1 => format!("{}l", base),
                    2 => format!("{}", base),
                    4 => format!("e{}", base),
                    8 => format!("r{}", base),
                    _ => unreachable!()
                }
            }
        }
    }
}

fn load(reg: &Register, location: &StackLocation, size: usize) -> String {
    let reg_str = reg.with_size(size.max(4));
    if size <= 2 {
        format!("movzx {reg_str}, {:?}", location)
    } else {
        format!("mov {reg_str}, {:?}", location)
    }
}

fn store(reg: &Register, location: &StackLocation, size: usize) -> String {
    let reg_str = reg.with_size(size);
    format!("mov {:?}, {reg_str}", location)
}
