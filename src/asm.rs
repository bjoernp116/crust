use crate::{
    error::ResErrorKind,
    functions::{self, FuncID, FuncSignature},
    lexer::Position,
    locations::{Locator, ValueLocation},
    parser::Litteral,
    ssa::{BasicBlock, Function, Operation, PlaceKind, SSA, SlotID, SlotTable, StackFrame},
    types::{Tables, TypeID},
};
pub struct AsmWriter {
    tables: Tables,
    buffer: String,
    data: Vec<(String, String)>,
}

impl AsmWriter {
    pub fn new(tables: Tables) -> AsmWriter {
        Self {
            tables,
            buffer: String::new(),
            data: Vec::new(),
        }
    }

}
/*
    pub fn buffer(&self) -> String {
        let mut buff_clone = self.buffer.clone();
        for (label, data) in &self.data {
            buff_clone.push_str("\n.section .rodata\n");
            buff_clone.push_str(label.as_str());
            buff_clone.push_str(":\n\t");
            buff_clone.push_str(data.as_str());
        }
        buff_clone
    }

    pub fn generate_funcs(&mut self, functions: Vec<Function>, locator: &Locator) {
        self.buffer
            .push_str(".intel_syntax noprefix\n.section .text\n.global main\n");
        for func in functions.iter() {
            for block in &func.blocks {
                self.generate_block(block, &func.slots, &func.args, locator);
            }
        }
    }

    pub fn generate_block(
        &mut self,
        block: &BasicBlock,
        slots: &SlotTable,
        args: &Vec<(SlotID, TypeID)>,
        locator: &Locator,
    ) {
        let frame = slots.stack_frame(&self.tables.type_handler);
        if block.label.entry() {
            self.buffer
                .push_str(&format!(".type {}, @function\n", block.label.ident()));
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
            self.generate_op(operation, &frame, slots, locator);
        }
    }

    pub fn generate_op(
        &mut self,
        operation: &Operation,
        frame: &StackFrame,
        slots: &SlotTable,
        locator: &Locator,
    ) {
        match operation {
            Operation::Const(value, litt) => {
                self.push("# Const");
                let destination = locator.display(value, &self.tables.type_handler);
                match litt {
                    Litteral::Number(n) => {
                        self.push(format!("mov {}, {}\n", destination, litt));
                    }
                    Litteral::Boolean(true) => {
                        self.push(format!("mov {}, 1\n", destination));
                    }
                    Litteral::Boolean(false) => {
                        self.push(format!("mov {}, 0\n", destination));
                    }
                    _ => todo!(),
                }
            }
            Operation::ConstStr(loc, string) => {
                self.push("# ConstStr");
                let label = self.push_data(format!(".ascii \"{}\"", string));
                todo!()
            }
            Operation::AddressOf(loc, place) => {
                self.push("# AddressOf");
                let dest = locator.display(loc, &self.tables.type_handler);
                match place.kind {
                    PlaceKind::Slot(slot) => {
                        let rbp_offset = frame.stack_map[&slot].offset;
                        self.push(format!(
                            "lea {}, [rbp - {}]",
                            dest,
                            rbp_offset,
                            //place_location.offset
                        ));
                    }
                    PlaceKind::Pointer(pointer) => {}
                }
            }
            Operation::Load(loc, place) => {
                self.push("# Load");
                let value = locator.get(loc);
                if value.size(&self.tables.type_handler) <= 2 {
                    self.push(format!("movzx {}, {}", locator.display(loc, &self.tables.type_handler)))
                } else {
                    self.push(format!("mov {}, {}", locator.display(loc, &self.tables.type_handler)))
                }
            }
            Operation::Store(place, loc) => {
                self.push("# Store");
                let place_location = slots
                    .place_location(
                        place.clone(),
                        &self.tables.struct_table,
                        &self.tables.type_handler,
                    )
                    .unwrap();
                match loc {
                    ValueLocation::Register(reg) => {
                        let reg_str = reg.with_size(place_location.size);
                        self.push(format!("mov {}, {reg_str}", place_location))
                    }
                    ValueLocation::Spill(stack_location) => todo!(),
                }
            }
            Operation::Deref(dest, src) => {
                self.push("# Deref");
                match (src, dest) {
                    (ValueLocation::Register(src_reg), ValueLocation::Register(dest_reg)) => {
                        let src_str = src_reg.with_size(8);
                        let dest_str = dest_reg.with_size(8);
                        self.push(format!("mov {dest_str}, [{src_str}]"))
                    }
                    _ => todo!(),
                }
            }
            Operation::Copy(sdest, ssrc, ty) => {
                self.push("# Copy");
                let ldest = slots
                    .place_location(
                        sdest.clone(),
                        &self.tables.struct_table,
                        &self.tables.type_handler,
                    )
                    .unwrap();
                let lsrc = slots
                    .place_location(
                        ssrc.clone(),
                        &self.tables.struct_table,
                        &self.tables.type_handler,
                    )
                    .unwrap();
                let ty = self.tables.type_handler.get(ty, None).unwrap();
                let mut left: usize = ty.size;
                let mut offset: usize = 0;

                while left >= 8 {
                    let loc = StackLocation::new(offset, 8);
                    self.push(format!("mov rax, {}", lsrc.index(&loc)));
                    self.push(format!("mov {}, rax", ldest.index(&loc)));
                    offset += 8;
                    left -= 8;
                }
                if left >= 4 {
                    let loc = StackLocation::new(offset, 4);
                    self.push(format!("mov eax, {}", lsrc.index(&loc)));
                    self.push(format!("mov {}, eax", ldest.index(&loc)));
                    offset += 4;
                    left -= 4;
                }
                if left >= 2 {
                    let loc = StackLocation::new(offset, 2);
                    self.push(format!("mov ax, {}", lsrc.index(&loc)));
                    self.push(format!("mov {}, ax", ldest.index(&loc)));
                    offset += 2;
                    left -= 2;
                }
                if left == 1 {
                    let loc = StackLocation::new(offset, 1);
                    self.push(format!("mov al, {}", lsrc.index(&loc)));
                    self.push(format!("mov {}, al", ldest.index(&loc)));
                }
                self.push("");
            }
            Operation::Eq(_, sdest, s1, s2) => {
                self.push("# Eq");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("sete {}\n", ldest));
            }
            Operation::NEq(_, sdest, s1, s2) => {
                self.push("# NEq");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setne {}\n", ldest));
            }
            Operation::G(_, sdest, s1, s2) => {
                self.push("# G");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("seta {}\n", ldest));
            }
            Operation::GEq(_, sdest, s1, s2) => {
                self.push("# GEq");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setae {}\n", ldest));
            }
            Operation::L(_, sdest, s1, s2) => {
                self.push("# L");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setb {}\n", ldest));
            }
            Operation::LEq(_, sdest, s1, s2) => {
                self.push("# LEq");
                let ldest = self.boolean_operator_prefix(frame, sdest, s1, s2);
                self.push(format!("setbe {}\n", ldest));
            }
            Operation::Add(ty, sdest, s1, s2) => {
                self.push("# Add");
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
                self.push("# Sub");
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
                self.push("# Mul");
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
                self.push("# Return");
                if let Some(slot) = opt_slot {
                    let l = frame.stack_map.get(&slot).unwrap();
                    self.push(load(&Register::A, l, l.size));
                }
                self.push("mov rsp, rbp");
                self.push("pop rbp");
                self.push("ret\n");
            }
            Operation::Branch(slot, then_block, else_block) => {
                self.push("# Branch");
                let l = frame.stack_map.get(&slot).unwrap();
                self.push(format!("cmp {}, 0", l));
                self.push(format!("je .L{:?}", else_block));
                self.push(format!("jmp .L{:?}\n", then_block));
            }
            Operation::Call(dest, id, slots) => {
                self.push("# Call");
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
                self.push("# Jump");
                self.push(format!("jmp .L{:?}\n", block));
            }
            _ => todo!(),
        }
    }

    fn boolean_operator_prefix(
        &mut self,
        frame: &StackFrame,
        sdest: &SlotID,
        s1: &SlotID,
        s2: &SlotID,
    ) -> StackLocation {
        let ldest = frame.stack_map.get(sdest).unwrap();
        let l1 = frame.stack_map.get(s1).unwrap();
        let l2 = frame.stack_map.get(s2).unwrap();
        self.push(load(&Register::A, l1, l1.size));
        self.push(format!("cmp {}, {}", Register::A.with_size(l1.size), l2));
        ldest.clone()
    }

    fn push(&mut self, str: impl ToString) {
        self.buffer.push('\t');
        self.buffer.push_str(&str.to_string());
        self.buffer.push('\n');
    }

    fn push_data(&mut self, field: impl ToString) -> String {
        let label = format!(".Lstr_{}", self.data.len());
        self.data.push((label.clone(), field.to_string()));
        label
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
        _ => todo!(),
    }
}

*/
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            Register::A | Register::B | Register::C | Register::D => match size {
                1 => format!("{}l", base),
                2 => format!("{}x", base),
                4 => format!("e{}x", base),
                8 => format!("r{}x", base),
                _ => unreachable!(),
            },
            Register::R8 | Register::R9 | Register::R10 | Register::R11 => match size {
                1 => format!("{}b", base),
                2 => format!("{}w", base),
                4 => format!("{}d", base),
                8 => format!("{}", base),
                _ => unreachable!(),
            },
            Register::SI | Register::DI => match size {
                1 => format!("{}l", base),
                2 => format!("{}", base),
                4 => format!("e{}", base),
                8 => format!("r{}", base),
                _ => unreachable!(),
            },
        }
    }
}

/*

fn load(reg: &Register, location: &StackLocation, size: usize) -> String {
    let reg_str = reg.with_size(size.max(4));
    if size <= 2 {
        format!("movzx {reg_str}, {}", location)
    } else {
        format!("mov {reg_str}, {}", location)
    }
}

fn store(reg: &Register, location: &StackLocation, size: usize) -> String {
    let reg_str = reg.with_size(size);
    format!("mov {}, {reg_str}", location)
}
*/
