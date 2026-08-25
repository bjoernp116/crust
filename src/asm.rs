use crate::{
    error::{ResError, ResErrorKind, ResResult, Severity},
    functions::{self, FuncID, FuncSignature},
    lexer::Position,
    locations::{Locator, StackLocation, ValueLocation},
    parser::Litteral,
    ssa::{
        BasicBlock, Function, Operation, Place, PlaceKind, SSA, SlotID, SlotTable, StackFrame,
        Terminator, ValueID,
    },
    symbols::SymbolID,
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

    pub fn buffer(&self) -> String {
        let mut buff_clone = self.buffer.clone();
        buff_clone.push_str("\n.section .rodata\n");
        for (label, data) in &self.data {
            buff_clone.push_str(label.as_str());
            buff_clone.push_str(":\n\t");
            buff_clone.push_str(data.as_str());
            buff_clone.push_str("\n");
        }
        buff_clone
    }

    pub fn generate_funcs(&mut self, functions: Vec<Function>) {
        self.buffer
            .push_str(".intel_syntax noprefix\n.section .text\n.global main\n");
        for func in functions.iter() {
            for block in &func.blocks {
                self.generate_block(block, &func.slots, &func.args, &func.locator);
            }
            println!("LEAKS: {:#?}", func.locator.get_leaks());
        }
    }

    pub fn generate_block(
        &mut self,
        block: &BasicBlock,
        slots: &SlotTable,
        args: &Vec<SymbolID>,
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

            for (i, sym) in args.iter().enumerate() {
                let reg = &ARG_REGISTERS[i];
                if let Some(id) = locator.lookup(sym) {
                    let value = locator.get(id);
                    let ty = self.tables.type_handler.get(&value.typeid, None).unwrap();
                    match value.location {
                        ValueLocation::Spill(_) => todo!(),
                        ValueLocation::Register(src) => {
                            self.push(Self::move_reg(&src, reg, ty.size));
                            continue;
                        }
                    }
                }
                if let Some(id) = slots.lookup(sym) {
                    let typeid = &slots.get(id.clone()).ty;
                    let l = frame.stack_map.get(&id).unwrap();
                    let ty = self.tables.type_handler.get(typeid, None).unwrap();
                    self.store(reg, todo!(), ty.size, slots, locator);
                    continue;
                }
                panic!("Symbol Not Defined!");
            }
        } else {
            self.buffer.push_str(&format!(".L{:?}:\n", block.label));
        }

        for operation in &block.operations {
            self.generate_op(operation, &frame, slots, locator);
        }
        self.generate_terminator(&block.terminator, locator, &frame);
    }

    pub fn generate_terminator(
        &mut self,
        terminator: &Terminator,
        locator: &Locator,
        frame: &StackFrame,
    ) {
        match terminator {
            Terminator::Return { value } => {
                self.push("# Return");
                if let Some(value) = value {
                    let l = locator.get(&value);
                    let ty = self.tables.type_handler.get(&l.typeid, None).unwrap();
                    self.push(format!(
                        "mov {}, {}",
                        Register::A.with_size(ty.size),
                        locator.display(&value, &self.tables.type_handler, frame)
                    ));
                }
                self.push("mov rsp, rbp");
                self.push("pop rbp");
                self.push("ret\n");
            }
            Terminator::Branch {
                predicate,
                destination,
                inverse,
            } => {
                self.push("# Branch");
                let l = locator.display(&predicate, &self.tables.type_handler, frame);
                self.push(format!("cmp {}, 0", l));
                self.push(format!("je .L{:?}", inverse));
                self.push(format!("jmp .L{:?}\n", destination));
            }
            Terminator::Jump { destination, } => {
                self.push("# Jump");
                self.push(format!("jmp .L{:?}\n", destination));
            }
            Terminator::Unknown => {
                self.push("mov rsp, rbp");
                self.push("pop rbp");
                self.push("ret\n");
            }
            _ => (),
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
                let destination = locator.display(value, &self.tables.type_handler, &frame);
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
                let value = locator.get(loc);
                match value.location {
                    ValueLocation::Register(reg) => {
                        self.push(format!(
                            "lea {}, [rip + {}]",
                            reg.with_size(8),
                            label
                        ));
                    },
                    ValueLocation::Spill(slot) => {
                        let location = frame
                            .stack_map
                            .get(&slot)
                            .ok_or(ResError {
                                kind: ResErrorKind::SlotNotFound(slot.clone()),
                                position: None,
                                severity: Severity::Error,
                            })
                            .map(|s| s.clone())
                            .unwrap();
                        self.push(format!(
                            "lea {}, [rip + {}]",
                            Register::R10.with_size(8),
                            label
                        ));
                        self.push(format!("mov {}, {}", location, Register::R10.with_size(8)))
                    }
                }
            }
            Operation::AddressOf(loc, place) => {
                self.push("# AddressOf");
                let value = locator.get(&loc);
                let ty = self.tables.type_handler.get(&value.typeid, None).unwrap().clone();
                match place.kind {
                    PlaceKind::Slot(slot) => {
                        let rbp_offset = frame.stack_map[&slot].offset;
                        match value.location {
                            ValueLocation::Register(reg) => {
                                self.push(format!(
                                    "lea {}, [rbp - {} + {}]",
                                    reg.with_size(8),
                                    rbp_offset,
                                    place.offset
                                ));
                            }
                            ValueLocation::Spill(slot) => {
                                self.push(format!(
                                    "lea {}, [rbp - {} + {}]",
                                    Register::R10.with_size(8),
                                    rbp_offset,
                                    place.offset
                                ));
                                let mut location = frame
                                    .stack_map
                                    .get(&slot)
                                    .ok_or(ResError {
                                        kind: ResErrorKind::SlotNotFound(slot.clone()),
                                        position: None,
                                        severity: Severity::Error,
                                    })
                                    .map(|s| s.clone()).unwrap();
                                
                                self.push(format!("mov {}, {}", location, &Register::R10.with_size(8)));
                            }
                        }
                    }
                    PlaceKind::Pointer(pointer) => { todo!() }
                }
            }
            Operation::Load(loc, place) => {
                self.push("# Load");
                let value = locator.get(loc);
                let ty = self.tables.type_handler.get(&value.typeid, None).unwrap().clone();
                match value.location {
                    ValueLocation::Register(reg) => {
                        self.load(&reg, place, ty.size, slots, locator);
                    }
                    ValueLocation::Spill(_) => {
                        self.load(&Register::R10, &place, ty.size, slots, locator);
                        self.push(format!(
                            "mov {}, {}",
                            locator.display(loc, &self.tables.type_handler, &frame),
                            &Register::R10.with_size(ty.size),
                        ));
                    }
                }
            }
            Operation::Move(dest, src) => {
                self.push("# Move");
                let src_str = self.value_location(src.clone(), locator, frame);
                self.push(format!(
                    "mov {}, {}",
                    locator.display(dest, &self.tables.type_handler, &frame),
                    src_str
                ));
            }
            Operation::Store(place, loc) => {
                self.push("# Store");
                let loc_str = self.value_location(loc.clone(), locator, frame);
                let place_str = self.place_str(&place, &locator, &slots).unwrap();
                self.push(format!(
                    "mov {}, {}",
                    place_str,
                    loc_str
                ));
            }
            Operation::Deref(dest, src) => {
                self.push("# Deref");
                let src_loc = locator.get(src).location;
                let dest_loc = locator.get(dest).location;
                match (src_loc, dest_loc) {
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
                let ty = self.tables.type_handler.get(ty, None).unwrap();
                let mut left: usize = ty.size;
                let mut offset: usize = 0;
                println!("copy {:?} <- {:?} ({:?})", sdest, ssrc, ty);
                let r = Register::R10;
                while left >= 8 {
                    self.load(&r, &ssrc.offset(offset, TypeID::U64), 8, slots, locator);
                    self.store(
                        &r,
                        &sdest.offset(offset, TypeID::U64),
                        8,
                        slots,
                        locator,
                    );
                    offset += 8;
                    left -= 8;
                }
                if left >= 4 {
                    self.load(&r, &ssrc.offset(offset, TypeID::U32), 4, slots, locator);
                    self.store(
                        &r,
                        &sdest.offset(offset, TypeID::U32),
                        4,
                        slots,
                        locator,
                    );
                    offset += 4;
                    left -= 4;
                }
                if left >= 2 {
                    self.load(&r, &ssrc.offset(offset, TypeID::U16), 2, slots, locator);
                    self.store(
                        &r,
                        &sdest.offset(offset, TypeID::U16),
                        2,
                        slots,
                        locator,
                    );
                    offset += 2;
                    left -= 2;
                }
                if left == 1 {
                    self.load(&r, &ssrc.offset(offset, TypeID::U8), 1, slots, locator);
                    self.store(&r, &sdest.offset(offset, TypeID::U8), 1, slots, locator);
                }
                self.push("");
            }
            Operation::Eq(_, sdest, s1, s2) => {
                self.push("# Eq");
                self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "sete {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::NEq(_, sdest, s1, s2) => {
                self.push("# NEq");
                let ldest = self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "setne {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::G(_, sdest, s1, s2) => {
                self.push("# G");
                let ldest = self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "seta {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::GEq(_, sdest, s1, s2) => {
                self.push("# GEq");
                let ldest = self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "setae {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::L(_, sdest, s1, s2) => {
                self.push("# L");
                let ldest = self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "setb {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::LEq(_, sdest, s1, s2) => {
                self.push("# LEq");
                let ldest = self.boolean_operator_prefix(locator, s1, s2, &frame);
                self.push(format!(
                    "setbe {}\n",
                    locator.display(sdest, &self.tables.type_handler, &frame)
                ));
            }
            Operation::Add(ty, sdest, s1, s2) => {
                self.push("# Add");
                let ldest = locator.display(&sdest, &self.tables.type_handler, &frame);
                let (l1, l2) = self.dual_value_location(s1.clone(), s2.clone(), locator, frame);
                self.push(format!("add {}, {}", l1, l2));
                self.push(format!("mov {}, {}", ldest, l1));
                self.push("");
            }
            Operation::Sub(ty, sdest, s1, s2) => {
                self.push("# Sub");
                let ldest = locator.display(&sdest, &self.tables.type_handler, &frame);
                let (l1, l2) = self.dual_value_location(s1.clone(), s2.clone(), locator, frame);
                self.push(format!("sub {}, {}", l1, l2));
                self.push(format!("mov {}, {}", ldest, l1));
                self.push("");
            }
            Operation::Mul(ty, sdest, s1, s2) => {
                self.push("# Mul");
                let ldest = locator.display(&sdest, &self.tables.type_handler, &frame);
                let (l1, l2) = self.dual_value_location(s1.clone(), s2.clone(), locator, frame);
                self.push(format!("imul {}, {}", l1, l2));
                self.push(format!("mov {}, {}", ldest, l1));
                self.push("");
            }
            Operation::Call(dest, id, slots) => {
                self.push("# Call");
                let func = self.tables.func_table.get(id).clone();
                for (i, (value_id, type_id)) in slots.iter().zip(func.params).enumerate() {
                    let t = self.tables.type_handler.get(&type_id, None).unwrap();
                    let reg = &ARG_REGISTERS[i];
                    self.push(format!(
                        "mov {}, {}",
                        reg.with_size(t.size),
                        locator.display(value_id, &self.tables.type_handler, &frame)
                    ));
                }
                self.push(format!("call {}", func.identifier));
                let ty = &self.tables.type_handler.get(&func.ret, None).unwrap();
                if ty.size != 0 {
                    let l = locator.display(&dest, &self.tables.type_handler, &frame);
                    self.push(format!("mov {}, {}", l, Register::A.with_size(ty.size)));
                }
                self.push("");
            }
            o => {
                println!("{:?}", o);
                todo!()
            }
        }
    }

    pub fn value_location(&mut self, id: ValueID, locator: &Locator, frame: &StackFrame) -> String {
        let value = locator.get(&id);
        let ty = self.tables.type_handler.get(&value.typeid, None).unwrap().clone();
        match value.location {
            ValueLocation::Register(reg) => {
                format!("{}", reg.with_size(ty.size))
            },
            ValueLocation::Spill(slot) => {
                let location = frame
                    .stack_map
                    .get(&slot)
                    .ok_or(ResError {
                        kind: ResErrorKind::SlotNotFound(slot.clone()),
                        position: None,
                        severity: Severity::Error,
                    })
                    .map(|s| s.clone())
                    .unwrap();
                self.push(format!("mov {}, {}", &Register::R10.with_size(ty.size), location));
                format!("{}", &Register::R10.with_size(ty.size))
            }
        }
    }

    pub fn dual_value_location(&mut self, id1: ValueID, id2: ValueID, locator: &Locator, frame: &StackFrame) -> (String, String) {
        let first_value = self.value_location(id1, locator, frame);
        let value = locator.get(&id2);
        let ty = self.tables.type_handler.get(&value.typeid, None).unwrap().clone();
        match value.location {
            ValueLocation::Register(reg) => {
                (first_value, format!("{}", reg.with_size(ty.size)))
            },
            ValueLocation::Spill(slot) => {
                let location = frame
                    .stack_map
                    .get(&slot)
                    .ok_or(ResError {
                        kind: ResErrorKind::SlotNotFound(slot.clone()),
                        position: None,
                        severity: Severity::Error,
                    })
                    .map(|s| s.clone())
                    .unwrap();
                self.push(format!("mov {}, {}", &Register::R11.with_size(ty.size), location));
                (first_value, format!("{}", &Register::R11.with_size(ty.size)))
            }
        }
    }

    fn boolean_operator_prefix(
        &mut self,
        locator: &Locator,
        s1: &ValueID,
        s2: &ValueID,
        frame: &StackFrame,
    ) {
        let (l1, l2) = self.dual_value_location(s1.clone(), s2.clone(), locator, frame);
        self.push(format!("cmp {}, {}", l1, l2));
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

    fn load(
        &mut self,
        reg: &Register,
        location: &Place,
        size: usize,
        slots: &SlotTable,
        locator: &Locator,
    ) {
        let reg_str = reg.with_size(size.max(8));
        if size <= 2 && !location.is_pointer() {
            let place_str = self.place_str(location, locator, slots).unwrap();
            self.push(format!(
                "movzx {reg_str}, {}", place_str
            ))
        } else {
            let place_str = self.place_str(location, locator, slots).unwrap();
            self.push(format!(
                "mov {reg_str}, {}", place_str
            ))
        }
    }

    fn move_reg(dest: &Register, src: &Register, size: usize) -> String {
        let dest_str = dest.with_size(size.max(4));
        let src_str = src.with_size(size.max(4));
        format!("mov {}, {}", dest_str, src_str)
    }

    fn store(
        &mut self,
        reg: &Register,
        location: &Place,
        size: usize,
        slots: &SlotTable,
        locator: &Locator,
    ) {
        let reg_str = reg.with_size(size);
        let place_str = self.place_str(location, locator, slots).clone().unwrap();
        self.push(format!(
            "mov {}, {reg_str}",
            place_str
        ))
    }

    pub fn place_str(
        &mut self,
        place: &Place,
        locator: &Locator,
        slots: &SlotTable,
    ) -> ResResult<String> {
        let frame = slots.stack_frame(&self.tables.type_handler);
        match place.kind {
            PlaceKind::Slot(s) => {
                let mut location = frame
                    .stack_map
                    .get(&s)
                    .ok_or(ResError {
                        kind: ResErrorKind::SlotNotFound(s.clone()),
                        position: None,
                        severity: Severity::Error,
                    })
                    .map(|s| s.clone())?;
                location.offset -= place.offset;
                location.size = location
                    .size
                    .min(self.tables.type_handler.get(&place.typeid, None)?.size);
                Ok(format!("{}", location))
            }
            PlaceKind::Pointer(id) => {
                let value = locator.get(&id);
                match value.location {
                    ValueLocation::Register(_) => {
                        Ok(format!(
                            "[{} + {}]",
                            locator.display(&id, &self.tables.type_handler, &frame),
                            place.offset
                        ))
                    },
                    ValueLocation::Spill(s) => {
                        let slot = slots.get(s);
                        let mut location = frame
                            .stack_map
                            .get(&s)
                            .ok_or(ResError {
                                kind: ResErrorKind::SlotNotFound(s.clone()),
                                position: None,
                                severity: Severity::Error,
                            })
                            .map(|s| s.clone())?;
                        self.push(format!("mov {}, {}", &Register::R10.with_size(8), location));
                        Ok(format!("[{} + {}]", &Register::R10.with_size(8), place.offset))
                    }
                }
            }
        }
    }
}

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
