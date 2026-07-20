/*
use crate::{
    chunks::{Func, IR, Operand, Operation, Slots},
    parser::Litteral,
};

pub struct Compiler {
    ir: IR,
    buffer: String,
}

impl Compiler {
    pub fn new(ir: IR) -> Compiler {
        Compiler {
            ir,
            buffer: String::new(),
        }
    }

    pub fn compile(&mut self) -> String {
        for function in &mut self.ir.functions {
            for block in &function.blocks {
                self.buffer.push_str(format!("{}: \n", block.label).as_str());
                for operation in &block.operations {
                    self.buffer.push_str(Self::parse_operation(&mut function.slots, operation).as_str());
                }
            }
        }
        println!("{}", self.buffer.clone());
        self.buffer.clone()
    }

    pub fn parse_operation(slots: &mut Slots, operation: &Operation) -> String {
        match operation {
            Operation::Const(slot, lit) => {
                slots.allocate_slot(slot.clone());
                let litteral = match lit {
                    Litteral::Number(n) => format!("{}", n),
                    Litteral::Boolean(true) => format!("1"),
                    Litteral::Boolean(false) => format!("0"),
                    _ => todo!(),
                };
                let location = slots.get_asm(slot.clone());
                format!("\tmov {}, {}\n", location, litteral)
            }
            Operation::Copy(dest, op) => {
                slots.allocate_slot(dest.clone());
                let slot = slots.get_asm(op.clone());
                let location = slots.get_asm(dest.clone());
                format!("\tmov {}, {}\n", location, slot)
            }
            Operation::Add(_t, dest, op1, op2) => {
                slots.allocate_slot(dest.clone());
                let dest = slots.get_asm(dest.clone());
                let v1 = match op1 {
                    Operand::Slot(s) => slots.get_asm(s.clone()),
                    Operand::Const(c) => format!("{}", c),
                };
                let v2 = match op2 {
                    Operand::Slot(s) => slots.get_asm(s.clone()),
                    Operand::Const(c) => format!("{}", c),
                };
                format!("\tadd {}, {}\n\tmov {}, {}\n", v1, v2, dest, v1)
            }
            op => {
                println!("- {}", op);
                todo!();
            }
        }
    }
}

*/
