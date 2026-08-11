use std::{collections::{HashMap, HashSet}, fmt::Display};

use crate::{asm::Register, error::Setter, ssa::{Place, PlaceKind, ValueID}, symbols::SymbolID, types::{TypeHandler, TypeID}};

#[derive(Debug)]
pub struct Locator {
    used_regs: Setter<Register>,
    values: Vec<Value>,
    bindings: HashMap<SymbolID, ValueID>,
}

impl Locator {
    pub fn new() -> Self {
        Self {
            used_regs: Setter::new(vec![
                Register::A,
                Register::C,
                Register::D,
                Register::SI,
                Register::DI,
                Register::R8,
                Register::R9,
                Register::R10,
                Register::R11,
            ]),
            values: Vec::new(),
            bindings: HashMap::new()
        }
    }

    pub fn lookup(&self, id: &SymbolID) -> Option<&ValueID> {
        self.bindings.get(id)
    }

    pub fn new_value(&mut self, typeid: TypeID) -> ValueID {
        let id = ValueID(self.values.len());
        let location = self.spill();
        let value = Value {
            location,
            typeid, 
        };
        self.values.push(value);
        id
    }

    pub fn new_symbol(&mut self, typeid: TypeID, id: SymbolID) -> ValueID {
        let value_id = ValueID(self.values.len());
        let location = self.spill();
        self.values.push(Value { location, typeid });
        self.bindings.insert(id, value_id);
        value_id
    }

    fn spill(&mut self) -> ValueLocation {
        if self.used_regs.left() != 0 {
            ValueLocation::Register(self.used_regs.get_unused().unwrap())
        } else {
            todo!()
        }
    }

    pub fn drop(&mut self, value: ValueID) {
        let value = self.get(&value);
        if let ValueLocation::Register(reg) = value.location {
            self.used_regs.drop(&reg);
        }
    }

    pub fn get(&self, value_id: &ValueID) -> Value {
        self.values[value_id.0].clone()
    }


    pub fn display(&self, value_id: &ValueID, th: &TypeHandler) -> String {
        let value = self.get(value_id);
        match value.location {
            ValueLocation::Register(reg) => {
                format!("{:?}", reg.with_size(th.get(&value.typeid, None).unwrap().size))
            },
            _ => todo!()
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueLocation {
    Spill(StackLocation),
    Register(Register),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub location: ValueLocation,
    pub typeid: TypeID,
}

impl Value {
    pub fn size(&self, th: &TypeHandler) -> usize {
        th.get(&self.typeid, None).unwrap().size
    }
}

impl ValueLocation {
    pub fn with_size(&self, size: usize) -> String {
        match self {
            Self::Spill(loc) => format!("{}", loc),
            Self::Register(reg) => format!("{}", reg.with_size(size))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StackLocation {
    pub offset: usize,
    pub size: usize,
    pub pointer: bool,
}

impl StackLocation {
    pub fn index(&self, rhs: &StackLocation) -> StackLocation {
        StackLocation {
            offset: self.offset + rhs.offset,
            size: rhs.size,
            pointer: self.pointer,
        }
    }
    pub fn new(offset: usize, size: usize) -> StackLocation {
        StackLocation {
            offset,
            size,
            pointer: false,
        }
    }
}

impl Display for StackLocation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let size = match self.size {
            1 => "byte",
            2 => "word",
            4 => "dword",
            8 => "qword",
            _ => "ERROR",
        };
        write!(f, "{} ptr [rbp - {}]", size, self.offset)
    }
}


