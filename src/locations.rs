use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use crate::{
    asm::Register,
    error::{ResError, ResErrorKind, Setter, Severity},
    ssa::{Place, PlaceKind, SlotID, SlotTable, StackFrame, ValueID},
    symbols::SymbolID,
    types::{TypeHandler, TypeID},
};

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
            ]),
            values: Vec::new(),
            bindings: HashMap::new(),
        }
    }

    pub fn lookup(&self, id: &SymbolID) -> Option<&ValueID> {
        self.bindings.get(id)
    }

    pub fn new_value(&mut self, typeid: TypeID, slots: &mut SlotTable) -> ValueID {
        let id = ValueID(self.values.len());
        let location = self.spill(typeid, slots);
        let value = Value { location, typeid, dropped: false };
        self.values.push(value);
        id
    }

    pub fn new_symbol(&mut self, typeid: TypeID, id: SymbolID, slots: &mut SlotTable) -> ValueID {
        let value_id = ValueID(self.values.len());
        let location = self.spill(typeid, slots);
        self.values.push(Value { location, typeid, dropped: false });
        self.bindings.insert(id, value_id);
        value_id
    }

    pub fn spill(&mut self, typeid: TypeID, slots: &mut SlotTable) -> ValueLocation {
        if self.used_regs.left() != 0 {
            ValueLocation::Register(self.used_regs.get_unused().unwrap())
        } else {
            ValueLocation::Spill(slots.new_temp(typeid))
        }
    }

    pub fn drop(&mut self, value_id: ValueID) {
        for v in self.bindings.values() {
            let value = self.get(&value_id);
            if let ValueLocation::Register(_) = value.location.clone() {
                if value_id == *v {
                    return;
                }
            }
        }
        let value = self.get_mut(&value_id);
        if let ValueLocation::Register(reg) = value.location.clone() {
            value.dropped = true;
            self.used_regs.drop(&reg);
        }
    }

    pub fn get(&self, value_id: &ValueID) -> Value {
        self.values[value_id.0].clone()
    }

    pub fn get_mut(&mut self, value_id: &ValueID) -> &mut Value {
        &mut self.values[value_id.0]
    }

    pub fn display(&self, value_id: &ValueID, th: &TypeHandler, frame: &StackFrame) -> String {
        let value = self.get(value_id);
        match value.location {
            ValueLocation::Register(reg) => {
                format!(
                    "{}",
                    reg.with_size(th.get(&value.typeid, None).unwrap().size)
                )
            }
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
                format!("{}", location)
            }
        }
    }

    pub fn get_leaks(&self) -> Vec<(ValueID, Value)> {
        self.values
            .iter()
            .enumerate()
            .filter(|(_, v)| {
                if let ValueLocation::Register(reg) = &v.location {
                    !v.dropped
                } else {
                    false
                }
            })
            .map(|(i, v)| (ValueID(i), v.clone()))
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ValueLocation {
    Spill(SlotID),
    Register(Register),
}

#[derive(Debug, Clone)]
pub struct Value {
    pub location: ValueLocation,
    pub typeid: TypeID,
    pub dropped: bool,
}

impl Value {
    pub fn size(&self, th: &TypeHandler) -> usize {
        th.get(&self.typeid, None).unwrap().size
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
            _ => panic!("Cant get pointer to stack loaction {:?}", self),
        };
        write!(f, "{} ptr [rbp - {}]", size, self.offset)
    }
}
