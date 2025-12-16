use std::collections::HashMap;

use anyhow::anyhow;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub identifier: String,
    pub size: usize,
}

impl Default for Type {
    fn default() -> Self {
        Type {
            identifier: String::from("u32"),
            size: 4,
        }
    }
}

#[derive(Default)]
pub struct TypeHandler {
    types: HashMap<String, Type>,
}

impl TypeHandler {
    pub fn new() -> Self {
        let mut types = HashMap::new();
        types.insert(
            "u8".to_owned(),
            Type {
                identifier: String::from("u8"),
                size: 1,
            },
        );
        types.insert(
            "u16".to_owned(),
            Type {
                identifier: String::from("u16"),
                size: 2,
            },
        );
        types.insert(
            "u32".to_owned(),
            Type {
                identifier: String::from("u32"),
                size: 4,
            },
        );
        types.insert(
            "u64".to_owned(),
            Type {
                identifier: String::from("u64"),
                size: 8,
            },
        );
        Self { types }
    }

    pub fn get(&self, identifier: String) -> anyhow::Result<&Type> {
        self.types
            .get(&identifier)
            .ok_or(anyhow!("Undefined type: {}", identifier))
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
    pub fn with(&self, t: Type) -> String {
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
                match t.size {
                    1 => format!("{}l", base),
                    2 => format!("{}x", base),
                    4 => format!("e{}x", base),
                    8 => format!("r{}x", base),
                    _ => unreachable!(),
                }
            }
            Register::R8 | Register::R9 | Register::R10 | Register::R11 => {
                match t.size {
                    1 => format!("{}b", base),
                    2 => format!("{}w", base),
                    4 => format!("{}d", base),
                    8 => format!("{}", base),
                    _ => unreachable!(),
                }
            }
            Register::SI | Register::DI => {
                match t.size {
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
