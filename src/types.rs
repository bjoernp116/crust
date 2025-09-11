use std::collections::HashMap;

use anyhow::anyhow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type {
    pub size: usize,
}

impl Default for Type {
    fn default() -> Self {
        Type { size: 4 }
    }
}

#[derive(Default)]
pub struct TypeHandler {
    types: HashMap<String, Type>,
}

impl TypeHandler {
    pub fn new() -> Self {
        let mut types = HashMap::new();
        types.insert("u8".to_owned(),  Type { size: 1 });
        types.insert("u16".to_owned(), Type { size: 2 });
        types.insert("u32".to_owned(), Type { size: 4 });
        types.insert("u64".to_owned(), Type { size: 8 });
        Self {
            types
        }
    }

    pub fn get(&self, identifier: &String) -> anyhow::Result<&Type> {
        self.types
            .get(identifier)
            .ok_or(anyhow!("Undefined type: {}", identifier))
    }
}


pub enum Register {
    A,
    B,
    C,
}

impl Register {
    pub fn with(&self, t: Type) -> String {
        let reg = match &self {
            Register::A => 'a',
            Register::B => 'b',
            Register::C => 'c'
        };
        match t.size {
            1 => format!("{}l", reg),
            2 => format!("{}x", reg),
            4 => format!("e{}x", reg),
            8 => format!("r{}x", reg),
            _ => unreachable!()
        }
    }
}


