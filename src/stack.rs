use crate::{
    parser::Statement,
    types::{Type, TypeHandler},
};
use std::collections::HashMap;

use anyhow::anyhow;

pub struct Variable {
    pub location: isize,
    pub variable_type: Type,
}

pub struct Stack {
    pub stack_size: isize,
    variables: HashMap<String, Variable>,
}

impl Stack {
    pub fn new() -> Stack {
        Stack {
            stack_size: 0,
            variables: HashMap::new(),
        }
    }
    pub fn declare_var(&mut self, identifier: String, variable_type: Type) {
        let variable = Variable {
            location: self.stack_size,
            variable_type,
        };
        self.variables.insert(identifier, variable);
    }
    pub fn push(&mut self, register: &str) -> String {
        self.stack_size += 1; //variable_type.size;
        format!("push {}", register)
    }
    pub fn pop(&mut self, register: &str) -> anyhow::Result<String> {
        if false {
            //self.stack_size == 0 {
            Err(anyhow!(";; Stack size error!"))
        } else {
            self.stack_size -= 1;
            Ok(format!("pop {}", register))
        }
    }
    pub fn contains(&self, identifier: &String) -> bool {
        self.variables.contains_key(identifier)
    }
    pub fn get(&self, identifier: &String) -> anyhow::Result<(String, Type)> {
        if let Some(variable) = self.variables.get(identifier) {
            let offset =
                (self.stack_size as isize - variable.location as isize - 1)
                    * 8 as isize;
            Ok((
                format!("QWORD [rsp + {}]", offset),
                variable.variable_type.clone(),
            ))
        } else {
            Err(anyhow!("Undecleared identifier!"))
        }
    }
}

fn size_to_asm_word(size: usize) -> String {
    match size {
        1 => "BYTE".to_owned(),
        2 => "WORD".to_owned(),
        4 => "DWORD".to_owned(),
        8 => "QWORD".to_owned(),
        _ => unreachable!(),
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    table: HashMap<String, isize>,
    pub sum: isize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            sum: 0,
        }
    }
    pub fn find_locals(
        &mut self,
        stmt: Statement,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<()> {
        let mut symbol_list = Vec::new();
        Self::get_symbols(stmt, &mut symbol_list, type_handler)?;
        symbol_list.sort_by(|c, n| c.1.cmp(&n.1));
        symbol_list.reverse();
        for (ident, type_size) in symbol_list {
            self.sum += type_size as isize;
            self.table.insert(ident, self.sum as isize);
        }
        Ok(())
    }
    pub fn push_args(
        &mut self,
        args: Vec<(String, String)>,
    ) -> anyhow::Result<()> {
        if args.len() > 6 {
            return Err(anyhow!("More than 6 args not supported yet!"));
        }
        for (i, (ident, _)) in args.into_iter().enumerate() {
            self.table.insert(ident, i as isize + 1);
        }
        Ok(())
    }
    pub fn clear(&mut self) {
        self.sum = 0;
        self.table.clear();
    }
    pub fn get(&self, ident: &String) -> String {
        let arg_register = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        if let Some(&offset) = self.table.get(ident) {
            if offset <= 0 {
                format!("QWORD [rbp {}]", offset)
            } else {
                format!("{}", arg_register[offset as usize])
            }
        } else {
            panic!("{} not defined!", ident)
        }
    }

    fn get_symbols(
        stmt: Statement,
        symbol_table: &mut Vec<(String, usize)>,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<()> {
        match stmt {
            Statement::VarDecl(ident, t, _) => {
                let type_size: usize = if let Some(typename) = t {
                    type_handler.get(&typename)?.size
                } else {
                    Type::default().size
                };
                symbol_table.push((ident, type_size));
            }
            Statement::Block(stmts) => {
                for stmt in stmts {
                    Self::get_symbols(stmt, symbol_table, type_handler)?;
                }
            }
            _ => (),
        }
        Ok(())
    }
}
