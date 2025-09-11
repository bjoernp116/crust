use crate::{
    parser::{Node, Statement},
    types::{Type, TypeHandler},
};
use std::collections::HashMap;

use anyhow::anyhow;

#[derive(Debug, Clone)]
pub struct Variable {
    pub offset: isize,
    pub variable_type: Type,
    pub explicit_type: bool,
    pub argument: bool
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
    table: HashMap<String, Variable>,
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
        // sort
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
        symbol_table: &mut Vec<(String, Type)>,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<()> {
        match stmt {
            Statement::VarDecl(ident, t, node) => {
                let variable_type: Type = if let Some(typename) = t {
                    type_handler.get(&typename)?.clone()
                } else {
                    self.infer_type(&node).0
                };
                symbol_table.push((ident, variable_type));
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
    
    fn infer_type(node: Node) -> anyhow::Result<(Type, bool)> {
        match node {
            Node::Binary { left, right, operator: _, position: _ } => {
                let (l_t, l_e) = Self::infer_type(*left)?;
                let (r_t, r_e) = Self::infer_type(*right)?;
                match (l_e, r_e) {
                    (true, false) => Ok((l_t, true)),
                    (false, true) => Ok((r_t, true)),
                    (false, false) => Ok((Type::default(), false)),
                    (true, true) => {
                        if l_t == r_t {
                            Ok((l_t, true))
                        } else {
                            Err(anyhow!("type mismatch"))
                        }
                    },
                }
            } 
            _ => todo!()
        }
    }
}
