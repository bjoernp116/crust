use crate::{
    parser::{Litteral, Node, Statement},
    types::{Type, TypeHandler},
};
use std::collections::HashMap;

use anyhow::anyhow;

#[derive(Debug, Clone)]
enum Location {
    Offset(isize),
    Register(String),
    Order(usize),
}

impl Location {
    pub fn unwrap_order(&self) -> usize {
        if let Location::Order(order) = &self {
            return *order;
        }
        unreachable!()
    }
    pub fn unwrap_offset(&self) -> isize {
        if let Location::Offset(offset) = &self {
            return *offset;
        }
        unreachable!()
    }
    pub fn unwrap_register(&self) -> String {
        if let Location::Register(reg) = &self {
            return reg.clone();
        }
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub location: Location,
    pub variable_type: Type,
    pub explicit_type: bool,
    pub argument: bool,
}

fn size_to_asm_word(size: usize) -> String {
    match size {
        1 => "byte".to_owned(),
        2 => "word".to_owned(),
        4 => "dword".to_owned(),
        8 => "qword".to_owned(),
        _ => unreachable!(),
    }
}

#[derive(Debug)]
pub struct SymbolTable {
    table: HashMap<String, Variable>,
    sum: isize,
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
        self.get_symbols(stmt, type_handler)?;
        // sort
        let mut symbol_list: Vec<(String, Variable)> = self
            .table
            .clone()
            .into_iter()
            .filter(|(k, v)| matches!(v.location, Location::Order(_)))
            .map(|(k, v)| (k, v))
            .collect();

        symbol_list.sort_by(|(_, v1), (_, v2)| {
            let o1 = v1.location.unwrap_order();
            let o2 = v2.location.unwrap_order();
            o1.cmp(&o2)
        });

        for (ident, var) in symbol_list {
            self.sum += var.variable_type.size as isize;
            let entry: &mut Variable = self.table.get_mut(&ident).unwrap();
            entry.location = Location::Offset(-self.sum);
        }
        Ok(())
    }
    pub fn reserved(&self) -> usize {
        let quotient = self.sum as f32 / 16f32;
        let rounded: usize = quotient as usize + 1;
        rounded * 16usize
    }
    pub fn push_args(
        &mut self,
        args: Vec<(String, String)>,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<()> {
        if args.len() > 6 {
            return Err(anyhow!("More than 6 args not supported yet!"));
        }
        let arg_register = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        for (i, (ident, typename)) in args.into_iter().enumerate() {
            let var_type = type_handler.get(typename)?;
            let var = Variable {
                location: Location::Register(arg_register[i].to_owned()),
                variable_type: *var_type,
                explicit_type: true,
                argument: true,
            };
            self.table.insert(ident, var);
        }
        Ok(())
    }
    pub fn clear(&mut self) {
        self.sum = 0;
        self.table.clear();
    }
    pub fn get(&self, ident: &String) -> String {
        if let Some(var) = self.table.get(ident) {
            match var.location.clone() {
                Location::Offset(offset) => format!(
                    "{} [rbp {}]",
                    size_to_asm_word(var.variable_type.size),
                    offset
                ),
                Location::Register(reg) => format!("{}", reg),
                Location::Order(_) => panic!("variable unordered!"),
            }
        } else {
            panic!("{} not defined!", ident)
        }
    }

    fn get_symbols(
        &mut self,
        stmt: Statement,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<()> {
        match stmt {
            Statement::VarDecl(ident, t, node) => {
                let (variable_type, explicit_type) = if let Some(typename) = t {
                    (type_handler.get(typename)?.clone(), true)
                } else {
                    self.infer_type(node, type_handler)?
                };
                let var = Variable {
                    location: Location::Order(self.table.len()),
                    variable_type,
                    explicit_type,
                    argument: false,
                };
                self.table.insert(ident, var);
            }
            Statement::Block(stmts) => {
                for stmt in stmts {
                    self.get_symbols(stmt, type_handler)?;
                }
            }
            _ => (),
        }
        Ok(())
    }

    fn infer_type(
        &mut self,
        node: Node,
        type_handler: &mut TypeHandler,
    ) -> anyhow::Result<(Type, bool)> {
        match node {
            Node::Binary {
                left,
                right,
                operator: _,
                position: _,
            } => {
                let (l_t, l_e) = self.infer_type(*left, type_handler)?;
                let (r_t, r_e) = self.infer_type(*right, type_handler)?;
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
                    }
                }
            }
            Node::Litteral(lit, _) => match lit {
                Litteral::Number(n) => {
                    let t: &Type = match n {
                        ..256.0 => type_handler.get("u8".to_owned())?,
                        ..65536.0 => type_handler.get("u16".to_owned())?,
                        ..4294967296.0 => type_handler.get("u32".to_owned())?,
                        _ => type_handler.get("u64".to_owned())?,
                    };
                    Ok((*t, false))
                }
                _ => todo!(),
            },
            Node::Identifier(ident, _) => {
                if let Some(var) = self.table.get(&ident) {
                    Ok((var.variable_type, true))
                } else {
                    Err(anyhow!("Undefined variable: {}", ident))
                }
            }
            _ => todo!(),
        }
    }
}
