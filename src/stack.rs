use crate::{
    parser::{Litteral, Node, Statement},
    types::{ARG_REGISTERS, Register, Type, TypeHandler},
};
use std::collections::HashMap;

use anyhow::anyhow;

#[derive(Debug, Clone)]
pub enum Location {
    Offset(isize),
    Register(Register),
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
    pub fn unwrap_register(&self) -> Register {
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
        stack_info: &mut StackFrameInfo,
    ) -> anyhow::Result<()> {
        self.get_symbols(stmt, type_handler)?;
        // sort
        let mut symbol_list: Vec<(String, Variable)> = self
            .table
            .clone()
            .into_iter()
            .filter(|(_, v)| matches!(v.location, Location::Order(_)))
            .collect();

        symbol_list.sort_by(|(_, v1), (_, v2)| {
            let o1 = v1.location.unwrap_order();
            let o2 = v2.location.unwrap_order();
            o1.cmp(&o2)
        });

        for (ident, var) in symbol_list {
            self.sum += 8; //var.variable_type.size as isize;
            stack_info.push_var(ident.clone(), var.variable_type.size as u32);
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
        for (i, (ident, typename)) in args.into_iter().enumerate() {
            let var_type = type_handler.get(typename)?;
            let var = Variable {
                location: Location::Register(ARG_REGISTERS[i].to_owned()),
                variable_type: var_type.clone(),
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
    pub fn get_str(&self, ident: &String) -> String {
        if let Some(var) = self.table.get(ident) {
            match var.location.clone() {
                Location::Offset(offset) => format!(
                    "[rbp {}]",
                    //size_to_asm_word(var.variable_type.size),
                    offset
                ),
                Location::Register(reg) => {
                    format!("{}", reg.with(var.variable_type.clone()))
                }
                Location::Order(_) => panic!("variable unordered!"),
            }
        } else {
            panic!("{} not defined!", ident)
        }
    }
    pub fn get_location(&self, ident: &String) -> Location {
        if let Some(var) = self.table.get(ident) {
            var.location.clone()
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
                    Ok((t.clone(), false))
                }
                _ => todo!(),
            },
            Node::Identifier(ident, _) => {
                if let Some(var) = self.table.get(&ident) {
                    Ok((var.variable_type.clone(), true))
                } else {
                    Err(anyhow!("Undefined variable: {}", ident))
                }
            }
            _ => todo!(),
        }
    }
}

#[derive(Debug)]
pub struct FuncStackFrame {
    pub identifier: String,
    pub start: u32,
    pub frames: Vec<VarStackFrame>,
}

impl FuncStackFrame {
    pub fn size(&self) -> u32 {
        self.frames.iter().map(|vf| vf.size).sum()
    }
}

#[derive(Debug)]
pub struct VarStackFrame {
    pub identifier: String,
    pub start: u32,
    pub size: u32,
}

#[derive(Debug)]
pub struct StackFrameInfo {
    stack: Vec<FuncStackFrame>,
}

impl StackFrameInfo {
    pub fn new() -> StackFrameInfo {
        StackFrameInfo { stack: Vec::new() }
    }
    pub fn current_func_start(&self) -> u32 {
        if let Some(last) = self.stack.last() {
            last.start + last.size()
        } else {
            0
        }
    }
    pub fn push_func(&mut self, identifier: String) {
        let func = FuncStackFrame {
            identifier,
            start: self.current_func_start(),
            frames: Vec::new(),
        };
        self.stack.push(func)
    }
    pub fn push_var(&mut self, identifier: String, size: u32) {
        let last = self
            .stack
            .last_mut()
            .expect("Cant call push_var before push_func in StackFrameInfo!");
        let var = VarStackFrame {
            identifier,
            size,
            start: last.start + last.size(),
        };
        last.frames.push(var);
    }
}
