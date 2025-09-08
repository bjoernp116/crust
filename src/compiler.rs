use std::{collections::HashMap, fmt::Display};

use anyhow::anyhow;

use crate::{
    parser::{BinaryOperator, Litteral, Node, Statement},
    stack::Stack,
};

pub struct Compiler {
    assembly: String,
    stack_size: usize,
    stack: Stack,
    variables: HashMap<String, usize>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            assembly: String::new(),
            stack: Stack::new(),
            stack_size: 0,
            variables: HashMap::new(),
        }
    }
    pub fn gen_binary_operator(&mut self, operator: BinaryOperator) {
        self.push(";; bin op");
        match operator {
            BinaryOperator::Add => {
                self.push("add rax, rbx");
            }
            BinaryOperator::Sub => {
                self.push("sub rax, rbx");
            }
            _ => (),
        }
    }

    pub fn gen_identifier(&mut self, identifier: String) -> anyhow::Result<()> {
        self.push(";; identifier");
        if !self.variables.contains_key(&identifier) {
            return Err(anyhow!(
                "Undeclared identifier: {}",
                identifier
            ));
        }
        let stack_location = self.variables.get(&identifier).unwrap();
        self.push(
            format!(
                "QWORD [rsp + {}]",
                (self.stack_size - stack_location) * 8
            )
                .as_str(),
        );
        Ok(())
    }

    pub fn gen_litteral(&mut self, litteral: Litteral) {
        self.push(";; litteral");
        match litteral {
            Litteral::Number(n) => {
                self.push(format!("mov rax, {}", n).as_str());
                self.stack_push("rax")
            }
            _ => todo!(),
        }
    }

    pub fn gen_expr(&mut self, expr: Node) -> anyhow::Result<()> {
        self.push(";; expr");
        match expr {
            Node::Binary {
                left,
                operator,
                right,
                position,
            } => {
                self.gen_expr(*left)?;
                self.gen_expr(*right)?;
                self.stack_pop("rax");
                self.stack_pop("rbx");
                self.gen_binary_operator(operator);
                self.stack_push("rax");
            }
            Node::Parenthesis(node) => {
                self.gen_expr(*node)?;
            }
            Node::Litteral(litteral, _) => {
                self.gen_litteral(litteral);
            }
            Node::Identifier(identifier, _) => {
                self.gen_identifier(identifier)?;
            }
            _ => (),
        }
        Ok(())
    }

    pub fn gen_statement(&mut self, stmt: Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::Exit(node) => {
                self.push(";; exit");

                self.gen_expr(node)?;
                self.push("mov rax, 60");
                self.stack_pop("rdi");
                self.push("syscall")
            }
            Statement::VarDecl(identifier, node) => {
                self.push(";; variable decleration");
            }
            _ => {}
        }

        Ok(())
    }

    pub fn gen_program(&mut self, stmts: Vec<Statement>) -> anyhow::Result<()> {
        self.push_header("global _start");
        self.push_header("_start:");
        for statement in stmts {
            self.gen_statement(statement)?;
        }
        self.push("mov rax, 60");
        self.push("mov rdi, 0");
        self.push("syscall");
        Ok(())
    }

    fn stack_push(&mut self, register: &str) {
        self.stack_size += 1;
        self.push(format!("push {}", register).as_str());
    }

    fn stack_pop(&mut self, register: &str) {
        if self.stack_size == 0 {
            self.push(";; Stack size error!");
        } else {
            self.stack_size -= 1;
            self.push(format!("pop {}", register).as_str());
        }
    }

    fn push(&mut self, line: &str) {
        self.assembly.push('\t');
        self.push_header(line);
    }

    fn push_header(&mut self, line: &str) {
        self.assembly.push_str(line);
        self.assembly.push('\n');
    }
}

impl Display for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.assembly)
    }
}
