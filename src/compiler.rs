use std::{collections::HashMap, fmt::Display};

use anyhow::anyhow;

use crate::{
    lexer::Position, parser::{BinaryOperator, Litteral, Node, Statement}, stack::{FuncStackFrame, StackFrameInfo, SymbolTable}, types::{Register, Type, TypeHandler}
};

#[derive(Clone)]
pub struct Function {
    ret: Option<Type>,
    args: Vec<(String, Type)>,
}

pub struct Compiler {
    assembly: String,
    type_handler: TypeHandler,
    symbol_table: SymbolTable,
    functions: HashMap<String, Function>,
    current_function: String,
    stack_size: isize,
    stack_frames: StackFrameInfo,
}

impl Compiler {
    pub fn new() -> Self {
        let assembly = String::new();
        Self {
            assembly,
            type_handler: TypeHandler::new(),
            symbol_table: SymbolTable::new(),
            functions: HashMap::new(),
            current_function: String::new(),
            stack_size: 0,
            stack_frames: StackFrameInfo::new()
        }
    }
    pub fn gen_binary_operator(&mut self, operator: BinaryOperator, t: Type) {
        self.push(";; bin op");
        match operator {
            BinaryOperator::Add => {
                self.push(format!(
                    "add {}, {}",
                    Register::A.with(t),
                    Register::B.with(t)
                ));
            }
            BinaryOperator::Sub => {
                self.push(format!(
                    "sub {}, {}",
                    Register::A.with(t),
                    Register::B.with(t)
                ));
            }
            BinaryOperator::Mul => {
                self.push("mul rbx");
            }
            BinaryOperator::Div => {
                self.push("xor rdx, rdx");
                self.push("div rbx");
            }
            _ => (),
        }
    }

    pub fn gen_identifier(
        &mut self,
        identifier: String,
        _: Option<Type>,
    ) -> anyhow::Result<Type> {
        self.push(";; identifier");
        let offset = self.symbol_table.get(&identifier);
        self.push(format!("mov rax, {}", offset));
        self.stack_push("rax");
        Ok(Type::default())
    }

    pub fn gen_litteral(&mut self, litteral: Litteral, t: Type) {
        self.push(";; litteral");
        match litteral {
            Litteral::Number(n) => {
                self.push(format!("mov {}, {}", Register::A.with(t), n));
                self.stack_push("rax")
            }
            _ => todo!(),
        }
    }

    pub fn gen_expr(
        &mut self,
        mut expr: Node,
        t: Option<Type>,
    ) -> anyhow::Result<Type> {
        expr.optimize();
        match expr {
            Node::Binary {
                left,
                operator,
                right,
                position: _,
            } => {
                let a_type = self.gen_expr(*left, t)?;
                let b_type = self.gen_expr(*right, t)?;
                let typ = if let Some(variable_type) = t {
                    variable_type
                } else {
                    Type {
                        size: usize::max(a_type.size, b_type.size),
                    }
                };
                self.stack_pop("rax");
                self.stack_pop("rbx");
                self.gen_binary_operator(operator, typ);
                self.stack_push("rax");
                Ok(typ)
            }
            Node::Parenthesis(node) => self.gen_expr(*node, t),
            Node::Litteral(litteral, _) => {
                if let Some(variable_type) = t {
                    self.gen_litteral(litteral, variable_type);
                    Ok(variable_type)
                } else {
                    self.gen_litteral(litteral, Type::default());
                    Ok(Type::default())
                }
            }
            Node::Identifier(identifier, _) => {
                self.gen_identifier(identifier, t)
            }
            Node::FuncIdentifier(identifier, args, _) => {
                self.push(";; function identifier");
                let arg_register = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                if !self.functions.contains_key(&identifier) {
                    return Err(anyhow!(
                        "Function {} not defined!",
                        identifier
                    ));
                }
                let function = self.functions[&identifier].clone();
                for (value, (_, t)) in
                    args.into_iter().zip(function.args.clone())
                {
                    self.gen_expr(value, Some(t))?;
                }
                for (i, (_, _)) in function.args.iter().enumerate() {
                    let register = arg_register[i];
                    self.stack_pop(register);
                }
                self.push(format!("call {}", identifier));
                if let Some(_) = function.ret {
                    self.stack_push("rax");
                }
                if let Some(ret_type) = function.ret {
                    Ok(ret_type)
                } else {
                    Ok(Type { size: 0 })
                }
            }
            _ => todo!(),
        }
    }

    pub fn gen_statement(&mut self, stmt: Statement) -> anyhow::Result<()> {
        match stmt {
            Statement::Exit(node) => {
                self.push_header(";; exit");
                self.gen_expr(node, None)?;
                self.push("mov rax, 60");
                self.stack_pop("rdi");
                self.push("syscall");
                self.push_header(";; /exit");
            }
            Statement::Return(node) => {
                self.push_header(";; return");
                println!("{}", self.current_function);
                if let Some(expr) = node {
                    let function =
                        self.functions[&self.current_function].clone();
                    let t = self.gen_expr(expr, function.ret)?;
                    match (function.ret, t) {
                        (Some(return_type), expr_type) => {
                            if return_type != expr_type {
                                return Err(anyhow!(
                                    "Return type mismatch in {}",
                                    self.current_function
                                ));
                            }
                        }
                        _ => (),
                    }
                }
                self.stack_pop("rax");
                self.push("mov rsp, rbp");
                self.stack_pop("rbp");
                self.push("ret");
                self.push_header(";; /return");
            }
            Statement::VarDecl(identifier, _, node) => {
                self.push_header(";; variable decleration");
                self.gen_expr(node, None)?;
                let offset = self.symbol_table.get(&identifier);
                self.stack_pop("rax");
                self.push(format!("mov {}, rax", offset));
                self.push_header(";; /variable decleration");
            }
            Statement::FuncDecl {
                identifier,
                args,
                ret,
                body,
            } => {
                self.current_function = identifier.clone();
                self.push_function(
                    identifier.clone(),
                    args.clone(),
                    ret.clone(),
                )?;
                self.stack_frames.push_func(identifier.clone());
                self.symbol_table.push_args(args, &mut self.type_handler)?;
                self.symbol_table
                    .find_locals(*body.clone(), &mut self.type_handler, &mut self.stack_frames)?;
                println!("{:#?}", self.symbol_table);
                self.push_header(format!("\n{}:", identifier));
                self.stack_push("rbp");
                self.push("mov rbp, rsp");
                let reserved = self.symbol_table.reserved();
                self.push(format!("sub rsp, {}", reserved));
                self.gen_statement(*body)?;
                if let None = ret {
                    self.push("mov rsp, rbp");
                    self.stack_pop("rbp");
                    self.push("ret");
                }
                self.symbol_table.clear();
            }
            Statement::Block(stmts) => {
                for statement in stmts {
                    self.gen_statement(statement)?;
                }
            }
            Statement::Expression(expr) => {
                self.gen_expr(expr, None)?;
            }
            _ => todo!(),
        }

        Ok(())
    }

    pub fn gen_program(&mut self, stmts: Vec<Statement>) -> anyhow::Result<()> {
        let (funcs, rest) = filter_function_statements(stmts);
        self.push_header("global _start");
        self.push_header("_start:");
        for statement in rest {
            self.gen_statement(statement)?;
        }
        self.push("call main");
        self.push("mov rax, 60");
        self.push("mov rdi, 0");
        self.push("syscall");

        for statement in funcs {
            self.gen_statement(statement)?;
        }
        println!("{:#?}", self.stack_frames);
        Ok(())
    }

    fn push_function(
        &mut self,
        identifier: String,
        arguments: Vec<(String, String)>,
        ret_typename: Option<String>,
    ) -> anyhow::Result<()> {
        let mut args: Vec<(String, Type)> = Vec::new();
        for arg in arguments {
            let (ident, typename) = arg;
            let t: Type = self.type_handler.get(typename)?.clone();
            args.push((ident, t));
        }
        let ret = if let Some(typename) = ret_typename {
            Some(self.type_handler.get(typename)?.clone())
        } else {
            None
        };
        let function = Function { args, ret };
        self.functions.insert(identifier, function);
        Ok(())
    }

    fn stack_push(&mut self, register: &str) {
        self.stack_size += 1;
        self.push(format!("push {}", register));
    }

    fn stack_pop(&mut self, register: &str) {
        self.stack_size -= 1;
        self.push(format!("pop {}", register));
    }

    fn push(&mut self, line: impl Into<String>) {
        self.assembly.push('\t');
        self.push_header(line);
    }

    fn push_header(&mut self, line: impl Into<String>) {
        self.assembly.push_str(&line.into());
        self.assembly.push('\n');
    }
}

impl Display for Compiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.assembly)
    }
}

fn filter_function_statements(
    stmts: Vec<Statement>,
) -> (Vec<Statement>, Vec<Statement>) {
    let mut funcs = Vec::new();
    let mut rest = Vec::new();
    for stmt in stmts {
        match stmt.clone() {
            Statement::FuncDecl {
                identifier: _,
                args: _,
                ret: _,
                body: _,
            } => {
                funcs.push(stmt);
            }
            _ => {
                rest.push(stmt);
            }
        }
    }
    (funcs, rest)
}

impl Node {
    pub fn optimize(&mut self) {
        match self {
            Node::Binary { left, right, operator, position } => {
                let mut left = *left.clone();
                let mut right = *right.clone();
                left.optimize();
                right.optimize();
                match (left, right) {
                    (Node::Litteral(l, _), Node::Litteral(r, _)) => {
                        if let Ok(optimized) = operator.eval(l, r) {
                            *self = Node::Litteral(optimized, position.clone()); 
                        }
                    }
                    _ => ()
                }
            },
            Node::Parenthesis(node) => node.optimize(),
            _ => ()
        }
    }
}


impl BinaryOperator {
    fn eval(&self, left: Litteral, right: Litteral) -> anyhow::Result<Litteral> {
        use Litteral::*;
        use BinaryOperator::*;

        match (left.clone(), self, right.clone()) {
            (Number(l), Eq,  Number(r)) => Ok(Boolean(l == r)),
            (Number(l), NEq,  Number(r)) => Ok(Boolean(l != r)),
            (Number(l), LEq,  Number(r)) => Ok(Boolean(l <= r)),
            (Number(l), GEq,  Number(r)) => Ok(Boolean(l >= r)),
            (Number(l), L,  Number(r)) => Ok(Boolean(l < r)),
            (Number(l), G,  Number(r)) => Ok(Boolean(l > r)),

            (Number(l), Add,  Number(r)) => Ok(Number(l + r)),
            (Number(l), Sub,  Number(r)) => Ok(Number(l - r)),
            (Number(l), Mul,  Number(r)) => Ok(Number(l * r)),
            (Number(l), Div,  Number(r)) => Ok(Number(l / r)),
            (Number(l), Pow,  Number(r)) => Ok(Number(l.powf(r))),

            (String(l), Eq, String(r)) => Ok(Boolean(l == r)),
            (String(l), NEq, String(r)) => Ok(Boolean(l != r)),
            (String(l), Add, String(r)) => Ok(String(format!("{}{}", l, r))),

            (Boolean(l), Eq, Boolean(r)) => Ok(Boolean(l == r)),
            (Boolean(l), NEq, Boolean(r)) => Ok(Boolean(l != r)),

            (String(_), Eq, Number(_)) => Ok(Boolean(false)),
            (Number(_), Eq, String(_)) => Ok(Boolean(false)),

            (Boolean(true), Or, _) => Ok(Boolean(true)),
            (Number(l), Or, _) => Ok(Number(l)),
            (String(l), Or, _) => Ok(String(l)),
            (_, Or, Boolean(true)) => Ok(Boolean(true)),
            (_, Or, String(r)) => Ok(String(r)),
            (_, Or, Number(r)) => Ok(Number(r)),
            (_, Or, _) => Ok(Boolean(false)),

            (Boolean(false) | Nil, And, _) => Ok(Boolean(false)),
            (_, And, Boolean(false) | Nil) => Ok(Boolean(false)),
            (Boolean(true), And, Boolean(true)) => Ok(Boolean(true)),
            (_, And, _) => Ok(Boolean(false)),


            (String(_), Add, Number(_)) |
            (Number(_), Add, String(_)) => Err(anyhow!("Operands must be two numbers or two strings")),
            (_, Add | Sub | Mul | Div | Pow, _) => Err(anyhow!("Operands must be numbers")),
            (_, Eq | NEq | LEq | GEq | L | G, _) => Err(anyhow!("Operands must be numbers")),
        }
    }
}
