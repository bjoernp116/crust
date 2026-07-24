use std::{collections::VecDeque, fmt::Display};

use crate::{
    error::{ResError, ResErrorKind, ResResult},
    lexer::{Position, Token, TokenType},
    ssa::{Operation, SlotID},
    types::TypeID,
};
use anyhow::anyhow;

#[derive(Clone)]
pub enum Node {
    Binary {
        left: Box<Node>,
        right: Box<Node>,
        operator: BinaryOperator,
        position: Position,
    },
    Parenthesis(Box<Node>),
    Unary(UnaryOperator, Box<Node>, Position),
    Litteral(Litteral, Position),
    Identifier(String, Position),
    FuncIdentifier(String, Vec<Node>, Position),
    Assignment(String, Box<Node>, Position),
}

impl Node {
    pub fn position(&self) -> Position {
        #![allow(unused)]
        match self {
            Self::Binary {
                left,
                operator,
                right,
                position,
            } => position.clone(),
            Self::Unary(_, _, position) => position.clone(),
            Self::Litteral(_, position) => position.clone(),
            Self::Parenthesis(child) => child.position(),
            Self::Identifier(_, pos) => pos.clone(),
            Self::FuncIdentifier(_, _, pos) => pos.clone(),
            Self::Assignment(_, _, pos) => pos.clone(),
        }
    }
}

#[derive(Clone)]
pub enum Litteral {
    Number(f64),
    Boolean(bool),
    Nil,
    String(String),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Node),
    Exit(Node),
    Return(Option<Node>),
    VarDecl(String, Option<String>, Node),
    FuncDecl {
        identifier: String,
        args: Vec<(String, String)>,
        ret: Option<String>,
        body: Box<Statement>,
    },
    Block(Vec<Statement>, Option<Box<Statement>>),
    If(Node, Box<Statement>, Option<Box<Statement>>),
    While(Node, Box<Statement>),
    For(
        Option<Box<Statement>>,
        Option<Node>,
        Option<Node>,
        Box<Statement>,
    ),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Exit(t) => write!(f, "exit: {}", t)?,
            Statement::Return(t) => write!(f, "return: {:?}", t)?,
            Statement::Expression(e) => write!(f, "expr: {}", e)?,
            Statement::VarDecl(i, t, e) => write!(f, "decl: {} = {} (type = {:?})", i, e, t)?,
            Statement::Block(block, tail) => {
                writeln!(f, "block: {{\n")?;
                for stmnt in block {
                    writeln!(f, "\t{}", stmnt)?;
                }
                if let Some(s) = tail {
                    writeln!(f, "\t{}", s)?;
                }
                writeln!(f, "}}\n")?;
            }
            Statement::If(condition, then, els) => {
                writeln!(f, "if {}", condition)?;
                writeln!(f, "then {}", then)?;
                if let Some(el) = els {
                    writeln!(f, "else {}", el)?;
                }
            }
            Statement::While(condition, body) => {
                writeln!(f, "while {}", condition)?;
                writeln!(f, "do {}", body)?;
            }
            Statement::For(init, con, inc, body) => {
                writeln!(f, "for {:?}, {:?}, {:?}", init, con, inc)?;
                writeln!(f, "do {}", body)?;
            }
            Statement::FuncDecl {
                identifier,
                args,
                ret,
                body,
            } => {
                writeln!(f, "func {} ({:?}) -> {:?}", identifier, args, ret)?;
                writeln!(f, "do {}", body)?;
            }
        }
        Ok(())
    }
}

impl Display for Litteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Litteral::Number(n) => write!(f, "{}", n),
            Litteral::Boolean(b) => write!(f, "{}", b),
            Litteral::Nil => write!(f, "nil"),
            Litteral::String(s) => write!(f, "{}", s),
        }
    }
}

impl std::fmt::Debug for Litteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Litteral::Number(n) => write!(f, "{:?}", n),
            Litteral::Boolean(b) => write!(f, "{}", b),
            Litteral::Nil => write!(f, "nil"),
            Litteral::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Clone, Debug)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl UnaryOperator {
    pub fn to_operation(&self, type_id: TypeID, dest: SlotID, x: SlotID) -> Operation {
        use UnaryOperator::*;
        match self {
            Not => Operation::Not(type_id, dest, x),
            Neg => Operation::Neg(type_id, dest, x),
        }
    }
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Pow,

    Eq,
    LEq,
    GEq,
    NEq,
    L,
    G,

    Or,
    And,
}

impl BinaryOperator {
    pub fn is_boolean_out(&self) -> bool {
        use BinaryOperator::*;
        match self {
            Eq | NEq | G | GEq | L | LEq | Or | And => true,
            _ => false,
        }
    }
    pub fn is_numerical_out(&self) -> bool {
        use BinaryOperator::*;
        match self {
            Eq | NEq | G | GEq | L | LEq | Or | And => false,
            _ => true,
        }
    }
    pub fn is_boolean_in(&self) -> bool {
        use BinaryOperator::*;
        match self {
            And | Or => true,
            _ => false,
        }
    }
    pub fn is_numerical_in(&self) -> bool {
        use BinaryOperator::*;
        match self {
            And | Or => false,
            _ => true,
        }
    }

    pub fn to_operation(&self, type_id: TypeID, dest: SlotID, x: SlotID, y: SlotID) -> Operation {
        use BinaryOperator::*;
        match self {
            Add => Operation::Add(type_id, dest, x, y),
            Sub => Operation::Sub(type_id, dest, x, y),
            Mul => Operation::Mul(type_id, dest, x, y),
            Div => Operation::Div(type_id, dest, x, y),
            Eq => Operation::Eq(type_id, dest, x, y),
            L => Operation::L(type_id, dest, x, y),
            LEq => Operation::LEq(type_id, dest, x, y),
            G => Operation::G(type_id, dest, x, y),
            GEq => Operation::GEq(type_id, dest, x, y),
            NEq => Operation::NEq(type_id, dest, x, y),
            _ => todo!(),
        }
    }
}

impl Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op = match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Pow => "^",
            BinaryOperator::Eq => "==",
            BinaryOperator::LEq => "<=",
            BinaryOperator::GEq => ">=",
            BinaryOperator::NEq => "!=",
            BinaryOperator::L => "<",
            BinaryOperator::G => ">",
            BinaryOperator::Or => "||",
            BinaryOperator::And => "&&",
        };
        write!(f, "{}", op)
    }
}

pub struct AstFactory {
    //head: Node,
    current: usize,
    tokens: VecDeque<Token>,
}

impl AstFactory {
    pub fn new(input: Vec<Token>) -> Self {
        Self {
            current: 0,
            tokens: input.into(),
        }
    }
    pub fn is(&self, token_type: TokenType) -> bool {
        self.tokens[self.current].token_type == token_type
    }
    pub fn parse_statements(&mut self) -> ResResult<Vec<Statement>> {
        let mut out: Vec<Statement> = Vec::new();
        while self.current < self.tokens.len() {
            let node = self.parse_statement()?;
            out.push(node);
        }
        Ok(out)
    }
    pub fn parse_statement(&mut self) -> ResResult<Statement> {
        let out: ResResult<Statement> = match self.tokens[self.current].token_type {
            TokenType::Return => {
                self.current += 1;
                if let TokenType::SemiColon = self.tokens[self.current].token_type {
                    Ok(Statement::Return(None))
                } else {
                    let value = self.parse_assignment()?;
                    Ok(Statement::Return(Some(value)))
                }
            }
            TokenType::Exit => {
                self.current += 1;
                let value = self.parse_assignment()?;
                Ok(Statement::Exit(value))
            }
            TokenType::Let => {
                self.current += 1;
                let identifier = self.parse_number()?;
                if let Node::Identifier(name, _) = identifier {
                    let variable_type =
                        if let TokenType::Colon = self.tokens[self.current].token_type {
                            self.current += 2;
                            Some(self.tokens[self.current - 1].raw.clone())
                        } else {
                            None
                        };
                    match self.tokens[self.current].token_type {
                        TokenType::SemiColon => {
                            let pos = self.tokens[self.current].position.clone();
                            let expr = Node::Litteral(Litteral::Nil, pos);
                            self.current += 1;
                            Ok(Statement::VarDecl(name, variable_type, expr))
                        }
                        TokenType::Equal => {
                            self.current += 1;
                            let expr = self.parse_assignment()?;
                            Ok(Statement::VarDecl(name, variable_type, expr))
                        }
                        _ => {
                            eprintln!("Expected = or ; after variable declearation!");
                            std::process::exit(70);
                        }
                    }
                } else {
                    Err(ResError::new_err(
                        ResErrorKind::ExpectedIdentifier,
                        identifier.position(),
                    ))
                }
            }
            TokenType::LeftBrace => {
                let mut statements: Vec<Statement> = Vec::new();
                let mut tail = None;
                self.current += 1;
                while self.current < self.tokens.len() {
                    match self.tokens[self.current].token_type {
                        TokenType::RightBrace => {
                            self.current += 1;
                            break;
                        }
                        TokenType::SemiColon => self.current += 1,
                        _ => {
                            let stmt = self.parse_statement()?; 
                            if let TokenType::RightBrace = self.tokens[self.current].token_type {
                                tail = Some(Box::new(stmt))
                            } else {
                                statements.push(stmt);
                                self.assert_semicolon()?;
                            }
                        },
                    }
                    if self.current == self.tokens.len() {
                        return Err(ResError::new_err(ResErrorKind::UnexpectedEOF, self.tokens.iter().last().unwrap().position))
                    }
                }
                Ok(Statement::Block(statements, tail))
            }
            TokenType::If => {
                self.current += 1;
                let condition = self.parse_assignment()?;
                let statement = Box::new(self.parse_statement()?);
                let else_stmnt = if self.current < self.tokens.len() {
                    match self.tokens[self.current].token_type {
                        TokenType::Else => {
                            self.current += 1;
                            Some(Box::new(self.parse_statement()?))
                        }
                        _ => None,
                    }
                } else {
                    None
                };

                Ok(Statement::If(condition, statement, else_stmnt))
            }
            TokenType::While => {
                self.current += 1;
                match self.tokens[self.current].token_type {
                    TokenType::LeftParen => {}
                    _ => {
                        eprintln!("Expected ( after if!");
                        std::process::exit(65);
                    }
                }
                self.current += 1;
                let condition = self.parse_assignment()?;
                self.current += 1;
                let statement = Box::new(self.parse_statement()?);

                Ok(Statement::While(condition, statement))
            }
            TokenType::For => {
                self.current += 1;
                match self.tokens[self.current].token_type {
                    TokenType::LeftParen => {}
                    _ => {
                        eprintln!("Expected ( after for!");
                        std::process::exit(65);
                    }
                }
                self.current += 1;
                if self.is(TokenType::SemiColon) {
                    self.current += 1;
                    if self.is(TokenType::SemiColon) {
                        self.current += 1;
                        if self.is(TokenType::RightParen) {
                            self.current += 1;
                            let body = Box::new(self.parse_statement()?);
                            return Ok(Statement::For(None, None, None, body));
                        }
                        let increment = self.parse_assignment()?;
                        self.current += 1;
                        let body = Box::new(self.parse_statement()?);
                        return Ok(Statement::For(None, None, Some(increment), body));
                    } else {
                        let condition = self.parse_assignment()?;
                        self.current += 1;
                        if self.is(TokenType::RightParen) {
                            self.current += 1;
                            let body = Box::new(self.parse_statement()?);
                            return Ok(Statement::For(None, Some(condition), None, body));
                        }
                    }
                } else {
                }
                if let Ok(constructor) = self.parse_statement() {
                    if let Ok(condition) = self.parse_assignment() {
                        self.current += 1;
                        if let Ok(incrementer) = self.parse_assignment() {
                            self.current += 1;
                            let body = Box::new(self.parse_statement()?);
                            return Ok(Statement::For(
                                Some(Box::new(constructor)),
                                Some(condition),
                                Some(incrementer),
                                body,
                            ));
                        }
                        self.current += 1;
                        let body = Box::new(self.parse_statement()?);
                        return Ok(Statement::For(
                            Some(Box::new(constructor)),
                            Some(condition),
                            None,
                            body,
                        ));
                    }
                    self.current += 1;
                    let body = Box::new(self.parse_statement()?);
                    return Ok(Statement::For(
                        Some(Box::new(constructor)),
                        None,
                        None,
                        body,
                    ));
                }
                self.current += 1;
                let body = Box::new(self.parse_statement()?);

                Ok(Statement::For(None, None, None, body))
            }
            TokenType::Fun => {
                self.current += 1;
                let identifier_token = self.parse_var_identifier()?;
                self.current += 1;
                if let Node::Identifier(ident, pos) = identifier_token {
                    match self.tokens[self.current].token_type {
                        TokenType::LeftParen => {}
                        _ => {
                            return Err(ResError::new_err(
                                ResErrorKind::ExpectedInitializerList,
                                pos,
                            ));
                        }
                    }
                    self.current += 1;
                    let mut args = Vec::new();
                    loop {
                        let curr = self.tokens[self.current].token_type.clone();
                        let next = self.tokens[self.current + 1].token_type.clone();
                        match (curr, next) {
                            (_, TokenType::RightParen) => {
                                self.current += 1;
                                break;
                            }
                            (TokenType::RightParen, _) => break,
                            _ => {
                                let arg = self.parse_named_argument()?;
                                args.push(arg);
                            }
                        }
                    }
                    self.current += 1;
                    let mut ret = None;
                    if let TokenType::Arrow = self.tokens[self.current].token_type {
                        self.current += 1;
                        if let Node::Identifier(return_type, _) = self.parse_number()? {
                            ret = Some(return_type);
                        }
                    }
                    let body = self.parse_statement()?;
                    Ok(Statement::FuncDecl {
                        identifier: ident,
                        args,
                        ret,
                        body: Box::new(body),
                    })
                } else {
                    Err(ResError::new_err(
                        ResErrorKind::ExpectedIdentifier,
                        identifier_token.position(),
                    ))
                }
            }
            _ => {
                let value = self.parse_assignment()?;
                Ok(Statement::Expression(value))
            }
        };


        out
    }
    fn parse_named_argument(&mut self) -> ResResult<(String, String)> {
        let n = self.parse_number()?;
        if let Node::Identifier(identifier, pos) = n {
            if let TokenType::Colon = self.tokens[self.current].token_type {
                self.current += 1;
                if let Node::Identifier(typename, _) = self.parse_number()? {
                    if let TokenType::Comma = self.tokens[self.current].token_type {
                        self.current += 1;
                    };
                    Ok((identifier, typename))
                } else {
                    Err(ResError::new_err(ResErrorKind::ExpectedTypeIdentifier, pos))
                }
            } else {
                Err(ResError::new_err(ResErrorKind::ExpectedTypeIdentifier, pos))
            }
        } else {
            Err(ResError::new_err(
                ResErrorKind::ExpectedTypeIdentifier,
                n.position(),
            ))
        }
    }
    fn parse_assignment(&mut self) -> ResResult<Node> {
        let identifier: Node = self.parse_or()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Equal => {
                    if let Node::Identifier(name, pos) = identifier {
                        self.current += 1;
                        let value = self.parse_assignment()?;
                        let position = Position::range(pos, value.position());
                        let node = Node::Assignment(name, Box::new(value), position);
                        return Ok(node);
                    }
                }
                _ => break,
            }
        }
        Ok(identifier)
    }

    pub fn parse_or(&mut self) -> ResResult<Node> {
        let mut node = self.parse_and()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Or => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_and()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    pub fn parse_and(&mut self) -> ResResult<Node> {
        let mut node = self.parse_equality()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::And => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_equality()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }
    pub fn parse_equality(&mut self) -> ResResult<Node> {
        let mut node: Node = self.parse_term()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::EqualEqual
                | TokenType::GreaterEqual
                | TokenType::LessEqual
                | TokenType::BangEqual
                | TokenType::Greater
                | TokenType::Less => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_term()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_term(&mut self) -> ResResult<Node> {
        let mut node: Node = self.parse_factor()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Plus | TokenType::Minus => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_factor()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_factor(&mut self) -> ResResult<Node> {
        let mut node: Node = self.parse_exponent()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Star | TokenType::Slash => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_exponent()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_exponent(&mut self) -> ResResult<Node> {
        let mut node: Node = self.parse_primary()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Carrot => {
                    let op = self.tokens[self.current].clone();
                    self.current += 1;
                    if self.current >= self.tokens.len() {
                        break;
                    }
                    let right = Box::new(self.parse_primary()?);
                    let position = Position::range(node.position(), right.position());
                    node = Node::Binary {
                        left: Box::new(node),
                        right,
                        operator: op.try_into()?,
                        position,
                    };
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_primary(&mut self) -> ResResult<Node> {
        if self.current >= self.tokens.len() {
            return Err(ResError::new_err(
                ResErrorKind::UnexpectedEOF,
                self.tokens.iter().last().unwrap().position,
            ));
        }
        match self.tokens[self.current].token_type.clone() {
            TokenType::LeftParen => self.parse_paren(),
            TokenType::Bang | TokenType::Minus => self.parse_unary(),
            _ => self.parse_number(),
        }
    }

    fn parse_unary(&mut self) -> ResResult<Node> {
        let op = self.tokens[self.current].clone();
        self.current += 1;
        let node: Node = self.parse_primary()?;
        let position = Position::range(op.clone().position, node.position());
        let unary = Node::Unary(op.try_into()?, Box::new(node), position);
        Ok(unary)
    }

    fn parse_paren(&mut self) -> ResResult<Node> {
        let mut open_p = 0;
        let mut private_tokens: VecDeque<Token> = VecDeque::new();

        match self.tokens[self.current].token_type {
            TokenType::LeftParen => {}
            _ => {
                return self.parse_number();
            }
        };
        self.current += 1;
        open_p += 1;
        while self.current < self.tokens.len() && open_p != 0 {
            match self.tokens[self.current].token_type.clone() {
                TokenType::LeftParen => open_p += 1,
                TokenType::RightParen => open_p -= 1,
                _x => (), //println!("{:?}", x)
            }
            private_tokens.push_back(self.tokens[self.current].clone());
            self.current += 1;
        }
        if open_p != 0 {
            return Err(ResError::new_err(
                ResErrorKind::UnescapedParenthesis,
                self.tokens[self.current].position,
            ));
        }
        let mut parser = AstFactory {
            tokens: private_tokens,
            current: 0,
        };
        let node = parser.parse_assignment()?;
        Ok(Node::Parenthesis(Box::new(node)))
    }

    fn parse_number(&mut self) -> ResResult<Node> {
        if self.current >= self.tokens.len() {
            return Err(ResError::new_err(
                ResErrorKind::UnexpectedEOF,
                self.tokens.iter().last().unwrap().position,
            ));
        }
        let position = self.tokens[self.current].position.clone();
        match self.tokens[self.current].token_type.clone() {
            TokenType::Number(x) => {
                let number = x;
                self.current += 1;
                Ok(Node::Litteral(Litteral::Number(number as f64), position))
            }
            TokenType::True => {
                self.current += 1;
                Ok(Node::Litteral(Litteral::Boolean(true), position))
            }
            TokenType::False => {
                self.current += 1;
                Ok(Node::Litteral(Litteral::Boolean(false), position))
            }
            TokenType::Nil => {
                self.current += 1;
                Ok(Node::Litteral(Litteral::Nil, position))
            }
            TokenType::StringLitteral(s) => {
                self.current += 1;
                Ok(Node::Litteral(Litteral::String(s.clone()), position))
            }
            TokenType::Identifier(i) => {
                self.current += 1;
                if let TokenType::LeftParen = self.tokens[self.current].token_type {
                    self.parse_func_identifier(i, position)
                } else {
                    Ok(Node::Identifier(i.clone(), position))
                }
            }
            _ => Err(ResError::new_err(
                ResErrorKind::ExpectedExpression,
                position,
            )),
        }
    }

    fn parse_func_identifier(&mut self, i: String, position: Position) -> ResResult<Node> {
        self.current += 1;
        let mut args = Vec::new();
        loop {
            let prev = self.tokens[self.current - 1].token_type.clone();
            let curr = self.tokens[self.current].token_type.clone();
            match (prev, curr) {
                (TokenType::Comma | TokenType::LeftParen, TokenType::RightParen) => {
                    self.current += 1;
                    break;
                }
                (TokenType::RightParen, _) => break,
                _ => {
                    let arg = self.parse_equality()?;
                    args.push(arg);
                    self.current += 1;
                }
            }
        }
        Ok(Node::FuncIdentifier(i.clone(), args, position))
    }
    fn parse_var_identifier(&mut self) -> ResResult<Node> {
        let position = self.tokens[self.current].position.clone();
        if let TokenType::Identifier(i) = self.tokens[self.current].token_type.clone() {
            Ok(Node::Identifier(i, position))
        } else {
            Err(ResError::new_err(
                ResErrorKind::ExpectedExpression,
                position,
            ))
        }
    }

    fn assert_semicolon(&mut self) -> ResResult<()> {
        if let Some(token) = self.tokens.get(self.current) {
            if let TokenType::SemiColon = token.token_type {
                self.current += 1;
                Ok(())
            } else {
                Err(ResError::new_err(ResErrorKind::ExpectedSemicolon, self.tokens[self.current - 1].position))
            }
        } else {
            Ok(())
        }
    }
}

impl TryFrom<Token> for BinaryOperator {
    type Error = ResError;
    fn try_from(token: Token) -> ResResult<BinaryOperator> {
        match token.token_type {
            TokenType::Plus => Ok(BinaryOperator::Add),
            TokenType::Minus => Ok(BinaryOperator::Sub),
            TokenType::Star => Ok(BinaryOperator::Mul),
            TokenType::Slash => Ok(BinaryOperator::Div),
            TokenType::Carrot => Ok(BinaryOperator::Pow),
            TokenType::LessEqual => Ok(BinaryOperator::LEq),
            TokenType::GreaterEqual => Ok(BinaryOperator::GEq),
            TokenType::EqualEqual => Ok(BinaryOperator::Eq),
            TokenType::BangEqual => Ok(BinaryOperator::NEq),
            TokenType::Less => Ok(BinaryOperator::L),
            TokenType::Greater => Ok(BinaryOperator::G),
            TokenType::Or => Ok(BinaryOperator::Or),
            TokenType::And => Ok(BinaryOperator::And),
            tt => Err(ResError::new_err(
                ResErrorKind::TokenToOperator(tt),
                token.position,
            )),
        }
    }
}

impl TryFrom<Token> for UnaryOperator {
    type Error = ResError;
    fn try_from(token: Token) -> ResResult<UnaryOperator> {
        match token.token_type {
            TokenType::Bang => Ok(UnaryOperator::Not),
            TokenType::Minus => Ok(UnaryOperator::Neg),
            tt => Err(ResError::new_err(
                ResErrorKind::TokenToOperator(tt),
                token.position,
            )),
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #![allow(unused)]
        match self {
            Node::Unary(op, e, _) => write!(f, "({} {})", op, e),
            Node::Litteral(l, _) => write!(f, "{}", l),
            Node::Binary {
                left,
                right,
                operator,
                position,
            } => write!(f, "({} {} {})", operator, left, right),
            Node::Parenthesis(e) => write!(f, "(group {})", e),
            Node::Identifier(i, _) => write!(f, "_{}", i),
            Node::FuncIdentifier(i, a, _) => write!(f, "_{}({:?})", i, a),
            Node::Assignment(i, v, _) => write!(f, "{} = {}", i, v),
        }
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        #![allow(unused)]
        match self {
            Node::Unary(op, e, _) => write!(f, "({} {:?})", op, e),
            Node::Litteral(l, _) => write!(f, "{:?}", l),
            Node::Binary {
                left,
                right,
                operator,
                position,
            } => write!(f, "({} {:?} {:?})", operator, left, right),
            Node::Parenthesis(e) => write!(f, "(group {:?})", e),
            Node::Identifier(i, _) => write!(f, "_{}", i),
            Node::FuncIdentifier(i, a, _) => write!(f, "_{}({:?})", i, a),
            Node::Assignment(i, v, _) => write!(f, "{} = {}", i, v),
        }
    }
}
