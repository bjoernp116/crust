use std::{collections::VecDeque, fmt::Display};

use crate::{
    error::{ResError, ResErrorKind, ResResult},
    lexer::{Position, Token, TokenType},
    locations::ValueLocation,
    ssa::{Operation, SlotID, ValueID},
    types::TypeID,
};

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
    FieldAccess(Box<Node>, String, Position),
    FuncIdentifier(String, Vec<Node>, Position),
    Assignment(Box<Node>, Box<Node>, Position),
    If(Box<Node>, Block, Option<Block>, Position),
    Block(Block),
    Loop(Block),
    Constructor(String, Vec<(String, Node)>, Position),
    Address(bool, Box<Node>, Position),
    Deref(Box<Node>, Position),
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
            Self::If(_, _, _, pos) => pos.clone(),
            Self::Block(block) => block.position.clone(),
            Self::Loop(block) => block.position.clone(),
            Self::Constructor(_, _, pos) => pos.clone(),
            Self::FieldAccess(_, _, pos) => pos.clone(),
            Self::Address(_, _, pos) => pos.clone(),
            Self::Deref(_, pos) => pos.clone(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
    pub tail: Option<Box<Node>>,
    pub position: Position,
}

impl Block {
    pub fn collect(&self) -> Vec<Statement> {
        let mut v = self.stmts.clone();
        if let Some(tail) = &self.tail {
            v.push(Statement::Expression(*tail.clone()));
        }
        v
    }
}

#[derive(Clone)]
pub enum Litteral {
    Number(usize),
    Boolean(bool),
    Nil,
    String(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum TypeSyntax {
    Raw(String),
    Reference {
        mutable: bool,
        pointee: Box<TypeSyntax>,
    },
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression(Node),
    Return(Option<Node>),
    Break(Option<Node>),
    VarDecl(String, Option<TypeSyntax>, bool, Node),
    FuncDecl {
        identifier: String,
        args: Vec<(String, TypeSyntax)>,
        ret: Option<String>,
        body: Block,
    },
    While(Node, Block),
    For(
        Option<Box<Statement>>,
        Option<Node>,
        Option<Node>,
        Box<Statement>,
    ),
    Struct(String, Vec<(String, TypeSyntax)>),
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Return(t) => write!(f, "return: {:?}", t)?,
            Statement::Break(t) => write!(f, "break: {:?}", t)?,
            Statement::Expression(e) => write!(f, "expr: {}", e)?,
            Statement::VarDecl(i, t, m, e) => {
                write!(f, "{} decl: {} = {} (type = {:?})", m, i, e, t)?
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
            Statement::Struct(ident, fields) => {
                writeln!(f, "struct {} {{{:#?}}}", ident, fields)?;
            }
        }
        Ok(())
    }
}

impl Display for Litteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Litteral::Number(n) => write!(f, "{}", n),
            Litteral::Boolean(true) => write!(f, "1"),
            Litteral::Boolean(false) => write!(f, "0"),
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
    pub fn to_operation(&self, type_id: TypeID, dest: ValueID, x: ValueID) -> Operation {
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
    pub fn to_operation(
        &self,
        type_id: TypeID,
        dest: ValueID,
        x: ValueID,
        y: ValueID,
    ) -> Operation {
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

    pub fn interpret(&self, left: Litteral, right: Litteral) -> Option<Litteral> {
        match (left, right) {
            (Litteral::Number(l), Litteral::Number(r)) => {
                let litt = match self {
                    Self::Eq => Litteral::Boolean(l == r),
                    Self::NEq => Litteral::Boolean(l != r),
                    Self::G => Litteral::Boolean(l > r),
                    Self::GEq => Litteral::Boolean(l >= r),
                    Self::L => Litteral::Boolean(l < r),
                    Self::LEq => Litteral::Boolean(l <= r),
                    Self::Add => Litteral::Number(l + r),
                    Self::Sub => Litteral::Number(l - r),
                    Self::Mul => Litteral::Number(l * r),
                    Self::Div => Litteral::Number(l / r),
                    _ => todo!(),
                };
                Some(litt)
            }
            _ => None,
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
        self.debug("parse_statement");
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
            TokenType::Break => {
                self.current += 1;
                if let TokenType::SemiColon = self.tokens[self.current].token_type {
                    Ok(Statement::Break(None))
                } else {
                    let value = self.parse_assignment()?;
                    Ok(Statement::Break(Some(value)))
                }
            }
            TokenType::Let => {
                self.current += 1;
                let mutable = if let TokenType::Mut = self.tokens[self.current].token_type {
                    self.current += 1;
                    true
                } else {
                    false
                };
                let identifier = self.parse_number()?;
                if let Node::Identifier(name, _) = identifier {
                    let variable_type =
                        if let TokenType::Colon = self.tokens[self.current].token_type {
                            self.current += 1;
                            if let TokenType::Reference = self.tokens[self.current].token_type {
                                self.current += 1;
                                let base = self.tokens[self.current].raw.clone();
                                Some(TypeSyntax::Reference {
                                    mutable: false,
                                    pointee: Box::new(TypeSyntax::Raw(base)),
                                })
                            } else {
                                Some(TypeSyntax::Raw(self.tokens[self.current].raw.clone()))
                            }
                        } else {
                            self.current -= 1;
                            None
                        };
                    self.current += 1;
                    match self.tokens[self.current].token_type {
                        TokenType::SemiColon => {
                            let pos = self.tokens[self.current].position.clone();
                            let expr = Node::Litteral(Litteral::Nil, pos);
                            self.current += 1;
                            Ok(Statement::VarDecl(name, variable_type, mutable, expr))
                        }
                        TokenType::Equal => {
                            self.current += 1;
                            let expr = self.parse_assignment()?;
                            Ok(Statement::VarDecl(name, variable_type, mutable, expr))
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
                let block = self.parse_block()?;

                Ok(Statement::While(condition, block))
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
                        if let Node::Identifier(return_type, _) = self.parse_var_identifier()? {
                            ret = Some(return_type);
                        }
                        self.current += 1;
                    }
                    let body = self.parse_block()?;
                    Ok(Statement::FuncDecl {
                        identifier: ident,
                        args,
                        ret,
                        body,
                    })
                } else {
                    Err(ResError::new_err(
                        ResErrorKind::ExpectedIdentifier,
                        identifier_token.position(),
                    ))
                }
            }
            TokenType::Struct => {
                self.current += 1;
                let identifier_token = self.parse_var_identifier()?;
                self.current += 1;
                if let Node::Identifier(ident, pos) = identifier_token {
                    match self.tokens[self.current].token_type {
                        TokenType::LeftBrace => {}
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
                            (_, TokenType::RightBrace) => {
                                self.current += 1;
                                break;
                            }
                            (TokenType::RightBrace, _) => break,
                            _ => {
                                let arg = self.parse_named_argument()?;
                                args.push(arg);
                            }
                        }
                    }
                    self.current += 1;
                    Ok(Statement::Struct(ident, args))
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
    fn parse_named_argument(&mut self) -> ResResult<(String, TypeSyntax)> {
        self.debug("parse_named_argument");
        let n = self.parse_number()?;
        if let Node::Identifier(identifier, pos) = n {
            if let TokenType::Colon = self.tokens[self.current].token_type {
                self.current += 1;
                let typename = self.parse_type_syntax()?;
                if let TokenType::Comma = self.tokens[self.current].token_type {
                    self.current += 1;
                }
                Ok((identifier, typename))
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
        self.debug("parse_assignment");
        let identifier: Node = self.parse_or()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Equal => {
                    if let Node::Identifier(_, pos) = identifier.clone() {
                        self.current += 1;
                        let value = self.parse_assignment()?;
                        let position = Position::range(pos, value.position());
                        let node =
                            Node::Assignment(Box::new(identifier), Box::new(value), position);
                        return Ok(node);
                    } else if let Node::FieldAccess(_, _, pos) = identifier.clone() {
                        self.current += 1;
                        let value = self.parse_assignment()?;
                        let position = Position::range(pos, value.position());
                        let node =
                            Node::Assignment(Box::new(identifier), Box::new(value), position);
                        return Ok(node);
                    } else {
                        return Err(ResError::new_err(
                            ResErrorKind::ExpectedIdentifier,
                            self.tokens[self.current].position,
                        ));
                    }
                }
                _ => break,
            }
        }
        Ok(identifier)
    }
    pub fn parse_or(&mut self) -> ResResult<Node> {
        self.debug("parse_or");
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

    pub fn parse_type_syntax(&mut self) -> ResResult<TypeSyntax> {
        self.debug("parse_type_syntax");
        if let TokenType::Reference = self.tokens[self.current].token_type {
            self.current += 1;
            if let TokenType::Identifier(base) = &self.tokens[self.current].token_type {
                self.current += 1;
                Ok(TypeSyntax::Reference {
                    mutable: false,
                    pointee: Box::new(TypeSyntax::Raw(base.clone())),
                })
            } else {
                Err(ResError::new_err(
                    ResErrorKind::ExpectedIdentifier,
                    self.tokens[self.current].position,
                ))
            }
        } else {
            if let TokenType::Identifier(base) = &self.tokens[self.current].token_type {
                self.current += 1;
                Ok(TypeSyntax::Raw(base.clone()))
            } else {
                Err(ResError::new_err(
                    ResErrorKind::ExpectedIdentifier,
                    self.tokens[self.current].position,
                ))
            }
        }
    }

    pub fn parse_and(&mut self) -> ResResult<Node> {
        self.debug("parse_and");
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
        self.debug("parse_equality");
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
        self.debug("parse_term");
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
        self.debug("parse_factor");
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
        self.debug("parse_exponent");
        let mut node: Node = self.parse_field_access()?;
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
        self.debug("parse_primary");
        if self.current >= self.tokens.len() {
            return Err(ResError::new_err(
                ResErrorKind::UnexpectedEOF,
                self.tokens.iter().last().unwrap().position,
            ));
        }
        match self.tokens[self.current].token_type.clone() {
            TokenType::LeftParen => self.parse_paren(),
            TokenType::Bang | TokenType::Minus => self.parse_unary(),
            TokenType::Reference | TokenType::Star => self.parse_address(),
            TokenType::LeftBrace => Ok(Node::Block(self.parse_block()?)),
            TokenType::If => {
                let pos1 = self.tokens[self.current].position.clone();
                self.current += 1;
                let condition = self.parse_assignment()?;
                let statement = self.parse_block()?;
                let else_stmnt = if self.current < self.tokens.len() {
                    match self.tokens[self.current].token_type {
                        TokenType::Else => {
                            self.current += 1;
                            Some(self.parse_block()?)
                        }
                        _ => None,
                    }
                } else {
                    None
                };
                let pos2 = self.tokens[self.current].position.clone();

                Ok(Node::If(
                    Box::new(condition),
                    statement,
                    else_stmnt,
                    Position::range(pos1, pos2),
                ))
            }
            TokenType::Loop => {
                self.current += 1;
                let block = self.parse_block()?;
                Ok(Node::Loop(block))
            }

            _ => self.parse_number(),
        }
    }

    fn parse_block(&mut self) -> ResResult<Block> {
        self.debug("parse_block");
        let pos1 = self.tokens[self.current].position.clone();
        let mut statements: Vec<Statement> = Vec::new();
        let mut tail = None;
        self.current += 1;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::RightBrace => {
                    break;
                }
                TokenType::SemiColon => self.current += 1,
                _ => {
                    let stmt = self.parse_statement()?;
                    if let TokenType::RightBrace = self.tokens[self.current].token_type {
                        match stmt {
                            Statement::Expression(node) => tail = Some(Box::new(node)),
                            s => {
                                statements.push(s);
                                self.assert_semicolon()?;
                            }
                        }
                    } else {
                        statements.push(stmt.clone());
                        self.assert_semicolon()?;
                    }
                }
            }
        }
        if self.current >= self.tokens.len() {
            return Err(ResError::new_err(
                ResErrorKind::UnexpectedEOF,
                self.tokens.iter().last().unwrap().position,
            ));
        }
        let pos2 = self.tokens[self.current - 1].position.clone();
        self.current += 1;
        let block = Block {
            stmts: statements,
            tail: tail,
            position: Position::range(pos1, pos2),
        };
        let out = Ok(block);
        out
    }

    fn parse_unary(&mut self) -> ResResult<Node> {
        self.debug("parse_unary");
        let op = self.tokens[self.current].clone();
        self.current += 1;
        let node: Node = self.parse_field_access()?;
        let position = Position::range(op.clone().position, node.position());
        let unary = Node::Unary(op.try_into()?, Box::new(node), position);
        Ok(unary)
    }

    fn parse_address(&mut self) -> ResResult<Node> {
        self.debug("parse_address");
        let op = self.tokens[self.current].clone();
        self.current += 1;
        let node: Node = self.parse_field_access()?;
        let position = Position::range(op.position, node.position());
        match op.token_type {
            TokenType::Reference => Ok(Node::Address(false, Box::new(node), position)),
            TokenType::Star => Ok(Node::Deref(Box::new(node), position)),
            _ => unreachable!(),
        }
    }

    fn parse_field_access(&mut self) -> ResResult<Node> {
        self.debug("parse_field_access");
        let mut node = self.parse_primary()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Dot => {
                    self.current += 1;
                    let (field_name, field_pos) =
                        if let Node::Identifier(i, pos) = self.parse_number()? {
                            (i, pos)
                        } else {
                            return Err(ResError::new_err(
                                ResErrorKind::ExpectedIdentifier,
                                node.position(),
                            ));
                        };

                    let position = Position::range(node.position(), field_pos);
                    node = Node::FieldAccess(Box::new(node), field_name, position);
                }
                _ => break,
            }
        }
        Ok(node)
    }

    fn parse_paren(&mut self) -> ResResult<Node> {
        self.debug("parse_paren");
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
        self.debug("parse_number");
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
                Ok(Node::Litteral(Litteral::Number(number as usize), position))
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
                match self.tokens[self.current].token_type {
                    TokenType::LeftBrace => self.parse_constructor(i, position),
                    TokenType::LeftParen => self.parse_func_identifier(i, position),
                    _ => Ok(Node::Identifier(i.clone(), position)),
                }
            }
            _ => Err(ResError::new_err(
                ResErrorKind::ExpectedExpression,
                position,
            )),
        }
    }

    fn parse_func_identifier(&mut self, i: String, position: Position) -> ResResult<Node> {
        self.debug("parse_func_identifier");
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
    fn parse_constructor(&mut self, i: String, position: Position) -> ResResult<Node> {
        self.debug("parse_constructor");
        self.current += 1;
        let mut idents = Vec::new();
        let mut nodes = Vec::new();
        loop {
            let prev = self.tokens[self.current - 1].token_type.clone();
            let curr = self.tokens[self.current].token_type.clone();
            match (prev, curr) {
                (TokenType::Comma | TokenType::LeftBrace, TokenType::RightBrace) => {
                    self.current += 1;
                    break;
                }
                (TokenType::RightBrace, _) => break,
                (_, TokenType::Identifier(ident)) => {
                    idents.push(ident);
                    self.current += 1;
                    if let TokenType::Colon = self.tokens[self.current].token_type {
                        self.current += 1;
                    } else {
                        return Err(ResError::new_err(
                            ResErrorKind::ExpectedTypeIdentifier,
                            position,
                        ));
                    }
                    let node = self.parse_equality()?;
                    nodes.push(node);
                    self.current += 1;
                }
                _ => {
                    return Err(ResError::new_err(
                        ResErrorKind::ExpectedIdentifier,
                        position,
                    ));
                }
            }
        }
        Ok(Node::Constructor(
            i.clone(),
            idents.into_iter().zip(nodes).collect(),
            position,
        ))
    }
    fn parse_var_identifier(&mut self) -> ResResult<Node> {
        self.debug("parse_var_identifier");
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
                Err(ResError::new_err(
                    ResErrorKind::ExpectedSemicolon(self.tokens[self.current].clone()),
                    self.tokens[self.current - 1].position,
                ))
            }
        } else {
            Ok(())
        }
    }

    fn debug(&self, func: &str) {
        //println!("{}:\ttoken = {}", func, self.tokens[self.current]);
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
            Node::FieldAccess(i, s, _) => write!(f, "_{}.{}", i, s),
            Node::Address(_, n, _) => write!(f, "&{}", n),
            Node::Deref(n, _) => write!(f, "*{}", n),
            Node::FuncIdentifier(i, a, _) => write!(f, "_{}({:?})", i, a),
            Node::Assignment(i, v, _) => write!(f, "{} = {}", i, v),
            Node::Block(block) => {
                writeln!(f, "block: {}\n", block)
            }
            Node::Loop(block) => {
                writeln!(f, "loop: {}\n", block)
            }
            Node::If(condition, then, els, _) => {
                writeln!(f, "if {}", condition)?;
                writeln!(f, "then {}", then)?;
                if let Some(el) = els {
                    writeln!(f, "else {}", el)?;
                }
                Ok(())
            }
            Node::Constructor(ident, args, _) => {
                write!(f, "construct {} {{{:#?}}}", ident, args)
            }
        }
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "block: {{\n")?;
        for stmnt in &self.stmts {
            writeln!(f, "\t{}", stmnt)?;
        }
        if let Some(s) = &self.tail {
            writeln!(f, "\ttail: {}", s)?;
        }
        writeln!(f, "}}\n")
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
            el => write!(f, "{}", el),
        }
    }
}
