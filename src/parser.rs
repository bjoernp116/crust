use std::{collections::VecDeque, fmt::Display};

use crate::{
    lexer::Position,
    lexer::{Token, TokenType},
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
    Block(Vec<Statement>),
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
            Statement::VarDecl(i, t, e) => {
                write!(f, "decl: {} = {} (type = {:?})", i, e, t)?
            }
            Statement::Block(block) => {
                writeln!(f, "block: {{\n")?;
                for stmnt in block {
                    writeln!(f, "\t{}", stmnt)?;
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

#[derive(Clone)]
pub enum UnaryOperator {
    Not,
    Neg,
}

impl Display for UnaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Not => write!(f, "!"),
            Self::Neg => write!(f, "-"),
        }
    }
}

#[derive(Clone)]
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
    pub fn parse_statements(&mut self) -> anyhow::Result<Vec<Statement>> {
        let mut out: Vec<Statement> = Vec::new();
        while self.current < self.tokens.len() {
            let node = match self.parse_statement() {
                Ok(expr) => expr,
                Err(e) => {
                    eprintln!("{}", e);
                    std::process::exit(65);
                }
            };
            out.push(node);
        }
        Ok(out)
    }
    pub fn parse_statement(&mut self) -> anyhow::Result<Statement> {
        let out = match self.tokens[self.current].token_type {
            TokenType::Return => {
                self.current += 1;
                if let TokenType::SemiColon =
                    self.tokens[self.current].token_type
                {
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
                    let variable_type = if let TokenType::Colon =
                        self.tokens[self.current].token_type
                    {
                        self.current += 2;
                        Some(self.tokens[self.current - 1].raw.clone())
                    } else {
                        None
                    };
                    match self.tokens[self.current].token_type {
                        TokenType::SemiColon => {
                            let pos =
                                self.tokens[self.current].position.clone();
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
                            eprintln!(
                                "Expected = or ; after variable declearation!"
                            );
                            std::process::exit(70);
                        }
                    }
                } else {
                    Err(anyhow!("Expected identifier got {}", identifier))
                }
            }
            TokenType::LeftBrace => {
                let mut statements: Vec<Statement> = Vec::new();
                self.current += 1;
                while self.current < self.tokens.len() {
                    match self.tokens[self.current].token_type {
                        TokenType::RightBrace => {
                            self.current += 1;
                            break;
                        }
                        TokenType::SemiColon => self.current += 1,
                        _ => statements.push(self.parse_statement()?),
                    }
                    if self.current == self.tokens.len() {
                        eprintln!(
                            "[line {}] Error at end: Expect '}}'.",
                            self.tokens[self.current - 1].position.line()
                        );
                        std::process::exit(65);
                    }
                }
                Ok(Statement::Block(statements))
            }
            TokenType::If => {
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
                        return Ok(Statement::For(
                            None,
                            None,
                            Some(increment),
                            body,
                        ));
                    } else {
                        let condition = self.parse_assignment()?;
                        self.current += 1;
                        if self.is(TokenType::RightParen) {
                            self.current += 1;
                            let body = Box::new(self.parse_statement()?);
                            return Ok(Statement::For(
                                None,
                                Some(condition),
                                None,
                                body,
                            ));
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
                println!("body");
                let body = Box::new(self.parse_statement()?);

                Ok(Statement::For(None, None, None, body))
            }
            TokenType::Fun => {
                self.current += 1;
                let identifier_token = self.parse_var_identifier()?;
                self.current += 1;
                if let Node::Identifier(ident, _) = identifier_token {
                    match self.tokens[self.current].token_type {
                        TokenType::LeftParen => {}
                        _ => {
                            return Err(anyhow!(
                                "Expected initializer list after function defenition!"
                            ));
                        }
                    }
                    self.current += 1;
                    let mut args = Vec::new();
                    loop {
                        let curr = self.tokens[self.current].token_type.clone();
                        let next =
                            self.tokens[self.current + 1].token_type.clone();
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
                    if let TokenType::Arrow =
                        self.tokens[self.current].token_type
                    {
                        self.current += 1;
                        if let Node::Identifier(return_type, _) =
                            self.parse_number()?
                        {
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
                    Err(anyhow!("Expected identifier in function defenition!"))
                }
            }
            _ => {
                let value = self.parse_assignment()?;
                Ok(Statement::Expression(value))
            }
        };

        match self.tokens.get(self.current) {
            Some(Token {
                position: _,
                raw: _,
                token_type: TokenType::SemiColon,
            }) => {
                self.current += 1;
            }
            _ => (),
        }

        out
    }
    fn parse_named_argument(&mut self) -> anyhow::Result<(String, String)> {
        if let Node::Identifier(identifier, _) = self.parse_number()? {
            if let TokenType::Colon = self.tokens[self.current].token_type {
                self.current += 1;
                if let Node::Identifier(typename, _) = self.parse_number()? {
                    if let TokenType::Comma =
                        self.tokens[self.current].token_type
                    {
                        self.current += 1;
                    };
                    Ok((identifier, typename))
                } else {
                    Err(anyhow!("Expected typename after colon!"))
                }
            } else {
                Err(anyhow!("Expected colon after argument!"))
            }
        } else {
            Err(anyhow!("Expected identifier in initializer list!"))
        }
    }
    fn parse_assignment(&mut self) -> anyhow::Result<Node> {
        let identifier: Node = self.parse_or()?;
        while self.current < self.tokens.len() {
            match self.tokens[self.current].token_type {
                TokenType::Equal => {
                    if let Node::Identifier(name, pos) = identifier {
                        self.current += 1;
                        let value = self.parse_assignment()?;
                        let position = Position::range(pos, value.position());
                        let node =
                            Node::Assignment(name, Box::new(value), position);
                        return Ok(node);
                    }
                }
                _ => break,
            }
        }
        Ok(identifier)
    }

    pub fn parse_or(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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

    pub fn parse_and(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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
    pub fn parse_equality(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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

    fn parse_term(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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

    fn parse_factor(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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

    fn parse_exponent(&mut self) -> anyhow::Result<Node> {
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
                    let position =
                        Position::range(node.position(), right.position());
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

    fn parse_primary(&mut self) -> anyhow::Result<Node> {
        if self.current >= self.tokens.len() {
            return Err(anyhow!("Out of bounds access in parse_primary"));
        }
        match self.tokens[self.current].token_type.clone() {
            TokenType::LeftParen => self.parse_paren(),
            TokenType::Bang | TokenType::Minus => self.parse_unary(),
            _ => self.parse_number(),
        }
    }

    fn parse_unary(&mut self) -> anyhow::Result<Node> {
        let op = self.tokens[self.current].clone();
        self.current += 1;
        let node: Node = self.parse_primary()?;
        let position = Position::range(op.clone().position, node.position());
        let unary = Node::Unary(op.try_into()?, Box::new(node), position);
        Ok(unary)
    }

    fn parse_paren(&mut self) -> anyhow::Result<Node> {
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
            return Err(anyhow!("Unescaped parenthesis"));
        }
        let mut parser = AstFactory {
            tokens: private_tokens,
            current: 0,
        };
        let node = parser.parse_assignment()?;
        Ok(Node::Parenthesis(Box::new(node)))
    }

    fn parse_number(&mut self) -> anyhow::Result<Node> {
        if self.current >= self.tokens.len() {
            return Err(anyhow!("Out of bounds access in parse_number"));
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
                if let TokenType::LeftParen =
                    self.tokens[self.current].token_type
                {
                    self.parse_func_identifier(i, position)
                } else {
                    Ok(Node::Identifier(i.clone(), position))
                }
            }
            _ => Err(anyhow!(
                "[line {}] Error at '{}': Expect expression.",
                &self.tokens[self.current].position.line(),
                &self.tokens[self.current].raw
            )),
        }
    }

    fn parse_func_identifier(
        &mut self,
        i: String,
        position: Position,
    ) -> anyhow::Result<Node> {
        self.current += 1;
        let mut args = Vec::new();
        loop {
            let prev = self.tokens[self.current - 1].token_type.clone();
            let curr = self.tokens[self.current].token_type.clone();
            println!("{:?}, {:?}", prev, curr);
            match (prev, curr) {
                (_, TokenType::RightParen) => {
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
    fn parse_var_identifier(&mut self) -> anyhow::Result<Node> {
        let position = self.tokens[self.current].position.clone();
        if let TokenType::Identifier(i) =
            self.tokens[self.current].token_type.clone()
        {
            Ok(Node::Identifier(i, position))
        } else {
            Err(anyhow!("Expected expression"))
        }
    }
}

impl TryFrom<Token> for BinaryOperator {
    type Error = anyhow::Error;
    fn try_from(token: Token) -> anyhow::Result<BinaryOperator> {
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
            _ => Err(anyhow!("Cant convert Token {} to operator", token)),
        }
    }
}

impl TryFrom<Token> for UnaryOperator {
    type Error = anyhow::Error;
    fn try_from(token: Token) -> anyhow::Result<UnaryOperator> {
        match token.token_type {
            TokenType::Bang => Ok(UnaryOperator::Not),
            TokenType::Minus => Ok(UnaryOperator::Neg),
            _ => Err(anyhow!("Cant convert Token {} to operator", token)),
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
