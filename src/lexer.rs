use std::fmt::Display;

#[derive(Clone)]
pub struct Position {
    pub from: (usize, usize),
    pub to: (usize, usize)
}

impl Position {
    pub fn new(line1: usize, col1: usize, line2: usize, col2: usize) -> Position {
        Self {
            from: (line1, col1),
            to: (line2, col2)
        }
    }
    pub fn range(from: Self, to: Self) -> Self {
        Self {
            from: from.from,
            to: to.to
        }
    }
    pub fn line(&self) -> usize {
        self.to.0
    }
}

#[derive(Clone, Debug, PartialEq)]
#[allow(unused)]
pub enum TokenType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Star,
    Dot,
    Comma,
    Plus,
    Minus,
    Slash,
    SemiColon,
    Equal,
    Bang,
    Greater,
    Less,
    Carrot,

    If,
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    Nil,
    Or,
    Return,
    This,
    True,
    Let,
    While,
    Exit,

    EqualEqual,
    BangEqual,
    LessEqual,
    GreaterEqual,

    Number(f64),
    StringLitteral(String),
    Identifier(String),
    Invalid(String),
}
#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub raw: String,
    pub position: Position,
}

pub fn scan(str: String) -> anyhow::Result<Vec<Token>> {
    let mut out = Vec::new();
    let mut buffer: String = String::new();
    let mut line_number = 1;
    let mut new_line = 0;
    let stream: Vec<char> = str.chars().collect();
    let mut i = 0;
    while i < stream.len() {
        match stream[i] {
            ' ' | '\t' => {},
            '\n' => {
                line_number += 1;
                new_line = i;
            },
            x if x.is_numeric() => {
                let mut float = false;
                let first_line = line_number;
                let first_col = i - new_line;
                loop {
                    if i != stream.len() && stream[i] == '.' && !float {
                        float = true;
                        buffer.push('.');
                        i+=1;
                        continue;
                    }
                    if i == stream.len() || !stream[i].is_numeric() {

                        let number = buffer.clone().parse::<f64>()?;
                        let token = Token {
                            token_type: TokenType::Number(number),
                            raw: buffer.clone(),
                            position: Position::new(first_line, first_col, line_number, i - new_line),
                        };
                        buffer.clear();
                        i -= 1;
                        out.push(token);
                        break;
                    }
                    buffer.push(stream[i]);
                    i += 1;
                }
            }
            '"' => {
                i += 1;
                let first_line = line_number;
                let first_col = i - new_line;
                loop {
                    if i == stream.len() {
                        let token = Token {
                            token_type: TokenType::Invalid(format!("Unterminated string.")),
                            raw: buffer.clone(),
                            position: Position::new(
                                first_line, 
                                first_col, 
                                line_number, 
                                i - new_line
                            ),
                        };
                        out.push(token);
                        break;
                    }
                    if stream[i] == '"' {
                        let token = Token {
                            token_type: TokenType::StringLitteral(buffer.clone()),
                            raw: format!("\"{}\"", buffer.clone()),
                            position: Position::new(
                                first_line, 
                                first_col, 
                                line_number, 
                                i - new_line
                            ),
                        };
                        buffer.clear();
                        out.push(token);
                        break;
                    }
                    buffer.push(stream[i]);
                    if stream[i] == '\n' {
                        line_number += 1;
                        new_line = i;
                    }
                    i += 1;
                }
            }
            '=' | '!' | '<' | '>' if i+1 < stream.len() && stream[i+1] == '=' => {
                let token_type = match stream[i] {
                    '=' => TokenType::EqualEqual,
                    '!' => TokenType::BangEqual,
                    '>' => TokenType::GreaterEqual,
                    '<' => TokenType::LessEqual,
                    _ => unreachable!()
                };
                let token = Token {
                    token_type,
                    raw: format!("{}{}", stream[i], stream[i+1]),
                    position: Position::new(line_number, i, line_number, i+1),
                };
                out.push(token);
                i += 1;
            }
            '/' if i+1 < stream.len() && stream[i+1] == '/' => {
                loop {
                    if i == stream.len() || stream[i] == '\n' {
                        line_number += 1;
                        break;
                    }
                    i += 1;
                }
            }
            c if c.is_alphabetic() || c == '_' => {
                let first_col = i - new_line;
                loop {
                    if i == stream.len()
                    || !(stream[i].is_alphanumeric() || stream[i] == '_') {
                        let token = Token {
                            token_type: TokenType::from(buffer.clone()),
                            raw: buffer.clone(),
                            position: Position::new(line_number, first_col, line_number, i - new_line)
                        };
                        buffer.clear();
                        out.push(token);
                        i -= 1;
                        break;
                    }
                    buffer.push(stream[i]);
                    i += 1;
                }
            }
            _ => {
                let token = Token {
                    token_type: TokenType::from(stream[i]),
                    raw: format!("{}", stream[i]),
                    position: Position::new(line_number, i - new_line, line_number, i - new_line)
                };
                out.push(token);
            }
        }
        i += 1;
    }
    Ok(out)
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenType::*;
        let str: &str = match self.token_type.clone() {
            Invalid(err) => {
                eprint!("[line {}]: Error: {}", self.position.line(), err);
                return Ok(());
            },
            LeftParen => "LEFT_PAREN",
            RightParen => "RIGHT_PAREN",
            LeftBrace => "LEFT_BRACE",
            RightBrace => "RIGHT_BRACE",
            Star => "STAR",
            Dot => "DOT",
            Comma => "COMMA",
            Plus => "PLUS",
            Minus => "MINUS",
            Slash => "SLASH",
            SemiColon => "SEMICOLON",
            Equal => "EQUAL",
            Bang => "BANG",
            Greater => "GREATER",
            Less => "LESS",
            Carrot => "CARROT",

            If => "IF",
            And => "AND",
            Class => "CLASS",
            Else => "ELSE",
            False => "FALSE",
            For => "FOR",
            Fun => "FUN",
            Nil => "NIL",
            Or => "OR",
            Exit => "EXIT",
            Return => "RETURN",
            This => "THIS",
            True => "TRUE",
            Var => "VAR",
            While => "WHILE",

            EqualEqual => "EQUAL_EQUAL",
            BangEqual => "BANG_EQUAL",
            LessEqual => "LESS_EQUAL",
            GreaterEqual => "GREATER_EQUAL",

            Number(_) => "NUMBER",
            StringLitteral(_) => "STRING",
            Identifier(_) => "IDENTIFIER"
        };
        write!(f, "{} {}", str, self.raw)?;
        Ok(())
    }
}

impl From<char> for TokenType {
    fn from(value: char) -> Self {
        use TokenType::*;
        match value {
            '(' => LeftParen,
            ')' => RightParen,
            '{' => LeftBrace,
            '}' => RightBrace,
            '*' => Star,
            '.' => Dot,
            ',' => Comma,
            '+' => Plus,
            '-' => Minus,
            '/' => Slash,
            ';' => SemiColon,
            '=' => Equal,
            '!' => Bang,
            '>' => Greater,
            '<' => Less,
            '^' => Carrot,
            c => Invalid(format!("Unexpected character: {}", c)),
        }
    }
}

impl From<String> for TokenType {
    fn from(value: String) -> Self {
        use TokenType::*;
        match value.as_str() {
            "if" => If,
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "nil" => Nil,
            "or" => Or,
            "exit" => Exit,
            "return" => Return,
            "this" => This,
            "true" => True,
            "let" => Let,
            "while" => While,
            _ => Identifier(value)
        }
    }
}

impl Token {
    pub fn is_valid(&self) -> bool {
        match self.token_type {
            TokenType::Invalid(_) => false,
            _ => true
        }
    }
}
