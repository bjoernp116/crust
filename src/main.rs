mod parser;
mod stack;
mod lexer;
mod compiler;
use std::{fs::read_to_string, path::PathBuf};
use compiler::Compiler;
use anyhow::anyhow;
use clap::{Parser, ValueEnum};

use crate::{lexer::{Token, TokenType}, parser::AstFactory};

#[derive(Parser, Debug)]
#[command(version, long_about = None)]
struct CLI {
    #[arg(value_enum)]
    command: Command,

    #[arg()]
    file_path: PathBuf,

    #[arg(short, long, default_value_t = false)]
    debug: bool
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Command {
    #[clap(name = "tokenize", alias = "t")]
    Tokenize,
    #[clap(name = "parse", alias = "p")]
    Parse,
    #[clap(name = "evaluate", alias = "e")]
    Evaluate,
    #[clap(name = "compile", alias = "c")]
    Compile,
}

fn main() -> anyhow::Result<()> {
    let args = CLI::parse();

    let file_contents: String = if let Ok(fc) = read_to_string(&args.file_path) {
        fc.to_owned()
    } else {
        return Err(anyhow!("Failed to read file {}", args.file_path.display()));
    };

    match args.command {
        Command::Tokenize => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;
            
            for token in tokens {
                if let TokenType::Invalid(e) = token.token_type {
                    eprintln!("[line {}] Error: {}", token.position.line(), e);
                } else {
                    println!("{}", token);
                }
            }
        },
        Command::Parse => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;

            let mut ast = AstFactory::new(tokens);
            match ast.parse_statements() {
                Ok(h) => println!("{:#?}", h),
                Err(e) => {
                    eprintln!("{}", e);
                }
            };
        },
        Command::Compile => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;

            let mut ast = AstFactory::new(tokens);
            let statements = ast.parse_statements()?;
            let mut compiler = Compiler::new();
            compiler.gen_program(statements)?;
            println!("{}", compiler);
            let content = format!("{}", compiler);
            let file_name: String = args.file_path
                .file_name()
                .unwrap()
                .to_str()
                .unwrap()
                .to_owned();
            let file_name = if let Some(name) = file_name.split_once(".") {
                name.0
            } else {
                ""
            };
            let path = format!("{}.asm", file_name);
            std::fs::write(path, content)?;
        }
        _ => todo!()
    }

    Ok(())
}

