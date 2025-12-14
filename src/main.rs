mod compiler;
mod lexer;
mod parser;
mod stack;
mod types;
use anyhow::anyhow;
use clap::{Parser, ValueEnum};
use compiler::Compiler;
use std::{fs::read_to_string, path::PathBuf};
use std::io::Write;

use crate::{
    lexer::{Token, TokenType},
    parser::AstFactory,
};

#[derive(Parser, Debug)]
#[command(version, long_about = None)]
struct CLI {
    #[arg(value_enum)]
    command: Command,

    #[arg()]
    file_path: PathBuf,

    #[arg(short, long, default_value_t = false)]
    debug: bool,

    #[arg(short, long, alias = "o")]
    output_dir: Option<PathBuf>,
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
    #[clap(name = "run", alias = "r")]
    Run,
}

fn main() -> anyhow::Result<()> {
    let args = CLI::parse();

    let file_contents: String = if let Ok(fc) = read_to_string(&args.file_path)
    {
        fc.to_owned()
    } else {
        return Err(anyhow!(
            "Failed to read file {}",
            args.file_path.display()
        ));
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
        }
        Command::Parse => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;

            let mut ast = AstFactory::new(tokens);
            match ast.parse_statements() {
                Ok(h) => println!("{:#?}", h),
                Err(e) => {
                    eprintln!("{}", e);
                }
            };
        }
        Command::Compile => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;

            let mut ast = AstFactory::new(tokens);
            let statements = ast.parse_statements()?;
            let mut compiler = Compiler::new();
            compiler.gen_program(statements)?;
            println!("{}", compiler);
            let content = format!("{}", compiler);
            let output_paths =
                OutputPaths::new(args.file_path, args.output_dir)?;
            std::fs::write(&output_paths.asm_path, content)?;
            output_paths.assemble()?;
            output_paths.link()?;
        }
        Command::Run => {
            let tokens: Vec<Token> = lexer::scan(file_contents)?;

            let mut ast = AstFactory::new(tokens);
            let statements = ast.parse_statements()?;
            let mut compiler = Compiler::new();
            compiler.gen_program(statements)?;
            let content = format!("{}", compiler);
            let output_paths =
                OutputPaths::new(args.file_path, args.output_dir)?;
            std::fs::write(&output_paths.asm_path, content)?;

            output_paths.assemble()?;
            println!("ASSEMBLED");
            output_paths.link()?;
            println!("LINKED");
            output_paths.run()?; 
            println!("EXECUTED");
        }
        _ => todo!(),
    }

    Ok(())
}

struct OutputPaths {
    source_path: PathBuf,
    obj_path: PathBuf,
    asm_path: PathBuf,
    exe_path: PathBuf,
}
impl OutputPaths {
    fn new(
        source_path: PathBuf,
        output_dir: Option<PathBuf>,
    ) -> anyhow::Result<OutputPaths> {
        let directory: PathBuf = if let Some(out_path) = output_dir {
            if out_path.is_dir() {
                out_path
            } else {
                return Err(anyhow!("Output argument must be a directory!"));
            }
        } else {
            std::env::current_dir().unwrap()
        };
        let file_name: String = source_path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_owned();
        let dir_string = directory.to_str().unwrap();
        let file_stem = if let Some(name) = file_name.split_once(".") {
            name.0
        } else {
            ""
        };
        let asm_path = format!("{}/{}.asm", dir_string, file_stem);
        let obj_path = format!("{}/{}.o", dir_string, file_stem);
        let exe_path = format!("{}/{}", dir_string, file_stem);

        Ok(Self {
            source_path,
            asm_path: PathBuf::from(asm_path),
            obj_path: PathBuf::from(obj_path),
            exe_path: PathBuf::from(exe_path),
        })
    }
    fn assemble(&self) -> anyhow::Result<()> {
        let result = std::process::Command::new("nasm")
            .arg("-felf64")
            .arg(&self.asm_path)
            .arg("-o")
            .arg(&self.obj_path)
            .output();
        if let Err(e) = result {
            return Err(anyhow!("NASM ERROR: {}", e));
        }
        Ok(())
    }
    fn link(&self) -> anyhow::Result<()> {
        let result = std::process::Command::new("ld")
            .arg(&self.obj_path)
            .arg("-o")
            .arg(&self.exe_path)
            .output();
        if let Err(e) = result {
            return Err(anyhow!("LD ERROR: {}", e));
        }
        Ok(())
    }
    fn run(&self) -> anyhow::Result<()> {
        println!("EXE PATH: {:?}", self.exe_path);
        let result = std::process::Command::new(&self.exe_path).output()?;
        std::io::stdout().write_all(&result.stdout)?;
        std::io::stderr().write_all(&result.stderr)?;
        
        println!("{}", result.status);

        Ok(())
    }
}
