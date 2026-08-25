mod asm;
mod structs;
mod error;
mod functions;
mod locations;
mod lexer;
mod parser;
mod ssa;
mod symbols;
mod types;
use anyhow::anyhow;
use clap::{Parser, ValueEnum};
//use compiler::Compiler;
use std::io::Write;
use std::{fs::read_to_string, path::PathBuf};

use crate::asm::AsmWriter;
use crate::error::{print_err, unwrap_print};
use crate::ssa::SSA;
use crate::types::{TypeHandler, Typer};
//use crate::chunks::IR;
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

    #[arg(alias = "op", long, default_value_t = false)]
    optimize: bool,

    #[arg(short, long, alias = "od")]
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
    #[clap(name = "resolve", alias = "res")]
    Resolve,
    #[clap(name = "ssa")]
    SSA,
    #[clap(name = "compile", alias = "c")]
    Compile,
    #[clap(name = "run", alias = "r")]
    Run,
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
        }
        Command::Parse => {
            let tokens: Vec<Token> = lexer::scan(file_contents.clone())?;

            let mut ast = AstFactory::new(tokens);
            match ast.parse_statements() {
                Ok(h) => println!("{:#?}", h),
                Err(e) => {
                    print_err(e, &file_contents, &TypeHandler::new());
                }
            };
        }
        Command::Resolve => {
            let tokens: Vec<Token> = lexer::scan(file_contents.clone())?;

            let mut ast = AstFactory::new(tokens);
            let mut type_handler = TypeHandler::new();
            let statements = unwrap_print(ast.parse_statements(), &file_contents, &type_handler);

            let mut typer = Typer::new(&mut type_handler);
            let mut typed_ast = unwrap_print(
                typer.resolve_stmts(statements, &mut type_handler, &file_contents),
                &file_contents,
                &type_handler,
            );
            if args.optimize {
                typer.optimize_stmts(&mut typed_ast);
            }
            println!("{:#?}", typed_ast);
            println!("{:#?}", typer);
            println!("{:#?}", type_handler);
        }
        Command::SSA => {
            let tokens: Vec<Token> = lexer::scan(file_contents.clone())?;

            let mut ast = AstFactory::new(tokens);
            let mut type_handler = TypeHandler::new();
            let statements = unwrap_print(ast.parse_statements(), &file_contents, &type_handler);

            let mut typer = Typer::new(&mut type_handler);
            let mut typed_ast = unwrap_print(
                typer.resolve_stmts(statements, &mut type_handler, &file_contents),
                &file_contents,
                &type_handler,
            );
            if args.optimize {
                typer.optimize_stmts(&mut typed_ast);
            }

            let mut tables = typer.tables(type_handler, file_contents);
            let ssa = unwrap_print(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            println!("{:#?}", tables.type_handler);
            println!("{:#?}", ssa);
        }
        Command::Compile => {
            let tokens: Vec<Token> = lexer::scan(file_contents.clone())?;

            let mut ast = AstFactory::new(tokens);
            let mut type_handler = TypeHandler::new();
            let statements = unwrap_print(ast.parse_statements(), &file_contents, &type_handler);

            let mut typer = Typer::new(&mut type_handler);
            let mut typed_ast = unwrap_print(
                typer.resolve_stmts(statements, &mut type_handler, &file_contents),
                &file_contents,
                &type_handler,
            );
            if args.optimize {
                typer.optimize_stmts(&mut typed_ast);
            }

            let mut tables = typer.tables(type_handler, file_contents);
            let ssa = unwrap_print(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            let mut writer = AsmWriter::new(tables);
            writer.generate_funcs(ssa.functions);
            println!("{}", writer.buffer());

            let output_paths = OutputPaths::new(args.file_path, args.output_dir)?;
            println!("{:#?}", output_paths);
            std::fs::write(&output_paths.asm_path, writer.buffer())?;
        }
        Command::Run => {
            let tokens: Vec<Token> = lexer::scan(file_contents.clone())?;

            let mut ast = AstFactory::new(tokens);
            let mut type_handler = TypeHandler::new();
            let statements = unwrap_print(ast.parse_statements(), &file_contents, &type_handler);

            let mut typer = Typer::new(&mut type_handler);
            let mut typed_ast = unwrap_print(
                typer.resolve_stmts(statements, &mut type_handler, &file_contents),
                &file_contents,
                &type_handler,
            );
            if args.optimize {
                typer.optimize_stmts(&mut typed_ast);
            }

            let mut tables = typer.tables(type_handler, file_contents);
            let ssa = unwrap_print(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            let mut writer = AsmWriter::new(tables);
            writer.generate_funcs(ssa.functions);

            let output_paths = OutputPaths::new(args.file_path, args.output_dir)?;
            println!("{:#?}", output_paths);
            output_paths.clean()?;
            std::fs::write(&output_paths.asm_path, writer.buffer())?;
            output_paths.assemble()?;
            output_paths.run()?;
        }
        _ => todo!(),
    }

    Ok(())
}

#[derive(Debug)]
struct OutputPaths {
    source_path: PathBuf,
    obj_path: PathBuf,
    asm_path: PathBuf,
    exe_path: PathBuf,
}
impl OutputPaths {
    fn new(source_path: PathBuf, output_dir: Option<PathBuf>) -> anyhow::Result<OutputPaths> {
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
        let asm_path = format!("{}/{}.s", dir_string, file_stem);
        let obj_path = format!("{}/{}.o", dir_string, file_stem);
        let exe_path = format!("{}/{}", dir_string, file_stem);

        Ok(Self {
            source_path,
            asm_path: PathBuf::from(asm_path),
            obj_path: PathBuf::from(obj_path),
            exe_path: PathBuf::from(exe_path),
        })
    }
    fn clean(&self) -> anyhow::Result<()> {
        let res = std::process::Command::new("rm")
            .arg(&self.asm_path)
            .arg(&self.obj_path)
            .arg(&self.exe_path)
            .output().unwrap();
        Ok(())
    }
    fn assemble(&self) -> anyhow::Result<()> {
        let result = std::process::Command::new("gcc")
            .arg("-no-pie")
            .arg("-g")
            .arg(&self.asm_path)
            .arg("runtime.s")
            .arg("-o")
            .arg(&self.exe_path)
            .output();
        if let Err(e) = result {
            return Err(anyhow!("GCC ERROR: {}", e));
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
        //println!("EXE PATH: {:?}", self.exe_path);
        let result = std::process::Command::new(&self.exe_path).output()?;
        std::io::stdout().write_all(&result.stdout)?;
        std::io::stderr().write_all(&result.stderr)?;

        println!("{:#?}", result);

        Ok(())
    }
}
