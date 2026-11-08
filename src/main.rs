mod optimization;
mod config;
mod asm;
mod structs;
mod error;
mod functions;
mod lexer;
mod parser;
mod ssa;
mod symbols;
mod types;
use anyhow::anyhow;
use clap::{Parser, ValueEnum};
//use compiler::Compiler;
use std::io::Write;
use std::println;
use std::{fs::read_to_string, path::PathBuf};

use crate::asm::AsmWriter;
use crate::config::get_config;
use crate::error::{print_err, unwrap_any, unwrap_print};
use crate::optimization::FunctionAnalysis;
use crate::ssa::{SSA, ValueID};
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
    file_path: Option<PathBuf>,

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
    #[clap(name = "analyze", alias = "a")]
    Analyze,
    #[clap(name = "compile", alias = "c")]
    Compile,
    #[clap(name = "run", alias = "r")]
    Run,
}

fn main() -> anyhow::Result<()> {
    let args = CLI::parse();
    let mut config = config::get_config();
    if let Some(file) = args.file_path {
        let path_str = file.to_str().unwrap().to_owned();
        if !config.include.contains(&path_str) {
            config.include.push(file.to_str().unwrap().to_owned());
        }
    }
    
    let file_contents = config.include.into_iter().map(|path| {
        read_to_string(path).unwrap().clone()
    }).collect::<Vec<String>>().join("\n\n");

    println!("{}", file_contents);

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
            let ssa = unwrap_any(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            println!("{:#?}", tables.type_handler);
            println!("{:#?}", ssa);
        }
        Command::Analyze => {
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
            let mut ssa = unwrap_any(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            println!("{}", ssa.functions.len());
            let analysis = FunctionAnalysis::analyze(&ssa.functions[12], &tables);
            println!("{:#?}", analysis);
            for function in &mut ssa.functions {
                let mut analysis = FunctionAnalysis::analyze(&function, &tables);
                for _ in 0..10 {
                    //analysis.optimize(function, &tables);
                }
                analysis.allocate(function);
            }
            //println!("{:#?}", ssa.functions[0]);
            //println!("{:#?}", analysis.value_info[&ValueID(60)]);
            std::process::exit(0);
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
            let mut ssa = unwrap_any(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            for function in &mut ssa.functions {
                let mut analysis = FunctionAnalysis::analyze(&function, &tables);
                for _ in 0..10 {
                    analysis.optimize(function, &tables);
                }
                analysis.allocate(function);
            }
            let mut writer = AsmWriter::new(tables);
            writer.generate_funcs(ssa.functions);
            println!("{}", writer.buffer());

            let output_paths = OutputPaths::new(config.name, args.output_dir)?;
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
            let mut ssa = unwrap_any(SSA::new(typed_ast, &mut tables, ), &tables.file_contents, &tables.type_handler);
            for function in &mut ssa.functions {
                let mut analysis = FunctionAnalysis::analyze(&function, &tables);
                for _ in 0..10 {
                    analysis.optimize(function, &tables);
                }
                analysis.allocate(function);
            }
            let mut writer = AsmWriter::new(tables);
            writer.generate_funcs(ssa.functions);

            let output_paths = OutputPaths::new(config.name, args.output_dir)?;
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
    obj_path: PathBuf,
    asm_path: PathBuf,
    exe_path: PathBuf,
}
impl OutputPaths {
    fn new(name: String, output_dir: Option<PathBuf>) -> anyhow::Result<OutputPaths> {
        let directory: PathBuf = if let Some(out_path) = output_dir {
            if out_path.is_dir() {
                out_path
            } else {
                return Err(anyhow!("Output argument must be a directory!"));
            }
        } else {
            std::env::current_dir().unwrap()
        }; 
        let dir_string = directory.to_str().unwrap();
        let asm_path = format!("{}/{}.s", dir_string, name);
        let obj_path = format!("{}/{}.o", dir_string, name);
        let exe_path = format!("{}/{}", dir_string, name);

        Ok(Self {
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
