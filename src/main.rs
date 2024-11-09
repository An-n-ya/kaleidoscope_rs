use std::{fs::File, io::Read, path::PathBuf};

use ast::Visitor;
use ast_dumper::llvm_module_to_string;
use clap::Parser;
use codegen::CodeGen;
use miette::IntoDiagnostic;
use token::tokenize;

mod ast;
mod ast_dumper;
mod codegen;
mod jit;
mod parser;
mod token;
mod utils;

#[derive(Debug, clap::Parser)]
struct Args {
    file: PathBuf,

    #[arg(short = 'l', long)]
    lexer: bool,

    #[arg(short = 'p', long)]
    parser: bool,

    #[arg(short = 'c', long, default_value = "true")]
    codegen: bool,
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    let mut file = File::open(args.file).into_diagnostic()?;
    let mut buf = "".to_string();
    file.read_to_string(&mut buf).into_diagnostic()?;
    if args.lexer {
        let (remain, tokens) = tokenize(&buf).expect("cannot tokenize");
        assert!(
            remain.is_empty(),
            "tokenize cannot finish. Remaining: {remain}"
        );
        let mut res = vec![];
        for t in tokens {
            res.push(t.kind.clone());
        }
        println!("{res:?}")
    } else if args.parser {
        let parser = parser::Parser {};
        let res = parser.parse(&buf)?;
        let dumped_stmts: Vec<_> = res.iter().map(|stmt| stmt.dump()).collect();
        for s in dumped_stmts {
            println!("{s}")
        }
    } else {
        let parser = parser::Parser {};
        let res = parser.parse(&buf)?;
        let mut code_gen = CodeGen::new();
        code_gen.compile(&res);
        let s = llvm_module_to_string(code_gen.module);
        println!("{s}")
    }
    Ok(())
}
