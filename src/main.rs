mod analyzer;
mod keyword_map;
mod lexer;
mod optimizer;
mod parser;
mod token;
mod transpiler;

use core::panic;
use std::env;
use std::fs;
use std::io;
use std::path::Path;

fn main() -> io::Result<()> {
    // コマンドライン引数からファイル名を取得
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} [--no-std] <filename>", args[0]);
        return Ok(());
    }

    let mut use_std = true;
    let mut filenames = Vec::new();
    for arg in args.iter().skip(1) {
        if arg == "--no-std" {
            use_std = false;
        } else {
            filenames.push(arg.clone());
        }
    }

    if filenames.is_empty() {
        eprintln!("Usage: {} [--no-std] <filename>", args[0]);
        return Ok(());
    }

    let filename = &filenames[0];
    // ファイルの内容を読み込む
    let source_code = fs::read_to_string(filename)?;
    // ===============================
    // 1. 字句解析 (Lexer)
    // ===============================
    let mut lexer = lexer::Lexer::new(&source_code);
    let tokens = lexer.tokenize();

    // トークンを一覧表示（デバッグ用）
    println!("--- Tokens ---");
    println!("{:?}", tokens);

    // ===============================
    // 2. 構文解析 (Parser)
    // ===============================
    let mut parser = parser::Parser::new(tokens);
    let parse_result = match parser.parse_file() {
        Ok(ast) => {
            println!("\n--- Parsed AST ---");
            println!("{:#?}", ast);
            // 出力内容をファイルに書き込む
            let output_filename = format!("{}.ast", filename);
            fs::write(output_filename, format!("{:#?}", ast))?;
            Ok::<_, io::Error>(ast)
        }
        Err(e) => {
            eprintln!("Parse Error: {}", e);
            panic!();
        }
    }?;

    // ===============================
    // 3. 意味解析 (Analyzer)
    // ===============================
    let optimized_ast = optimizer::optimize_ast(parse_result);

    let mut analyzer = analyzer::Analyzer::new(optimized_ast);
    if !use_std {
        analyzer.set_use_std(false);
    }
    match analyzer.analyze() {
        Ok(_) => {
            println!("Analysis OK");
            let python_code = transpiler::transpile_to_python(&analyzer.root_node);
            let output_py = Path::new(filename).with_extension("py");
            fs::write(&output_py, python_code)?;
            println!("Transpiled Python written to {}", output_py.display());
        }
        Err(e) => {
            eprintln!("Analyze Error: {}", e);
            panic!();
        }
    }

    Ok(())
}
