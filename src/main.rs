mod lexer;
mod token;
mod keyword_map;
mod parser;

use std::fs;
use std::io;
use std::env;

fn main() -> io::Result<()> {
    // コマンドライン引数からファイル名を取得
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];

    // ファイルの内容を読み込む
    let source_code = fs::read_to_string(filename)?;
    
    // ===============================
    // 1. 字句解析 (Lexer)
    // ===============================
    let mut lexer = lexer::Lexer::new(&source_code);
    let tokens = lexer.tokenize();

    // トークンを一覧表示（デバッグ用）
    println!("--- Tokens ---");
    for token in &tokens {
        println!("{:?}", token);
    }

    // ===============================
    // 2. 構文解析 (Parser)
    // ===============================
    let mut parser = parser::Parser::new(tokens);
    match parser.parse_file() {
        Ok(ast) => {
            println!("\n--- Parsed AST ---");
            println!("{:#?}", ast);
        }
        Err(e) => {
            eprintln!("Parse Error: {}", e);
        }
    }

    Ok(())
}
