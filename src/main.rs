mod analyzer;
mod bundler;
mod keyword_map;
mod lexer;
mod optimizer;
mod parser;
mod token;
mod transpiler;

use serde_json::json;
use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::process;
use std::sync::Arc;

fn main() -> io::Result<()> {
    // コマンドライン引数からファイル名を取得
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!(
            "Usage: {} [--no-std] [--check] [--emit-symbols] [--source-map] <filename>",
            args[0]
        );
        return Ok(());
    }

    let mut use_std = true;
    let mut check_only = false;
    let mut emit_symbols = false;
    let mut emit_source_map = false;
    let mut filenames = Vec::new();
    for arg in args.iter().skip(1) {
        match arg.as_str() {
            "--no-std" => {
                use_std = false;
            }
            "--check" => {
                check_only = true;
            }
            "--emit-symbols" => {
                emit_symbols = true;
            }
            "--source-map" => {
                emit_source_map = true;
            }
            _ => {
                filenames.push(arg.clone());
            }
        }
    }

    if filenames.is_empty() {
        eprintln!(
            "Usage: {} [--no-std] [--check] [--emit-symbols] [--source-map] <filename>",
            args[0]
        );
        return Ok(());
    }

    let filename = &filenames[0];
    // ファイルの内容を読み込む
    let source_code: Arc<str> = Arc::from(fs::read_to_string(filename)?);
    // ===============================
    // 1. 字句解析 (Lexer)
    // ===============================
    let mut lexer = lexer::Lexer::new(&source_code);
    let tokens = lexer.tokenize();

    // トークンを一覧表示（デバッグ用）
    // println!("--- Tokens ---");
    // println!("{:?}", tokens);

    // ===============================
    // 2. 構文解析 (Parser)
    // ===============================
    let mut parser = parser::Parser::new(source_code.clone(), tokens);
    let parse_result = match parser.parse_file() {
        Ok(ast) => {
            // println!("\n--- Parsed AST ---");
            // println!("{:#?}", ast);
            // 出力内容をファイルに書き込む
            // let output_filename = format!("{}.ast", filename);
            // fs::write(output_filename, format!("{:#?}", ast))?;
            ast
        }
        Err(e) => {
            eprintln!("Parse Error: {}", e);
            process::exit(1);
        }
    };

    // ===============================
    // 3. 意味解析 (Analyzer)
    // ===============================
    let optimized_ast = optimizer::optimize_ast(parse_result);

    let mut analyzer = analyzer::Analyzer::new(optimized_ast, source_code.clone());
    if !use_std {
        analyzer.set_use_std(false);
    }
    match analyzer.analyze() {
        Ok(summary) => {
            if check_only {
                println!("Analysis succeeded.");
            } else {
                let bundle =
                    bundler::bundle_project(Path::new(filename), &analyzer.root_node, use_std)?;
                let output_py = Path::new(filename).with_extension("py");
                fs::write(&output_py, &bundle.code)?;
                println!("Transpiled Python written to {}", output_py.display());

                if emit_source_map {
                    let output_map = output_py.with_extension("py.map");
                    let payload = json!({
                        "version": 1,
                        "file": output_py
                            .file_name()
                            .and_then(|s| s.to_str())
                            .unwrap_or_default(),
                        "source": filename,
                        "mappings": bundle.source_map,
                    });
                    let pretty = serde_json::to_string_pretty(&payload)?;
                    fs::write(&output_map, pretty)?;
                    println!("Source map written to {}", output_map.display());
                }
            }
            if emit_symbols {
                let payload = json!({
                    "kind": "typepy-symbols",
                    "file": filename,
                    "summary": summary,
                });
                println!("{}", payload.to_string());
            }
        }
        Err(e) => {
            let formatted = analyzer.format_error(&e);
            eprintln!("Analyze Error: {}", formatted);
            process::exit(1);
        }
    }

    Ok(())
}
