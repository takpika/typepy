# TypePy
Pythonベースの静的型付けプログラミング言語

## 概要
TypePy は Python ライクな文法をもつ静的型付け言語です。Rust 製の
コンパイラは字句解析・構文解析・簡易的な意味解析を行い、与えられた
ソースを AST として出力します。

## キーワード
`src/keyword_map.rs` で定義されているキーワードは以下の通りです。

```
if else for in while switch case default break continue
class struct enum def let final static private public const
import from as try catch throw finally return guard pass
None true false
```

## 型
組み込み型は `src/analyzer.rs` に定義されている次の 12 種類です。

```
int8  int16  int32  int64
uint8 uint16 uint32 uint64
float32 float64
string bool
```

型の末尾に `?` を付けると `None` を許容するオプショナル型となります。
複数の型を `A | B` のように並べるとユニオン型を表します。関数型は
`function(T1, T2) -> R` の形式で記述します。

## 構文の特徴
- 変数宣言: `let` / `final` / `const`
- クラス・構造体・列挙型: `class` / `struct` / `enum`
- 関数定義: `def name(params) -> type { ... }`
- ラムダ式: `params => expr` または `params { ... }`
- 配列 `[T]`、辞書 `{K: V}`、タプル `(T1, T2)`
- 制御構文: `if` / `switch` / `for` / `while` / `try` / `catch` など
- デコレータ記法 `@decorator`
- `?.` (オプショナルチェーン)、`!` (unwrap)、`??` (null 合体演算子)
- 範囲演算子 `..` (閉区間) と `..<` (半開区間)

## コメント
`#` から行末までがコメントです。`"""` や `'''` による複数行文字列も
使用できます。

## 実行例
`example/example.tpy` に加えて、`example/recursion.tpy` や
`example/control_flow.tpy`、`example/error_handling.tpy`、
`example/lambda_functions.tpy` など多数のサンプルを用意しています。次のコマンドで
字句解析・構文解析結果を確認できます。

```bash
cargo run -- example/example.tpy
```
