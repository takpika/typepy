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

## VS Code 拡張機能

`vscode-typepy` ディレクトリには TypePy 用の VS Code 拡張機能が含まれています。
次の手順でビルド・インストールできます。

1. `cargo build` で TypePy バイナリをビルド（`target/debug/typepy` を生成）
2. `cd vscode-typepy`
3. `npm install`
4. `npx vsce package` で `.vsix` を生成
5. VS Code の「Extensions: Install from VSIX...」コマンドでインストール

拡張機能は `.tpy` ファイル保存時にコンパイラの字句解析〜意味解析（Analyzer）までを実行し、結果をエディタ上にダイアグノスティックとして表示します。
コマンドパレットの `TypePy: Analyze Current File` から手動実行も可能で、次のようなエディタ支援機能を提供します。

- 解析結果で報告されたエラーのダイアグノスティック表示
- キーワード／組み込み型に加え、プロジェクト内の関数・変数・列挙体を型情報付きで提示する自動補完
- `EnumName.` と入力した際の列挙値候補のサジェスト
- 関数や変数、列挙体にマウスを合わせると Analyzer が推定した型情報をツールチップで表示

設定の `typepy.analyzer.runOnChange` を `true` にすると、編集中（デフォルト 700ms デバウンス）にも解析が走り、ホバー／補完にほぼリアルタイムの結果が反映されます。

### CLI オプション

コマンドラインから解析のみを行いたい場合は `--check` フラグを利用してください。

```bash
cargo run -- --check example/control_flow.tpy
```

解析に成功すると `Analysis succeeded.` が表示され、失敗した場合は該当箇所の行・列情報付きでエラーが表示されます。
`--emit-symbols` フラグを併用すると、解析結果から得られた関数／変数／型の情報を JSON 形式で標準出力へ出力できます（VS Code 拡張機能はこの情報を自動的に利用します）。

```bash
cargo run -- --check --emit-symbols example/control_flow.tpy
```
