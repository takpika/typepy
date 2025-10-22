# TypePy VS Code Extension

VS Code extension that adds basic editor support for the TypePy programming language.

## Features

- Syntax highlighting that understands TypePy keywords, built-in types, decorators, function calls, and variable bindings
- Context-aware auto-completion for keywords, built-in and user-defined types, functions, variables, and enum variants (including shorthand `.variant` suggestions, with type signatures/details in the suggestion list)
- Hover tooltips that reveal the latest analyzer-inferred types for functions, variables, enums, and their variants
- Analyzer integration that surfaces parse/analyzer errors inside VS Code
- Command palette action `TypePy: Analyze Current File`
- Optional on-save analysis (enabled by default) with diagnostics in the editor
- Comment toggling with `#` and auto-closing pairs for brackets, braces, parentheses, and triple-quoted strings

## Getting Started

1. Open this repository in VS Code.
2. Build the TypePy compiler once: `cargo build` (this produces `target/debug/typepy` which the extension uses by default).
3. Run the `Extensions: Install from VSIX...` command.
4. Select the `.vsix` file generated from this folder (see the Development section for packaging).
5. Open a `.tpy` file to activate the extension.

The analyzer runs automatically when you save a `.tpy` file. Trigger it manually at any time via the command palette (`TypePy: Analyze Current File`).
Each run also collects symbol/type metadata (via the compiler’s `--emit-symbols` flag), enabling completions that reflect the latest function signatures and enum variants in your project.

### Configuration

The following settings are available under the `TypePy` section:

- `typepy.analyzer.runOnSave` (default: `true`) — toggle automatic analysis on save.
- `typepy.analyzer.runOnChange` (default: `false`) — enable debounced analysis while typing for near real-time diagnostics and hover data.
- `typepy.analyzer.changeDebounceMs` (default: `700`) — adjust the delay applied when `runOnChange` is enabled.
- `typepy.analyzer.command` — override the executable used for analysis (defaults to the locally built `target/debug/typepy`, or falls back to `cargo run`).
- `typepy.analyzer.args` — additional arguments appended before the analyzed file path (for example `["--no-std"]`).

## Development

To build a `.vsix` package locally:

```bash
cd vscode-typepy
npm install
npx vsce package
```

This produces a distributable `typepy-vscode-*.vsix` file that you can install in VS Code.

During development you can use `F5` in VS Code to launch a new Extension Development Host and preview the colorization.
