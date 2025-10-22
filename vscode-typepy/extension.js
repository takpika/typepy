"use strict";

const vscode = require("vscode");
const path = require("path");
const fs = require("fs");
const cp = require("child_process");
const crypto = require("crypto");

const KEYWORDS = [
  "if",
  "else",
  "for",
  "in",
  "while",
  "switch",
  "case",
  "default",
  "break",
  "continue",
  "class",
  "struct",
  "enum",
  "def",
  "let",
  "final",
  "static",
  "private",
  "public",
  "const",
  "import",
  "from",
  "as",
  "try",
  "catch",
  "throw",
  "finally",
  "return",
  "guard",
  "pass"
];

const PRIMITIVE_TYPES = [
  "int8",
  "int16",
  "int32",
  "int64",
  "uint8",
  "uint16",
  "uint32",
  "uint64",
  "float32",
  "float64",
  "string",
  "bool"
];

const BUILTIN_CONSTANTS = ["None", "true", "false"];

const BUILTIN_FUNCTIONS = [
  {
    label: "print",
    detail: "Built-in function",
    documentation: "Print values to standard output."
  },
  {
    label: "exit",
    detail: "Built-in function",
    documentation: "Terminate the current program."
  }
];

const MEMBER_SUGGESTIONS = [
  {
    label: "append",
    detail: "Array method",
    documentation: "Append a value to the end of the array."
  },
  {
    label: "removeAt",
    detail: "Array method",
    documentation: "Remove the element at the given index."
  },
  {
    label: "length",
    detail: "Array property",
    documentation: "Number of elements in the array."
  },
  {
    label: "get",
    detail: "Dict method",
    documentation: "Retrieve a value from a dictionary, optionally with defaults."
  }
];

const symbolCache = new Map();
const pendingChangeTimers = new Map();
const runningAnalyses = new Set();
const tempFileRegistry = new Map();

function createKeywordCompletion(word) {
  const item = new vscode.CompletionItem(word, vscode.CompletionItemKind.Keyword);
  item.insertText = word;
  item.detail = "Keyword";
  return item;
}

function createTypeCompletion(typeName) {
  const item = new vscode.CompletionItem(typeName, vscode.CompletionItemKind.TypeParameter);
  item.insertText = typeName;
  item.detail = "Primitive type";
  return item;
}

function createConstantCompletion(name) {
  const item = new vscode.CompletionItem(name, vscode.CompletionItemKind.Constant);
  item.insertText = name;
  item.detail = "Built-in constant";
  return item;
}

function createFunctionCompletion(definition) {
  const item = new vscode.CompletionItem(definition.label, vscode.CompletionItemKind.Function);
  item.insertText = definition.label;
  item.detail = definition.detail;
  if (definition.documentation) {
    item.documentation = new vscode.MarkdownString(definition.documentation);
  }
  return item;
}

function createMemberCompletion(definition) {
  const item = new vscode.CompletionItem(definition.label, vscode.CompletionItemKind.Method);
  item.insertText = definition.label;
  item.detail = definition.detail;
  if (definition.documentation) {
    item.documentation = new vscode.MarkdownString(definition.documentation);
  }
  return item;
}

function createSnippetCompletion(label, snippet, detail, documentation) {
  const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.Snippet);
  item.insertText = new vscode.SnippetString(snippet);
  item.detail = detail;
  if (documentation) {
    item.documentation = new vscode.MarkdownString(documentation);
  }
  item.sortText = `0_${label}`;
  return item;
}

async function ensureTempDirectory(workspaceFolder) {
  const tempDir = path.join(workspaceFolder.uri.fsPath, ".typepy-vscode");
  await fs.promises.mkdir(tempDir, { recursive: true });
  return tempDir;
}

async function ensureTempFileForDocument(document) {
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!workspaceFolder) {
    return null;
  }
  const tempDir = await ensureTempDirectory(workspaceFolder);
  const key = document.uri.toString();
  let cachedPath = tempFileRegistry.get(key);
  if (!cachedPath) {
    const hash = crypto.createHash("sha1").update(key).digest("hex");
    const extension = path.extname(document.fileName) || ".tpy";
    cachedPath = path.join(tempDir, `${hash}${extension}`);
    tempFileRegistry.set(key, cachedPath);
  }
  await fs.promises.writeFile(cachedPath, document.getText(), "utf8");
  return cachedPath;
}

async function removeTempFileForDocument(document) {
  const key = document.uri.toString();
  const cachedPath = tempFileRegistry.get(key);
  if (cachedPath) {
    tempFileRegistry.delete(key);
    try {
      await fs.promises.unlink(cachedPath);
    } catch {
      // ignore removal errors
    }
  }
}

function buildFunctionSignature(fnSummary) {
  const paramsText = fnSummary.params
    .map((param) => `${param.name}: ${param.type_repr}`)
    .join(", ");
  const containerPrefix = fnSummary.container ? `${fnSummary.container}.` : "";
  return `${containerPrefix}${fnSummary.name}(${paramsText}) -> ${fnSummary.return_type}`;
}

function createFunctionCompletionFromSummary(fnSummary) {
  const item = new vscode.CompletionItem(fnSummary.name, vscode.CompletionItemKind.Function);
  const snippet = new vscode.SnippetString();
  snippet.appendText(fnSummary.name);
  snippet.appendText("(");
  fnSummary.params.forEach((param, index) => {
    snippet.appendPlaceholder(param.name);
    if (index < fnSummary.params.length - 1) {
      snippet.appendText(", ");
    }
  });
  snippet.appendText(")");
  item.insertText = snippet;
  item.detail = buildFunctionSignature(fnSummary);
  item.documentation = new vscode.MarkdownString(
    `\`${buildFunctionSignature(fnSummary)}\`\n\nDefined${fnSummary.container ? ` in \`${fnSummary.container}\`` : ""}.`
  );
  item.sortText = `1_${fnSummary.name}`;
  return item;
}

function createVariableCompletionFromSummary(variableSummary) {
  const label = variableSummary.container
    ? `${variableSummary.container}.${variableSummary.name}`
    : variableSummary.name;
  const item = new vscode.CompletionItem(variableSummary.name, vscode.CompletionItemKind.Variable);
  item.insertText = variableSummary.name;
  item.detail = variableSummary.container
    ? `${label}: ${variableSummary.type_repr}`
    : variableSummary.type_repr;
  item.documentation = new vscode.MarkdownString(
    variableSummary.container
      ? `\`${label}: ${variableSummary.type_repr}\`${variableSummary.is_static ? " (static)" : ""}`
      : `\`${variableSummary.name}: ${variableSummary.type_repr}\``
  );
  item.sortText = `2_${label}`;
  return item;
}

function createTypeCompletionFromSummary(typeSummary) {
  if (typeSummary.kind === "enum") {
    const item = new vscode.CompletionItem(typeSummary.name, vscode.CompletionItemKind.Enum);
    item.insertText = typeSummary.name;
    item.detail = "Enum";
    item.documentation = new vscode.MarkdownString(
      typeSummary.variants && typeSummary.variants.length > 0
        ? `Variants: ${typeSummary.variants.join(", ")}`
        : "Enum type"
    );
    item.sortText = `3_${typeSummary.name}`;
    return item;
  }

  const isClass = typeSummary.kind === "class";
  const kind = isClass ? vscode.CompletionItemKind.Class : vscode.CompletionItemKind.Struct;
  const item = new vscode.CompletionItem(typeSummary.name, kind);
  item.insertText = typeSummary.name;
  item.detail = isClass ? "Class" : "Struct";
  if (typeSummary.fields && typeSummary.fields.length > 0) {
    const lines = typeSummary.fields.map((field) => {
      const modifier = field.is_static && isClass ? "static " : "";
      return `- ${modifier}\`${field.name}: ${field.type_repr}\``;
    });
    const doc = new vscode.MarkdownString(lines.join("\n"));
    doc.isTrusted = false;
    item.documentation = doc;
  }
  item.sortText = `3_${typeSummary.name}`;
  return item;
}

function createEnumVariantCompletion(enumName, variant, options = {}) {
  const {
    label = variant,
    insertText = variant,
    filterText = null,
    sortTextPrefix = "1_",
    detail = `${enumName} variant`
  } = options;
  const item = new vscode.CompletionItem(label, vscode.CompletionItemKind.EnumMember);
  item.insertText = insertText;
  item.detail = detail;
  item.sortText = `${sortTextPrefix}${label}`;
  if (filterText) {
    item.filterText = filterText;
  }
  return item;
}

function renderFunctionHover(fnSummary) {
  const md = new vscode.MarkdownString();
  md.appendCodeblock(buildFunctionSignature(fnSummary), "typepy");
  if (fnSummary.params && fnSummary.params.length > 0) {
    const paramsList = fnSummary.params
      .map((param) => `- \`${param.name}: ${param.type_repr}\``)
      .join("\n");
    md.appendMarkdown("\n\n");
    md.appendMarkdown(paramsList);
  }
  md.isTrusted = false;
  return md;
}

function renderVariableHover(variableSummary) {
  const prefix = variableSummary.container ? `${variableSummary.container}.` : "";
  const md = new vscode.MarkdownString();
  md.appendCodeblock(`${prefix}${variableSummary.name}: ${variableSummary.type_repr}`, "typepy");
  md.isTrusted = false;
  return md;
}

function renderTypeHover(typeSummary) {
  const md = new vscode.MarkdownString();
  if (typeSummary.kind === "enum") {
    md.appendMarkdown(`**Enum \`${typeSummary.name}\`**`);
    if (typeSummary.variants && typeSummary.variants.length > 0) {
      const variantsList = typeSummary.variants.map((variant) => `- \`${variant}\``).join("\n");
      md.appendMarkdown("\n\n");
      md.appendMarkdown(variantsList);
    }
    md.isTrusted = false;
    return md;
  }

  const label = typeSummary.kind === "class" ? "Class" : "Struct";
  md.appendMarkdown(`**${label} \`${typeSummary.name}\`**`);
  if (typeSummary.fields && typeSummary.fields.length > 0) {
    const lines = typeSummary.fields
      .map((field) => {
        const modifier = field.is_static && typeSummary.kind === "class" ? "static " : "";
        return `- ${modifier}\`${field.name}: ${field.type_repr}\``;
      })
      .join("\n");
    md.appendMarkdown("\n\n");
    md.appendMarkdown(lines);
  }
  md.isTrusted = false;
  return md;
}

function renderEnumVariantHover(enumSummary, variant) {
  const md = new vscode.MarkdownString();
  md.appendMarkdown(`\`${enumSummary.name}.${variant}\` *(enum variant)*`);
  if (enumSummary.variants && enumSummary.variants.length > 0) {
    md.appendMarkdown(
      `\n\nDefined on enum \`${enumSummary.name}\` with variants: ${enumSummary.variants.join(", ")}`
    );
  }
  md.isTrusted = false;
  return md;
}

function toRangeFromLocation(location) {
  if (!location) {
    return null;
  }
  const startLine =
    typeof location.start_line === "number"
      ? location.start_line
      : typeof location.startLine === "number"
      ? location.startLine
      : null;
  const startColumn =
    typeof location.start_column === "number"
      ? location.start_column
      : typeof location.startColumn === "number"
      ? location.startColumn
      : null;
  const endLine =
    typeof location.end_line === "number"
      ? location.end_line
      : typeof location.endLine === "number"
      ? location.endLine
      : startLine;
  const endColumn =
    typeof location.end_column === "number"
      ? location.end_column
      : typeof location.endColumn === "number"
      ? location.endColumn
      : startColumn;
  if (
    [startLine, startColumn, endLine, endColumn].some(
      (value) => typeof value !== "number" || Number.isNaN(value)
    )
  ) {
    return null;
  }
  try {
    const start = new vscode.Position(startLine, startColumn);
    const end = new vscode.Position(endLine, endColumn);
    return new vscode.Range(start, end);
  } catch (error) {
    console.warn("TypePy: Invalid location summary received.", error);
    return null;
  }
}

function detectContainerFromContext(document, wordRange) {
  const lineText = document.lineAt(wordRange.start).text;
  const before = lineText.slice(0, wordRange.start.character);
  const match = before.match(/([A-Za-z_][\w]*)\.\s*$/);
  return match ? match[1] : null;
}

function containerMatches(symbolContainer, contextContainer) {
  if (!symbolContainer || !contextContainer) {
    return true;
  }
  if (symbolContainer === contextContainer) {
    return true;
  }
  if (/^[a-z_]/.test(contextContainer)) {
    return true;
  }
  return false;
}

async function fileExists(filePath) {
  try {
    await fs.promises.access(filePath, fs.constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

async function resolveAnalyzerCommand(document, config) {
  const overrideCommand = (config.get("analyzer.command") || "").trim();
  const extraArgs = config.get("analyzer.args") || [];
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!workspaceFolder) {
    return null;
  }

  const ensureEmitSymbols = (args) =>
    args.includes("--emit-symbols") ? args : [...args, "--emit-symbols"];

  if (overrideCommand.length > 0) {
    return {
      cwd: workspaceFolder.uri.fsPath,
      command: overrideCommand,
      args: ensureEmitSymbols([...extraArgs])
    };
  }

  const executableName = process.platform === "win32" ? "typepy.exe" : "typepy";
  const localBinary = path.join(workspaceFolder.uri.fsPath, "target", "debug", executableName);
  if (await fileExists(localBinary)) {
    return {
      cwd: workspaceFolder.uri.fsPath,
      command: localBinary,
      args: ensureEmitSymbols(["--check", ...extraArgs])
    };
  }

  return {
    cwd: workspaceFolder.uri.fsPath,
    command: "cargo",
    args: ensureEmitSymbols(["run", "--quiet", "--", "--check", ...extraArgs])
  };
}

function runProcess(command, args, cwd) {
  return new Promise((resolve) => {
    const child = cp.spawn(command, [...args], {
      cwd,
      shell: false
    });

    let stdout = "";
    let stderr = "";
    let spawnError = null;

    child.stdout.on("data", (data) => {
      stdout += data.toString();
    });

    child.stderr.on("data", (data) => {
      stderr += data.toString();
    });

    child.on("error", (error) => {
      spawnError = error;
    });

    child.on("close", (code) => {
      resolve({ code, stdout, stderr, error: spawnError });
    });
  });
}

function buildDiagnostics(output, document) {
  const diagnostics = [];
  const regex = /^(Parse Error|Analyze Error): (.+) \(line (\d+):(\d+), line (\d+):(\d+)\)$/gm;
  let match;

  while ((match = regex.exec(output)) !== null) {
    const [, kind, message, startLineStr, startColStr, endLineStr, endColStr] = match;
    const startLine = Math.max(parseInt(startLineStr, 10) - 1, 0);
    const startCol = Math.max(parseInt(startColStr, 10), 0);
    const endLine = Math.max(parseInt(endLineStr, 10) - 1, 0);
    let endCol = Math.max(parseInt(endColStr, 10), 0);

    if (startLine >= document.lineCount || endLine >= document.lineCount) {
      continue;
    }

    const lineLength = document.lineAt(endLine).text.length;
    if (endCol <= startCol && endLine === startLine) {
      endCol = startCol + 1;
    }
    if (endCol > lineLength) {
      endCol = lineLength;
    }

    const range = new vscode.Range(startLine, startCol, endLine, endCol);
    diagnostics.push(
      new vscode.Diagnostic(range, `${kind}: ${message}`, vscode.DiagnosticSeverity.Error)
    );
  }

  return diagnostics;
}

function extractSymbolSummary(output) {
  const lines = output.split(/\r?\n/);
  for (let index = lines.length - 1; index >= 0; index -= 1) {
    const line = lines[index].trim();
    if (!line.startsWith("{") || !line.endsWith("}")) {
      continue;
    }
    try {
      const parsed = JSON.parse(line);
      if (parsed && parsed.kind === "typepy-symbols" && parsed.summary) {
        return parsed;
      }
    } catch {
      // ignore parse errors
    }
  }
  return null;
}

async function analyzeDocument(document, diagnosticsCollection, outputChannel, options = {}) {
  if (document.languageId !== "typepy") {
    return;
  }

  const docKey = document.uri.toString();
  runningAnalyses.add(docKey);

  const config = vscode.workspace.getConfiguration("typepy", document.uri);
  const commandInfo = await resolveAnalyzerCommand(document, config);
  if (!commandInfo) {
    vscode.window.showWarningMessage("TypePy: Workspace folder not found for analysis.");
    diagnosticsCollection.delete(document.uri);
    runningAnalyses.delete(docKey);
    return;
  }

  const targetPath = options.targetPath ?? document.fileName;
  const args = [...commandInfo.args, targetPath];
  const commandLine = [commandInfo.command, ...args].join(" ");
  outputChannel.appendLine(`$ ${commandLine}`);

  const result = await runProcess(commandInfo.command, args, commandInfo.cwd);
  if (result.error) {
    outputChannel.appendLine(result.error.message);
    vscode.window.showErrorMessage(`TypePy analyzer failed: ${result.error.message}`);
    diagnosticsCollection.delete(document.uri);
    runningAnalyses.delete(docKey);
    if (typeof options.cleanup === "function") {
      await options.cleanup();
    }
    return;
  }

  if (result.stdout.trim().length > 0) {
    outputChannel.appendLine(result.stdout.trimEnd());
  }
  if (result.stderr.trim().length > 0) {
    outputChannel.appendLine(result.stderr.trimEnd());
  }

  const combinedOutput = `${result.stdout}\n${result.stderr}`;
  const diagnostics = buildDiagnostics(combinedOutput, document);
  const summaryPayload = extractSymbolSummary(combinedOutput);
  if (summaryPayload && summaryPayload.summary) {
    symbolCache.set(docKey, summaryPayload.summary);
  } else {
    symbolCache.delete(docKey);
  }

  if (diagnostics.length > 0) {
    diagnosticsCollection.set(document.uri, diagnostics);
    vscode.window.setStatusBarMessage("TypePy: Analysis reported issues.", 4000);
  } else {
    diagnosticsCollection.delete(document.uri);
    const successMessage =
      result.code === 0 ? "TypePy: Analysis succeeded." : "TypePy: Analysis completed.";
    vscode.window.setStatusBarMessage(successMessage, 3000);
  }

  if (result.code !== 0 && diagnostics.length === 0) {
    vscode.window.showErrorMessage(
      "TypePy analyzer finished with errors. See the TypePy Analyzer output for details."
    );
  }

  if (typeof options.cleanup === "function") {
    await options.cleanup();
  }

  runningAnalyses.delete(docKey);

  return diagnostics;
}

function scheduleAnalysis(document, diagnosticsCollection, outputChannel, debounceMs) {
  const key = document.uri.toString();
  if (document.isUntitled) {
    return;
  }
  const delay = Math.max(100, debounceMs || 700);
  if (pendingChangeTimers.has(key)) {
    clearTimeout(pendingChangeTimers.get(key));
  }
  pendingChangeTimers.set(
    key,
    setTimeout(async () => {
      pendingChangeTimers.delete(key);
      if (document.isClosed) {
        return;
      }
      if (runningAnalyses.has(key)) {
        scheduleAnalysis(document, diagnosticsCollection, outputChannel, delay);
        return;
      }
      try {
        const tempPath = await ensureTempFileForDocument(document);
        if (!tempPath) {
          return;
        }
        await analyzeDocument(document, diagnosticsCollection, outputChannel, {
          targetPath: tempPath,
        });
      } catch (error) {
        outputChannel.appendLine(`TypePy: Failed to analyze on change: ${error.message}`);
      }
    }, delay)
  );
}

async function triggerAnalysisForDocument(document, diagnosticsCollection, outputChannel) {
  if (document.languageId !== "typepy") {
    return;
  }
  if (document.isUntitled) {
    return;
  }
  const key = document.uri.toString();
  if (runningAnalyses.has(key)) {
    return;
  }
  try {
    if (document.isDirty) {
      const tempPath = await ensureTempFileForDocument(document);
      if (!tempPath) {
        return;
      }
      await analyzeDocument(document, diagnosticsCollection, outputChannel, {
        targetPath: tempPath,
      });
    } else {
      await analyzeDocument(document, diagnosticsCollection, outputChannel);
    }
  } catch (error) {
    outputChannel.appendLine(`TypePy: Initial analysis failed: ${error.message}`);
  }
}

function activate(context) {
  const diagnosticsCollection = vscode.languages.createDiagnosticCollection("typepy");
  const outputChannel = vscode.window.createOutputChannel("TypePy Analyzer");

  context.subscriptions.push(diagnosticsCollection, outputChannel);

  const analyzeCommand = vscode.commands.registerCommand("typepy.analyzeFile", async () => {
    const editor = vscode.window.activeTextEditor;
    if (!editor || editor.document.languageId !== "typepy") {
      vscode.window.showInformationMessage("Open a TypePy file to analyze.");
      return;
    }
    if (editor.document.isUntitled) {
      vscode.window.showWarningMessage("Please save the file before running the TypePy analyzer.");
      return;
    }
    if (editor.document.isDirty) {
      await editor.document.save();
    }
    await analyzeDocument(editor.document, diagnosticsCollection, outputChannel);
  });

  const saveSubscription = vscode.workspace.onDidSaveTextDocument(async (document) => {
    if (document.languageId !== "typepy") {
      return;
    }
    const config = vscode.workspace.getConfiguration("typepy", document.uri);
    if (!config.get("analyzer.runOnSave", true)) {
      return;
    }
    await analyzeDocument(document, diagnosticsCollection, outputChannel);
  });

  const configSubscription = vscode.workspace.onDidChangeConfiguration(async (event) => {
    if (event.affectsConfiguration("typepy")) {
      const editor = vscode.window.activeTextEditor;
      if (editor && editor.document.languageId === "typepy") {
        const config = vscode.workspace.getConfiguration("typepy", editor.document.uri);
        const debounceMs = Math.max(config.get("analyzer.changeDebounceMs", 700), 100);
        if (config.get("analyzer.runOnSave", true)) {
          await analyzeDocument(editor.document, diagnosticsCollection, outputChannel);
        } else {
          diagnosticsCollection.delete(editor.document.uri);
        }
        if (config.get("analyzer.runOnChange", false)) {
          scheduleAnalysis(editor.document, diagnosticsCollection, outputChannel, debounceMs);
        } else {
          const key = editor.document.uri.toString();
          const timer = pendingChangeTimers.get(key);
          if (timer) {
            clearTimeout(timer);
            pendingChangeTimers.delete(key);
          }
        }
      }
    }
  });

  const changeSubscription = vscode.workspace.onDidChangeTextDocument((event) => {
    const { document } = event;
    if (document.languageId !== "typepy") {
      return;
    }
    const config = vscode.workspace.getConfiguration("typepy", document.uri);
    if (!config.get("analyzer.runOnChange", false)) {
      return;
    }
    const key = document.uri.toString();
    symbolCache.delete(key);
    const debounceMs = Math.max(config.get("analyzer.changeDebounceMs", 700), 100);
    scheduleAnalysis(document, diagnosticsCollection, outputChannel, debounceMs);
  });

  const closeSubscription = vscode.workspace.onDidCloseTextDocument(async (document) => {
    if (document.languageId !== "typepy") {
      return;
    }
    const key = document.uri.toString();
    symbolCache.delete(key);
    const timer = pendingChangeTimers.get(key);
    if (timer) {
      clearTimeout(timer);
      pendingChangeTimers.delete(key);
    }
    runningAnalyses.delete(key);
    await removeTempFileForDocument(document);
    diagnosticsCollection.delete(document.uri);
  });

  const completionProvider = vscode.languages.registerCompletionItemProvider(
    { language: "typepy", scheme: "file" },
    {
      provideCompletionItems(document, position) {
        const linePrefix = document.lineAt(position).text.slice(0, position.character);
        const items = [];
        const summary = symbolCache.get(document.uri.toString()) || null;

        const inMemberAccess = /\.\s*$/.test(linePrefix);
        const inTypeContext = /(:|->)\s*([A-Za-z_][\w]*)?$/.test(linePrefix);

        if (inMemberAccess) {
          if (summary) {
            const enumMatch = linePrefix.match(/([A-Za-z_][\w]*)\.\s*$/);
            if (enumMatch) {
              const enumName = enumMatch[1];
              summary.types
                .filter((typeInfo) => typeInfo.kind === "enum" && typeInfo.name === enumName)
                .forEach((enumInfo) => {
                  if (enumInfo.variants) {
                    enumInfo.variants.forEach((variant) => {
                      items.push(createEnumVariantCompletion(enumName, variant));
                    });
                  }
                });
            } else {
              const trimmedPrefix = linePrefix.replace(/\s*$/, "");
              const charBeforeDot =
                trimmedPrefix.length >= 2
                  ? trimmedPrefix.charAt(trimmedPrefix.length - 2)
                  : "";
              const hasIdentifierBeforeDot = /\w/.test(charBeforeDot);
              if (!hasIdentifierBeforeDot) {
                const seen = new Set();
                summary.types
                  .filter((typeInfo) => typeInfo.kind === "enum" && Array.isArray(typeInfo.variants))
                  .forEach((enumInfo) => {
                    enumInfo.variants.forEach((variant) => {
                      const key = `${enumInfo.name}::${variant}`;
                      if (seen.has(key)) {
                        return;
                      }
                      seen.add(key);
                      items.push(
                        createEnumVariantCompletion(enumInfo.name, variant, {
                          label: `.${variant}`,
                          insertText: variant,
                          filterText: `.${variant}`,
                          detail: `${enumInfo.name} variant (shorthand)`,
                          sortTextPrefix: "1_."
                        })
                      );
                    });
                  });
              }
            }
          }
          MEMBER_SUGGESTIONS.forEach((definition) => {
            items.push(createMemberCompletion(definition));
          });
          return items;
        }

        if (inTypeContext) {
          PRIMITIVE_TYPES.forEach((typeName) => {
            items.push(createTypeCompletion(typeName));
          });
          if (summary) {
            summary.types.forEach((typeInfo) => {
              const completion = createTypeCompletionFromSummary(typeInfo);
              if (completion) {
                items.push(completion);
              }
            });
          }
          items.push(createConstantCompletion("None"));
          return items;
        }

        if (summary) {
          summary.functions.forEach((fnSummary) => {
            items.push(createFunctionCompletionFromSummary(fnSummary));
          });
          summary.variables.forEach((variableSummary) => {
            items.push(createVariableCompletionFromSummary(variableSummary));
          });
          summary.types.forEach((typeInfo) => {
            const completion = createTypeCompletionFromSummary(typeInfo);
            if (completion) {
              items.push(completion);
            }
          });
        }

        KEYWORDS.forEach((word) => {
          items.push(createKeywordCompletion(word));
        });

        BUILTIN_CONSTANTS.forEach((name) => {
          items.push(createConstantCompletion(name));
        });

        BUILTIN_FUNCTIONS.forEach((definition) => {
          items.push(createFunctionCompletion(definition));
        });

        items.push(
          createSnippetCompletion(
            "def … -> …",
            "def ${1:funcName}(${2:param}: ${3:Type}) -> ${4:ReturnType} {\n    $0\n}",
            "Function definition",
            "Insert a TypePy function definition skeleton."
          )
        );

        items.push(
          createSnippetCompletion(
            "class …",
            "class ${1:TypeName} {\n    def ${2:method}(${3:self}) -> ${4:None} {\n        $0\n    }\n}",
            "Class definition",
            "Insert a TypePy class definition block."
          )
        );

        items.push(
          createSnippetCompletion(
            "if …",
            "if ${1:condition} {\n    $0\n}",
            "If statement",
            "Insert an if statement."
          )
        );

        items.push(
          createSnippetCompletion(
            "for in …",
            "for ${1:item} in ${2:iterable} {\n    $0\n}",
            "For loop",
            "Insert a for loop."
          )
        );

        return items;
      }
    },
    ".",
    ":",
    ">"
  );

  const hoverProvider = vscode.languages.registerHoverProvider(
    { language: "typepy", scheme: "file" },
    {
      provideHover(document, position) {
        const summary = symbolCache.get(document.uri.toString());
        if (!summary) {
          return undefined;
        }
        const wordRange = document.getWordRangeAtPosition(position, /[A-Za-z_][\w]*/);
        if (!wordRange) {
          return undefined;
        }
        const word = document.getText(wordRange);
        const container = detectContainerFromContext(document, wordRange);

        const hoverItems = [];
        const seenFunctionKeys = new Set();
        const seenVariableKeys = new Set();
        const seenTypeNames = new Set();

        if (summary.functions) {
          summary.functions
            .filter((fn) => fn.name === word)
            .filter((fn) => containerMatches(fn.container || null, container))
            .forEach((fn) => {
              const key = `${fn.container || "global"}::${fn.name}`;
              if (seenFunctionKeys.has(key)) {
                return;
              }
              seenFunctionKeys.add(key);
              hoverItems.push(renderFunctionHover(fn));
            });
        }

        if (summary.variables) {
          summary.variables
            .filter((variable) => variable.name === word)
            .filter((variable) => containerMatches(variable.container || null, container))
            .forEach((variable) => {
              const key = `${variable.container || "global"}::${variable.name}`;
              if (seenVariableKeys.has(key)) {
                return;
              }
              seenVariableKeys.add(key);
              hoverItems.push(renderVariableHover(variable));
            });
        }

        if (summary.types) {
          summary.types
            .filter((typeInfo) => typeInfo.name === word)
            .forEach((typeInfo) => {
              if (seenTypeNames.has(typeInfo.name)) {
                return;
              }
              seenTypeNames.add(typeInfo.name);
              const typeHover = renderTypeHover(typeInfo);
              if (typeInfo.kind === "class" && summary.functions) {
                const methodLines = summary.functions
                  .filter((fn) => fn.container === typeInfo.name)
                  .map((fn) => {
                    const params = fn.params
                      .map((param) => `${param.name}: ${param.type_repr}`)
                      .join(", ");
                    return `- \`${fn.name}(${params}) -> ${fn.return_type}\``;
                  });
                if (methodLines.length > 0) {
                  typeHover.appendMarkdown("\n\n**Methods**\n" + methodLines.join("\n"));
                }
              }
              hoverItems.push(typeHover);
            });

          const enumMatches = summary.types.filter(
            (typeInfo) =>
              typeInfo.kind === "enum" &&
              typeInfo.variants &&
              typeInfo.variants.includes(word) &&
              (!container || container === typeInfo.name)
          );
          enumMatches.forEach((enumInfo) => {
            hoverItems.push(renderEnumVariantHover(enumInfo, word));
          });
        }

        if (hoverItems.length === 0) {
          return undefined;
        }

        return new vscode.Hover(hoverItems, wordRange);
      }
    }
  );

  const definitionProvider = vscode.languages.registerDefinitionProvider(
    { language: "typepy", scheme: "file" },
    {
      provideDefinition(document, position) {
        const summary = symbolCache.get(document.uri.toString());
        if (!summary) {
          return undefined;
        }
        const wordRange = document.getWordRangeAtPosition(position, /[A-Za-z_][\w]*/);
        if (!wordRange) {
          return undefined;
        }
        const word = document.getText(wordRange);
        if (!word) {
          return undefined;
        }
        const contextContainer = detectContainerFromContext(document, wordRange);
        const uri = document.uri;
        const seenKeys = new Set();
        const results = [];

        const pushLocation = (locationSummary, keyPrefix) => {
          if (!locationSummary) {
            return;
          }
          const range = toRangeFromLocation(locationSummary);
          if (!range) {
            return;
          }
          const key = `${keyPrefix}:${range.start.line}:${range.start.character}:${range.end.line}:${range.end.character}`;
          if (seenKeys.has(key)) {
            return;
          }
          seenKeys.add(key);
          results.push(new vscode.Location(uri, range));
        };

        if (Array.isArray(summary.functions)) {
          summary.functions
            .filter((fn) => fn && fn.name === word)
            .filter((fn) => containerMatches(fn.container || null, contextContainer))
            .forEach((fn) => {
              pushLocation(fn.location, `fn:${fn.container || "global"}`);
            });
        }

        if (Array.isArray(summary.variables)) {
          summary.variables
            .filter((variable) => variable && variable.name === word)
            .filter((variable) => containerMatches(variable.container || null, contextContainer))
            .forEach((variable) => {
              pushLocation(variable.location, `var:${variable.container || "global"}`);
            });
        }

        if (Array.isArray(summary.types)) {
          summary.types
            .filter((typeInfo) => typeInfo && typeInfo.name === word)
            .forEach((typeInfo) => {
              pushLocation(typeInfo.location, `type:${typeInfo.kind || "unknown"}`);
            });
        }

        return results.length > 0 ? results : undefined;
      }
    }
  );

  const openSubscription = vscode.workspace.onDidOpenTextDocument(async (document) => {
    await triggerAnalysisForDocument(document, diagnosticsCollection, outputChannel);
  });

  context.subscriptions.push(
    analyzeCommand,
    saveSubscription,
    configSubscription,
    changeSubscription,
    closeSubscription,
    openSubscription,
    completionProvider,
    hoverProvider,
    definitionProvider
  );

  void (async () => {
    const editors = vscode.window.visibleTextEditors || [];
    for (const editor of editors) {
      await triggerAnalysisForDocument(editor.document, diagnosticsCollection, outputChannel);
    }
  })();
}

function deactivate() {
  // No resources to clean up explicitly.
}

module.exports = {
  activate,
  deactivate
};
