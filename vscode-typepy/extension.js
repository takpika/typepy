"use strict";

const vscode = require("vscode");
const path = require("path");
const fs = require("fs");
const cp = require("child_process");
const crypto = require("crypto");
const os = require("os");

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

const DEFAULT_DEBUG_PYTHON = process.platform === "win32" ? "python" : "python3";

function normalizeFsPath(filePath) {
  if (!filePath) {
    return null;
  }
  const resolved = path.isAbsolute(filePath) ? filePath : path.resolve(filePath);
  const normalized = path.normalize(resolved);
  return process.platform === "win32" ? normalized.toLowerCase() : normalized;
}

function countFileLines(filePath) {
  try {
    const content = fs.readFileSync(filePath, "utf8");
    if (content.length === 0) {
      return 0;
    }
    const matches = content.match(/\r\n|\r|\n/g);
    const lineBreaks = matches ? matches.length : 0;
    return lineBreaks + 1;
  } catch (error) {
    console.warn(`TypePy: Failed to count lines for ${filePath}: ${error.message}`);
    return 0;
  }
}

class FileMapping {
  constructor(typepyPath, pythonPath, rawMappings) {
    this.typepyPath = typepyPath;
    this.pythonPath = pythonPath;
    this.normalizedTypepyPath = normalizeFsPath(typepyPath);
    this.normalizedPythonPath = normalizeFsPath(pythonPath);
    this.maxOriginalLine = countFileLines(typepyPath);
    this.maxGeneratedLine = countFileLines(pythonPath);
    this.originalAnchors = [];
    this.generatedAnchors = [];

    if (Array.isArray(rawMappings)) {
      rawMappings
        .filter((entry) =>
          Number.isFinite(entry.original_line) && Number.isFinite(entry.generated_line)
        )
        .forEach((entry) => {
          const original = Math.max(1, Math.floor(entry.original_line));
          const generated = Math.max(1, Math.floor(entry.generated_line));
          this.originalAnchors.push({ original, generated });
          this.generatedAnchors.push({ original, generated });
        });
    }

    this.originalAnchors.sort((a, b) => a.original - b.original);
    this.generatedAnchors.sort((a, b) => a.generated - b.generated);
  }

  clampGenerated(line) {
    if (this.maxGeneratedLine > 0) {
      return Math.max(1, Math.min(line, this.maxGeneratedLine));
    }
    return Math.max(1, line);
  }

  clampOriginal(line) {
    if (this.maxOriginalLine > 0) {
      return Math.max(1, Math.min(line, this.maxOriginalLine));
    }
    return Math.max(1, line);
  }

  originalToGenerated(line) {
    if (!Number.isFinite(line)) {
      return line;
    }
    if (this.originalAnchors.length === 0) {
      return this.clampGenerated(line);
    }
    let anchor = this.originalAnchors[0];
    for (const candidate of this.originalAnchors) {
      if (candidate.original > line) {
        break;
      }
      anchor = candidate;
    }
    const offset = anchor.generated - anchor.original;
    return this.clampGenerated(line + offset);
  }

  generatedToOriginal(line) {
    if (!Number.isFinite(line)) {
      return line;
    }
    if (this.generatedAnchors.length === 0) {
      return this.clampOriginal(line);
    }
    let anchor = this.generatedAnchors[0];
    for (const candidate of this.generatedAnchors) {
      if (candidate.generated > line) {
        break;
      }
      anchor = candidate;
    }
    const offset = anchor.original - anchor.generated;
    return this.clampOriginal(line + offset);
  }
}

class SourceMapRegistry {
  constructor(outputChannel) {
    this.outputChannel = outputChannel;
    this.mappingsByTypepy = new Map();
    this.mappingsByPython = new Map();
  }

  registerFromPayload(typepyPath, pythonPath, sourceMapPayload) {
    try {
      const mapping = new FileMapping(typepyPath, pythonPath, sourceMapPayload?.mappings || []);
      if (mapping.originalAnchors.length === 0) {
        this.outputChannel.appendLine(
          `TypePy Debug: Source map for ${typepyPath} is sparse; breakpoint mapping may be approximate.`
        );
      }
      this.mappingsByTypepy.set(mapping.normalizedTypepyPath, mapping);
      this.mappingsByPython.set(mapping.normalizedPythonPath, mapping);
    } catch (error) {
      this.outputChannel.appendLine(
        `TypePy Debug: Failed to register source map for ${typepyPath}: ${error.message}`
      );
    }
  }

  getMappingForTypePy(typepyPath) {
    return this.mappingsByTypepy.get(normalizeFsPath(typepyPath));
  }

  getMappingForPython(pythonPath) {
    return this.mappingsByPython.get(normalizeFsPath(pythonPath));
  }

  mapOriginalToGenerated(typepyPath, line) {
    const mapping = this.getMappingForTypePy(typepyPath);
    if (!mapping) {
      return null;
    }
    return {
      path: mapping.pythonPath,
      line: mapping.originalToGenerated(line)
    };
  }

  mapGeneratedToOriginal(pythonPath, line) {
    const mapping = this.getMappingForPython(pythonPath);
    if (!mapping) {
      return null;
    }
    return {
      path: mapping.typepyPath,
      line: mapping.generatedToOriginal(line)
    };
  }
}

class DapMessageReader {
  constructor(onMessage) {
    this.onMessage = onMessage;
    this.buffer = Buffer.alloc(0);
    this.expectedLength = null;
  }

  handleData(chunk) {
    this.buffer = Buffer.concat([this.buffer, chunk]);
    while (true) {
      if (this.expectedLength === null) {
        const separatorIndex = this.buffer.indexOf("\r\n\r\n");
        if (separatorIndex === -1) {
          return;
        }
        const header = this.buffer.slice(0, separatorIndex).toString("utf8");
        const match = /Content-Length: *(\d+)/i.exec(header);
        if (!match) {
          throw new Error(`Invalid DAP header: ${header}`);
        }
        this.expectedLength = parseInt(match[1], 10);
        this.buffer = this.buffer.slice(separatorIndex + 4);
      }

      if (this.buffer.length < this.expectedLength) {
        return;
      }

      const messageBuffer = this.buffer.slice(0, this.expectedLength);
      this.buffer = this.buffer.slice(this.expectedLength);
      this.expectedLength = null;

      const messageText = messageBuffer.toString("utf8");
      let message;
      try {
        message = JSON.parse(messageText);
      } catch (error) {
        console.error("TypePy Debug: Failed to parse DAP message", error);
        continue;
      }
      this.onMessage(message);
    }
  }
}

function sendDapMessage(stream, message) {
  if (!stream.writable) {
    return;
  }
  const payload = JSON.stringify(message);
  const header = `Content-Length: ${Buffer.byteLength(payload, "utf8")}\r\n\r\n`;
  stream.write(header, "utf8");
  stream.write(payload, "utf8");
}

class TypePyDebugAdapter {
  constructor(sessionConfiguration, outputChannel) {
    this.outputChannel = outputChannel;
    this.sessionConfiguration = sessionConfiguration;
    this.onDidSendMessageEmitter = new vscode.EventEmitter();
    this.onDidSendMessage = this.onDidSendMessageEmitter.event;
    this.pendingRequests = new Map();
    this.sourceMaps = new SourceMapRegistry(outputChannel);
    this.disposed = false;
    this.sequenceCounter = 1;
    this.startError = null;

    this.typepyInfo = sessionConfiguration.__typepy || {};
    this.debugpyProcess = null;
    this.debugpyReader = null;

    this.loadSourceMap();
    this.startDebugpyAdapter();
  }

  loadSourceMap() {
    const mapPath = this.typepyInfo.mapPath;
    if (!mapPath) {
      return;
    }
    try {
      const content = fs.readFileSync(mapPath, "utf8");
      const payload = JSON.parse(content);
      this.sourceMaps.registerFromPayload(
        this.typepyInfo.programPath,
        this.typepyInfo.pythonPath,
        payload
      );
    } catch (error) {
      this.outputChannel.appendLine(
        `TypePy Debug: Failed to read source map ${mapPath}: ${error.message}`
      );
    }
  }

  startDebugpyAdapter() {
    const pythonExecutable = this.typepyInfo.pythonInterpreter || DEFAULT_DEBUG_PYTHON;
    this.outputChannel.appendLine(
      `TypePy Debug: Launching debugpy adapter with ${pythonExecutable}`
    );
    try {
      this.debugpyProcess = cp.spawn(pythonExecutable, ["-m", "debugpy.adapter"], {
        cwd: this.typepyInfo.cwd,
        stdio: ["pipe", "pipe", "pipe"]
      });
    } catch (error) {
      this.startError = error;
      this.outputChannel.appendLine(
        `TypePy Debug: Failed to spawn debugpy.adapter: ${error.message}`
      );
      vscode.window.showErrorMessage(
        "TypePy: Failed to start debugpy adapter. Please verify that debugpy is installed."
      );
      return;
    }

    this.debugpyProcess.on("error", (error) => {
      this.outputChannel.appendLine(
        `TypePy Debug: debugpy.adapter process error: ${error.message}`
      );
    });

    this.debugpyProcess.on("exit", (code, signal) => {
      const suffix = signal ? `signal ${signal}` : `code ${code}`;
      this.outputChannel.appendLine(`TypePy Debug: debugpy.adapter exited with ${suffix}`);
    });

    this.debugpyReader = new DapMessageReader((message) => {
      this.handleServerMessage(message);
    });

    this.debugpyProcess.stdout.on("data", (chunk) => {
      try {
        this.debugpyReader.handleData(chunk);
      } catch (error) {
        this.outputChannel.appendLine(
          `TypePy Debug: Failed to handle debugpy message: ${error.message}`
        );
      }
    });

    this.debugpyProcess.stderr.on("data", (chunk) => {
      this.outputChannel.append(chunk.toString());
    });
  }

  handleMessage(message) {
    if (this.disposed) {
      return;
    }

    if (this.startError) {
      if (message.type === "request") {
        const failureResponse = {
          type: "response",
          seq: this.sequenceCounter++,
          command: message.command,
          request_seq: message.seq,
          success: false,
          message: `TypePy debug adapter failed to start: ${this.startError.message}`
        };
        this.onDidSendMessageEmitter.fire(failureResponse);
      }
      return;
    }

    if (!this.debugpyProcess || !this.debugpyProcess.stdin.writable) {
      return;
    }

    const metadata = { command: message.command };
    const forwarded = this.transformClientMessage(message, metadata);

    if (message.type === "request") {
      this.pendingRequests.set(message.seq, metadata);
    }

    sendDapMessage(this.debugpyProcess.stdin, forwarded);
  }

  transformClientMessage(message, metadata) {
    if (message.type !== "request") {
      return message;
    }

    if (message.command === "setBreakpoints") {
      const args = { ...message.arguments };
      if (args && args.source && args.source.path) {
        const originalSourcePath = args.source.path;
        const newArgs = { ...args };
        const newSource = { ...args.source };
        const breakpoints = Array.isArray(args.breakpoints)
          ? args.breakpoints.map((bp) => ({ ...bp }))
          : [];

        let mappedSourcePath = null;
        breakpoints.forEach((bp, index) => {
          const mapped = this.sourceMaps.mapOriginalToGenerated(originalSourcePath, bp.line);
          if (mapped) {
            breakpoints[index].line = mapped.line;
            mappedSourcePath = mapped.path || mappedSourcePath;
          }
        });

        if (mappedSourcePath) {
          newSource.path = mappedSourcePath;
          newSource.name = path.basename(mappedSourcePath);
        }

        newArgs.source = newSource;
        newArgs.breakpoints = breakpoints;
        metadata.typepySource = originalSourcePath;
        metadata.pythonSource = mappedSourcePath || originalSourcePath;
        return {
          ...message,
          arguments: newArgs
        };
      }
      return message;
    }

    if (message.command === "launch") {
      const args = { ...message.arguments };
      const pythonPath = this.typepyInfo.pythonPath;
      if (pythonPath) {
        args.program = pythonPath;
      }
      if (!args.cwd) {
        args.cwd = this.typepyInfo.cwd || path.dirname(pythonPath || "");
      }
      const interpreter = this.typepyInfo.pythonInterpreter;
      const hasPython = args.python !== undefined && args.python !== null && args.python !== "";
      const hasPythonPath =
        args.pythonPath !== undefined && args.pythonPath !== null && args.pythonPath !== "";
      if (interpreter && !hasPython && !hasPythonPath) {
        args.python = interpreter;
      }
      if (!args.console) {
        args.console = "integratedTerminal";
      }
      return {
        ...message,
        arguments: args
      };
    }

    return message;
  }

  handleServerMessage(message) {
    if (message.type === "response" && this.pendingRequests.has(message.request_seq)) {
      const metadata = this.pendingRequests.get(message.request_seq);
      if (metadata.command === "setBreakpoints") {
        this.transformBreakpointResponse(message, metadata);
      } else if (metadata.command === "stackTrace") {
        this.transformStackTraceResponse(message);
      }
      this.pendingRequests.delete(message.request_seq);
    } else if (message.type === "event" && message.event === "output") {
      const bodyText = message.body && message.body.output ? message.body.output : "";
      if (bodyText) {
        this.outputChannel.append(bodyText);
      }
    }

    this.onDidSendMessageEmitter.fire(message);
  }

  transformBreakpointResponse(message, metadata) {
    if (!message.body || !Array.isArray(message.body.breakpoints)) {
      return;
    }
    const pythonSource = metadata.pythonSource;
    const typepySource = metadata.typepySource;
    if (!pythonSource || !typepySource) {
      return;
    }
    message.body.breakpoints = message.body.breakpoints.map((bp) => {
      const mapped = this.sourceMaps.mapGeneratedToOriginal(pythonSource, bp.line);
      if (mapped) {
        const source = { ...(bp.source || {}) };
        source.path = typepySource;
        source.name = path.basename(typepySource);
        source.origin = "TypePy";
        bp.source = source;
        bp.line = mapped.line;
      }
      return bp;
    });
  }

  transformStackTraceResponse(message) {
    if (!message.body || !Array.isArray(message.body.stackFrames)) {
      return;
    }
    message.body.stackFrames = message.body.stackFrames.map((frame) => {
      if (!frame || !frame.source || !frame.source.path) {
        return frame;
      }
      const mapped = this.sourceMaps.mapGeneratedToOriginal(frame.source.path, frame.line);
      if (!mapped) {
        return frame;
      }
      const updatedSource = { ...frame.source };
      updatedSource.path = mapped.path;
      updatedSource.name = path.basename(mapped.path);
      updatedSource.origin = "TypePy";
      frame.source = updatedSource;
      frame.line = mapped.line;
      return frame;
    });
  }

  dispose() {
    this.disposed = true;
    if (this.debugpyProcess) {
      this.debugpyProcess.stdin?.end();
      if (!this.debugpyProcess.killed) {
        this.debugpyProcess.kill();
      }
    }
    this.onDidSendMessageEmitter.dispose();
  }
}

class TypePyDebugAdapterFactory {
  constructor(outputChannel) {
    this.outputChannel = outputChannel;
  }

  createDebugAdapterDescriptor(session) {
    const adapter = new TypePyDebugAdapter(session.configuration, this.outputChannel);
    return new vscode.DebugAdapterInlineImplementation(adapter);
  }

  dispose() {}
}

class TypePyDebugConfigurationProvider {
  constructor(outputChannel) {
    this.outputChannel = outputChannel;
  }

  resolveProgramPath(folder, rawProgram) {
    let programPath = rawProgram;

    if (!programPath) {
      const editor = vscode.window.activeTextEditor;
      if (editor && editor.document.languageId === "typepy") {
        return editor.document.fileName;
      }
      return null;
    }

    const editor = vscode.window.activeTextEditor;
    if (programPath.includes("${file}")) {
      if (editor && editor.document.languageId === "typepy") {
        programPath = programPath.replace(/\$\{file\}/g, editor.document.fileName);
      } else {
        return null;
      }
    }

    const workspacePath = folder
      ? folder.uri.fsPath
      : vscode.workspace.workspaceFolders && vscode.workspace.workspaceFolders.length > 0
      ? vscode.workspace.workspaceFolders[0].uri.fsPath
      : null;

    if (workspacePath) {
      programPath = programPath
        .replace(/\$\{workspaceFolder\}/g, workspacePath)
        .replace(/\$\{workspaceRoot\}/g, workspacePath);
    }

    if (programPath.startsWith("~")) {
      programPath = path.join(os.homedir(), programPath.slice(1));
    }
    if (!path.isAbsolute(programPath)) {
      const base = workspacePath || process.cwd();
      programPath = path.resolve(base, programPath);
    }
    return programPath;
  }

  findOpenDocument(programPath) {
    const normalized = normalizeFsPath(programPath);
    return vscode.workspace.textDocuments.find(
      (doc) => normalizeFsPath(doc.uri.fsPath) === normalized
    );
  }

  async ensureTranspiled(document, programPath) {
    const typepyUri = vscode.Uri.file(programPath);
    const workspaceFolder = vscode.workspace.getWorkspaceFolder(typepyUri);
    if (!workspaceFolder) {
      vscode.window.showErrorMessage(
        "TypePy: Debugging requires the file to be within a workspace folder."
      );
      return null;
    }

    const config = vscode.workspace.getConfiguration("typepy", typepyUri);
    const commandInfo = await resolveAnalyzerCommand(document, config, {
      includeCheck: false,
      includeEmitSymbols: false,
      additionalArgs: ["--source-map"]
    });
    if (!commandInfo) {
      return null;
    }

    const args = [...commandInfo.args, programPath];
    this.outputChannel.appendLine(`$ ${commandInfo.command} ${args.join(" ")}`);
    const result = await runProcess(commandInfo.command, args, commandInfo.cwd);
    if (result.stdout.trim().length > 0) {
      this.outputChannel.appendLine(result.stdout.trimEnd());
    }
    if (result.stderr.trim().length > 0) {
      this.outputChannel.appendLine(result.stderr.trimEnd());
    }
    if (result.code !== 0 || result.error) {
      vscode.window.showErrorMessage(
        "TypePy: Transpilation failed. See TypePy Debug output for details."
      );
      return null;
    }

    return {
      workspaceFolder,
      commandInfo
    };
  }

  async resolveDebugConfiguration(folder, config) {
    const resolvedConfig = { ...config };
    resolvedConfig.type = resolvedConfig.type || "typepy";
    resolvedConfig.request = resolvedConfig.request || "launch";
    resolvedConfig.name = resolvedConfig.name || "TypePy: Launch";

    const programPath = this.resolveProgramPath(folder, resolvedConfig.program);
    if (!programPath) {
      vscode.window.showErrorMessage(
        "TypePy: Unable to determine the program to debug. Please specify the program path."
      );
      return null;
    }

    try {
      await fs.promises.access(programPath, fs.constants.R_OK);
    } catch (error) {
      vscode.window.showErrorMessage(
        `TypePy: Program file not found or inaccessible: ${programPath}`
      );
      return null;
    }

    let document = this.findOpenDocument(programPath);
    if (!document) {
      try {
        document = await vscode.workspace.openTextDocument(programPath);
      } catch (error) {
        vscode.window.showErrorMessage(
          `TypePy: Failed to open program file for debugging: ${error.message}`
        );
        return null;
      }
    }

    if (document.isDirty) {
      const saved = await document.save();
      if (!saved) {
        vscode.window.showWarningMessage("TypePy: Debugging cancelled because file was not saved.");
        return null;
      }
    }

    const transpileResult = await this.ensureTranspiled(document, programPath);
    if (!transpileResult) {
      return null;
    }

    const parsedProgram = path.parse(programPath);
    const pythonPath = path.join(parsedProgram.dir, `${parsedProgram.name}.py`);
    const mapPath = `${pythonPath}.map`;

    try {
      await fs.promises.access(pythonPath, fs.constants.R_OK);
    } catch (error) {
      vscode.window.showErrorMessage(
        `TypePy: Generated Python file not found: ${pythonPath}`
      );
      return null;
    }

    try {
      await fs.promises.access(mapPath, fs.constants.R_OK);
    } catch (error) {
      vscode.window.showWarningMessage(
        `TypePy: Source map not found for ${programPath}. Breakpoint mapping may be unavailable.`
      );
    }

    const typepyConfig = vscode.workspace.getConfiguration(
      "typepy",
      vscode.Uri.file(programPath)
    );
    const interpreter =
      resolvedConfig.pythonPath ||
      typepyConfig.get("debug.pythonPath") ||
      DEFAULT_DEBUG_PYTHON;

    resolvedConfig.__typepy = {
      programPath,
      pythonPath,
      mapPath,
      pythonInterpreter: interpreter,
      cwd: resolvedConfig.cwd || parsedProgram.dir
    };

    return resolvedConfig;
  }
}

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

async function resolveAnalyzerCommand(document, config, options = {}) {
  const overrideCommand = (config.get("analyzer.command") || "").trim();
  const extraArgs = config.get("analyzer.args") || [];
  const workspaceFolder = vscode.workspace.getWorkspaceFolder(document.uri);
  if (!workspaceFolder) {
    return null;
  }

  const includeCheck = options.includeCheck !== false;
  const includeEmitSymbols = options.includeEmitSymbols !== false;
  const additionalArgs = options.additionalArgs || [];

  const mergedArgs = [];
  if (includeCheck) {
    mergedArgs.push("--check");
  }
  mergedArgs.push(...extraArgs);
  mergedArgs.push(...additionalArgs);

  const finalArgs = includeEmitSymbols
    ? mergedArgs.includes("--emit-symbols")
      ? mergedArgs.slice()
      : [...mergedArgs, "--emit-symbols"]
    : mergedArgs.filter((arg) => arg !== "--emit-symbols");

  if (overrideCommand.length > 0) {
    return {
      cwd: workspaceFolder.uri.fsPath,
      command: overrideCommand,
      args: finalArgs.slice()
    };
  }

  const executableName = process.platform === "win32" ? "typepy.exe" : "typepy";
  const localBinary = path.join(workspaceFolder.uri.fsPath, "target", "debug", executableName);
  if (await fileExists(localBinary)) {
    return {
      cwd: workspaceFolder.uri.fsPath,
      command: localBinary,
      args: finalArgs.slice()
    };
  }

  return {
    cwd: workspaceFolder.uri.fsPath,
    command: "cargo",
    args: ["run", "--quiet", "--", ...finalArgs]
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
  const debugOutputChannel = vscode.window.createOutputChannel("TypePy Debug");
  const debugConfigProvider = new TypePyDebugConfigurationProvider(debugOutputChannel);
  const debugAdapterFactory = new TypePyDebugAdapterFactory(debugOutputChannel);

  const debugConfigRegistration = vscode.debug.registerDebugConfigurationProvider(
    "typepy",
    debugConfigProvider
  );
  const debugAdapterRegistration = vscode.debug.registerDebugAdapterDescriptorFactory(
    "typepy",
    debugAdapterFactory
  );

  context.subscriptions.push(
    diagnosticsCollection,
    outputChannel,
    debugOutputChannel,
    debugConfigRegistration,
    debugAdapterRegistration
  );

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
