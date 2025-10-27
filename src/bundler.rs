use crate::analyzer::Analyzer;
use crate::lexer::Lexer;
use crate::optimizer;
use crate::parser::{self, AstNode};
use crate::transpiler::{self, SourceMapEntry, TypePyModuleBinding, TypePyTranspileContext};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

pub struct BundleOutput {
    pub code: String,
    pub source_map: Vec<SourceMapEntry>,
}

pub fn bundle_project(
    entry_path: &Path,
    entry_ast: &AstNode,
    use_std: bool,
) -> io::Result<BundleOutput> {
    let canonical_entry = fs::canonicalize(entry_path)?;
    let root_dir = canonical_entry
        .parent()
        .map(|p| p.to_path_buf())
        .unwrap_or_else(|| PathBuf::from("."));

    let mut project = Project::new(root_dir.clone(), use_std);
    project.insert_entry(canonical_entry.clone(), entry_ast.clone())?;
    project.resolve_file(&canonical_entry)?;

    let entry_info = project
        .files
        .get(canonical_entry.as_path())
        .expect("entry file should be registered");
    let mut used_names = collect_exports(&entry_info.ast);

    let module_order = project.module_order(&canonical_entry);

    let mut module_public_names: HashMap<PathBuf, HashMap<String, String>> = HashMap::new();
    for module_path in &module_order {
        if let Some(info) = project.files.get(module_path.as_path()) {
            let exports = collect_exports(&info.ast);
            let public = determine_public_names(info, exports, &mut used_names);
            module_public_names.insert(module_path.clone(), public);
        }
    }

    let mut blocks = Vec::new();
    for module_path in &module_order {
        if let Some(info) = project.files.get(module_path.as_path()) {
            blocks.push(transpile_block(info, true, &module_public_names)?);
        }
    }
    blocks.push(transpile_block(entry_info, false, &module_public_names)?);

    let merged_imports = merge_imports(&blocks);
    let (code, source_map) = assemble_output(&merged_imports, &blocks);

    Ok(BundleOutput { code, source_map })
}

struct Project {
    root_dir: PathBuf,
    use_std: bool,
    files: HashMap<PathBuf, FileInfo>,
    visiting: HashSet<PathBuf>,
}

impl Project {
    fn new(root_dir: PathBuf, use_std: bool) -> Self {
        Self {
            root_dir,
            use_std,
            files: HashMap::new(),
            visiting: HashSet::new(),
        }
    }

    fn insert_entry(&mut self, path: PathBuf, ast: AstNode) -> io::Result<()> {
        let module_name = module_name_for_path(&path);
        let dir = path
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| self.root_dir.clone());
        let imports = collect_imports(&ast, &dir, &self.root_dir)?;
        self.files.insert(
            path.clone(),
            FileInfo {
                path,
                module_name,
                ast,
                imports,
            },
        );
        Ok(())
    }

    fn resolve_file(&mut self, path: &Path) -> io::Result<()> {
        if !self.files.contains_key(path) {
            self.load_file(path)?;
        }
        let dependencies = self
            .files
            .get(path)
            .map(|info| info.imports.dependencies())
            .unwrap_or_default();
        for dep in dependencies {
            self.resolve_file(&dep)?;
        }
        Ok(())
    }

    fn load_file(&mut self, path: &Path) -> io::Result<()> {
        let canonical = fs::canonicalize(path)?;
        if self.files.contains_key(&canonical) {
            return Ok(());
        }
        if !self.visiting.insert(canonical.clone()) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "Circular import detected while loading {}",
                    canonical.display()
                ),
            ));
        }
        let ast = parse_typepy_file(&canonical, self.use_std)?;
        let dir = canonical
            .parent()
            .map(|p| p.to_path_buf())
            .unwrap_or_else(|| self.root_dir.clone());
        let imports = collect_imports(&ast, &dir, &self.root_dir)?;
        self.files.insert(
            canonical.clone(),
            FileInfo {
                path: canonical.clone(),
                module_name: module_name_for_path(&canonical),
                ast,
                imports,
            },
        );
        self.visiting.remove(&canonical);
        Ok(())
    }

    fn module_order(&self, entry_path: &Path) -> Vec<PathBuf> {
        let mut visited = HashSet::new();
        let mut order = Vec::new();
        self.visit(entry_path, &mut visited, &mut order);
        order
            .into_iter()
            .filter(|p| p != entry_path)
            .collect::<Vec<_>>()
    }

    fn visit(&self, path: &Path, visited: &mut HashSet<PathBuf>, order: &mut Vec<PathBuf>) {
        if !visited.insert(path.to_path_buf()) {
            return;
        }
        if let Some(info) = self.files.get(path) {
            for dep in info.imports.dependencies() {
                self.visit(&dep, visited, order);
            }
        }
        order.push(path.to_path_buf());
    }
}

struct FileInfo {
    path: PathBuf,
    module_name: String,
    ast: AstNode,
    imports: ImportInfo,
}

#[derive(Default)]
struct ImportInfo {
    module_imports: Vec<ModuleImport>,
    from_imports: Vec<FromImport>,
}

impl ImportInfo {
    fn dependencies(&self) -> Vec<PathBuf> {
        let mut deps = HashSet::new();
        for item in &self.module_imports {
            deps.insert(item.path.clone());
        }
        for item in &self.from_imports {
            deps.insert(item.path.clone());
        }
        let mut list: Vec<_> = deps.into_iter().collect();
        list.sort();
        list
    }
}

struct ModuleImport {
    alias: String,
    path: PathBuf,
}

struct FromImport {
    alias: String,
    member: String,
    module_name: String,
    path: PathBuf,
}

struct BlockOutput {
    display_name: String,
    imports: Vec<String>,
    body: String,
    source_map: Vec<SourceMapEntry>,
}

fn parse_typepy_file(path: &Path, use_std: bool) -> io::Result<AstNode> {
    let source = Arc::from(fs::read_to_string(path)?);
    let mut lexer = Lexer::new(&source);
    let tokens = lexer.tokenize();
    let mut parser = parser::Parser::new(source.clone(), tokens);
    let parsed = parser
        .parse_file()
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, err.to_string()))?;
    let optimized = optimizer::optimize_ast(parsed);
    let mut analyzer = Analyzer::new(optimized, source.clone());
    if !use_std {
        analyzer.set_use_std(false);
    }
    analyzer
        .analyze()
        .map_err(|err| io::Error::new(io::ErrorKind::InvalidData, analyzer.format_error(&err)))?;
    Ok(analyzer.root_node.clone())
}

fn collect_imports(ast: &AstNode, file_dir: &Path, root_dir: &Path) -> io::Result<ImportInfo> {
    let mut info = ImportInfo::default();
    if let AstNode::File(items) = ast {
        for item in items {
            if let AstNode::Import { module, names } = item {
                match module {
                    Some(module_name) => {
                        if let Some(path) = resolve_module_path(module_name, file_dir, root_dir)? {
                            for import in names {
                                let alias = import
                                    .as_name
                                    .clone()
                                    .unwrap_or_else(|| import.name.clone());
                                info.from_imports.push(FromImport {
                                    alias,
                                    member: import.name.clone(),
                                    module_name: module_name.clone(),
                                    path: path.clone(),
                                });
                            }
                        }
                    }
                    None => {
                        for import in names {
                            let alias = import
                                .as_name
                                .clone()
                                .unwrap_or_else(|| import.name.clone());
                            if let Some(path) =
                                resolve_module_path(&import.name, file_dir, root_dir)?
                            {
                                info.module_imports.push(ModuleImport { alias, path });
                            }
                        }
                    }
                }
            }
        }
    }
    Ok(info)
}

fn resolve_module_path(
    module_name: &str,
    file_dir: &Path,
    root_dir: &Path,
) -> io::Result<Option<PathBuf>> {
    let mut candidates = Vec::new();
    candidates.push(file_dir.join(format!("{}.tpy", module_name)));
    if file_dir != root_dir {
        candidates.push(root_dir.join(format!("{}.tpy", module_name)));
    }
    for candidate in candidates {
        if candidate.exists() {
            return Ok(Some(fs::canonicalize(candidate)?));
        }
    }
    Ok(None)
}

fn module_name_for_path(path: &Path) -> String {
    path.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or_default()
        .to_string()
}

fn collect_exports(ast: &AstNode) -> HashSet<String> {
    let mut names = HashSet::new();
    if let AstNode::File(items) = ast {
        for item in items {
            match item {
                AstNode::FunctionDef { name, .. } => {
                    names.insert(name.clone());
                }
                AstNode::ClassDef { name, .. } => {
                    names.insert(name.clone());
                }
                AstNode::StructDef { name, .. } => {
                    names.insert(name.clone());
                }
                AstNode::EnumDef { name, .. } => {
                    names.insert(name.clone());
                }
                AstNode::VarDecl { name, .. } => {
                    names.insert(name.clone());
                }
                _ => {}
            }
        }
    }
    names
}

fn determine_public_names(
    info: &FileInfo,
    exports: HashSet<String>,
    used: &mut HashSet<String>,
) -> HashMap<String, String> {
    let mut names: Vec<_> = exports.into_iter().collect();
    names.sort();
    let mut map = HashMap::new();
    for name in names {
        let mut public = name.clone();
        if used.contains(&public) {
            let mut candidate = format!("{}__{}", info.module_name, name);
            let mut counter = 1usize;
            while used.contains(&candidate) {
                candidate = format!("{}__{}{}", info.module_name, name, counter);
                counter += 1;
            }
            public = candidate;
        }
        used.insert(public.clone());
        map.insert(name, public);
    }
    map
}

fn build_typepy_context(
    info: &FileInfo,
    module_public_names: &HashMap<PathBuf, HashMap<String, String>>,
    include_export_renames: bool,
) -> Option<TypePyTranspileContext> {
    let mut ctx = TypePyTranspileContext::default();
    for import in &info.imports.module_imports {
        if let Some(export_map) = module_public_names.get(import.path.as_path()) {
            ctx.module_aliases.insert(
                import.alias.clone(),
                TypePyModuleBinding {
                    member_map: export_map.clone(),
                },
            );
        }
    }
    for from_import in &info.imports.from_imports {
        if module_public_names.contains_key(&from_import.path) {
            ctx.from_import_modules
                .insert(from_import.module_name.clone());
        }
    }
    if include_export_renames {
        if let Some(exports) = module_public_names.get(info.path.as_path()) {
            for (original, public) in exports {
                if original != public {
                    ctx.export_renames.insert(original.clone(), public.clone());
                }
            }
        }
    }
    if ctx.module_aliases.is_empty()
        && ctx.from_import_modules.is_empty()
        && ctx.export_renames.is_empty()
    {
        None
    } else {
        Some(ctx)
    }
}

fn transpile_block(
    info: &FileInfo,
    include_export_aliases: bool,
    module_public_names: &HashMap<PathBuf, HashMap<String, String>>,
) -> io::Result<BlockOutput> {
    let ctx = build_typepy_context(info, module_public_names, include_export_aliases);
    let (code, map) = transpiler::transpile_with_context(&info.ast, ctx);
    let (imports, body_part, import_line_count) = split_imports_and_body(&code);

    let mut alias_lines: Vec<String> = Vec::new();

    let mut seen_alias = HashSet::new();
    for from_import in &info.imports.from_imports {
        if let Some(exports) = module_public_names.get(from_import.path.as_path()) {
            if let Some(public) = exports.get(&from_import.member) {
                if &from_import.alias != public && seen_alias.insert(from_import.alias.clone()) {
                    alias_lines.push(format!("{} = {}", from_import.alias, public));
                }
            }
        }
    }
    alias_lines.sort();

    let mut alias_block = String::new();
    if !alias_lines.is_empty() {
        alias_block.push_str(&alias_lines.join("\n"));
        alias_block.push('\n');
    }

    let mut body = String::new();
    body.push_str(&body_part);
    if !body.is_empty() && !body.ends_with('\n') {
        body.push('\n');
    }
    if !alias_block.is_empty() {
        if !body.is_empty() {
            body.push('\n');
        }
        body.push_str(&alias_block);
    }
    if !body.is_empty() && !body.ends_with('\n') {
        body.push('\n');
    }

    let mut adjusted_map = Vec::new();
    for entry in map {
        if entry.generated_line <= import_line_count {
            continue;
        }
        let mut updated = entry.clone();
        updated.generated_line -= import_line_count;
        if updated.original_file.is_none() {
            updated.original_file = Some(info.path.to_string_lossy().to_string());
        }
        adjusted_map.push(updated);
    }

    Ok(BlockOutput {
        display_name: info
            .path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&info.module_name)
            .to_string(),
        imports,
        body,
        source_map: adjusted_map,
    })
}

fn split_imports_and_body(code: &str) -> (Vec<String>, String, usize) {
    let mut imports = Vec::new();
    let mut body_lines = Vec::new();
    let mut import_line_count = 0usize;
    let mut in_import_section = true;

    for line in code.lines() {
        if in_import_section {
            if line.starts_with("import ") || line.starts_with("from ") {
                imports.push(line.to_string());
                import_line_count += 1;
                continue;
            } else if line.trim().is_empty() && imports.is_empty() {
                // skip leading blank lines before imports
                import_line_count += 1;
                continue;
            } else {
                in_import_section = false;
            }
        }
        body_lines.push(line.to_string());
    }

    let mut body = body_lines.join("\n");
    if !body.is_empty() && code.ends_with('\n') {
        body.push('\n');
    }

    (imports, body, import_line_count)
}

fn merge_imports(blocks: &[BlockOutput]) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut merged = Vec::new();
    for block in blocks {
        for line in &block.imports {
            if line.trim().is_empty() {
                continue;
            }
            if seen.insert(line.clone()) {
                merged.push(line.clone());
            }
        }
    }
    merged
}

fn assemble_output(imports: &[String], blocks: &[BlockOutput]) -> (String, Vec<SourceMapEntry>) {
    let mut code = String::new();
    let mut source_map = Vec::new();
    let mut current_line = 1usize;

    let path_guard_prelude = [
        "import os as __typepy_os",
        "import sys as __typepy_sys",
        "__typepy_dir = None",
        "if '__file__' in globals():",
        "    __typepy_dir = __typepy_os.path.abspath(__typepy_os.path.dirname(__file__))",
        "if __typepy_dir:",
        "    __typepy_sys.path = [",
        "        p for p in __typepy_sys.path",
        "        if __typepy_os.path.abspath(p) != __typepy_dir",
        "    ]",
        "    __typepy_sys.path.append(__typepy_dir)",
        "del __typepy_dir",
        "del __typepy_os",
        "del __typepy_sys",
    ];
    for line in &path_guard_prelude {
        code.push_str(line);
        code.push('\n');
        current_line += 1;
    }
    code.push('\n');
    current_line += 1;

    if !imports.is_empty() {
        for line in imports {
            code.push_str(line);
            code.push('\n');
            current_line += 1;
        }
        code.push('\n');
        current_line += 1;
    }

    for block in blocks {
        code.push_str(&format!("# {}\n", block.display_name));
        current_line += 1;
        let body_start_line = current_line;
        if !block.body.is_empty() {
            code.push_str(&block.body);
            current_line += block.body.lines().count();
        }
        code.push('\n');
        current_line += 1;
        for entry in &block.source_map {
            let mut updated = entry.clone();
            updated.generated_line += body_start_line - 1;
            source_map.push(updated);
        }
    }

    if !code.ends_with('\n') {
        code.push('\n');
    }

    (code, source_map)
}
