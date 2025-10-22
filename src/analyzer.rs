use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::Arc;

use crate::parser::AstNode;
use crate::parser::BinOp;
use crate::parser::CatchBlock;
use crate::parser::Decorator;
use crate::parser::Expr;
use crate::parser::LambdaBody;
use crate::parser::SwitchCase;
use crate::parser::VarDeclType;
use crate::parser::VarTypeField;
use crate::token::Span;
use serde::Serialize;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    Any,
    Unknown,
    None,
    Named(String),
    Array(Box<Type>),
    Dict(Box<Type>, Box<Type>),
    Tuple(Vec<Type>),
    Function(Vec<Type>, Box<Type>),
    Union(Vec<Type>),
    Optional(Box<Type>),
    IntLiteral,
    FloatLiteral,
    TypeObject(String),
    Range(Box<Type>),
}

impl Type {
    fn optional(inner: Type) -> Type {
        Type::Optional(Box::new(inner))
    }

    fn union(types: Vec<Type>) -> Type {
        let mut flat: Vec<Type> = Vec::new();
        for ty in types {
            match ty {
                Type::Union(items) => {
                    for item in items {
                        if !flat.contains(&item) {
                            flat.push(item);
                        }
                    }
                }
                other => {
                    if !flat.contains(&other) {
                        flat.push(other);
                    }
                }
            }
        }
        if flat.len() == 1 {
            flat.into_iter().next().unwrap()
        } else {
            Type::Union(flat)
        }
    }

    fn unwrap_optional(&self) -> Option<Type> {
        match self {
            Type::Optional(inner) => Some((**inner).clone()),
            _ => None,
        }
    }

    fn is_optional(&self) -> bool {
        matches!(self, Type::Optional(_))
    }

    fn as_function(&self) -> Option<(Vec<Type>, Type)> {
        match self {
            Type::Function(params, ret) => Some((params.clone(), (*ret.clone()))),
            _ => None,
        }
    }

    fn normalize_name(name: &str) -> String {
        match name {
            "str" => "string".to_string(),
            other => other.to_string(),
        }
    }

    pub fn describe(&self) -> String {
        match self {
            Type::Any => "Any".to_string(),
            Type::Unknown => "Unknown".to_string(),
            Type::None => "None".to_string(),
            Type::Named(name) => name.clone(),
            Type::Array(inner) => format!("[{}]", inner.describe()),
            Type::Dict(key, value) => format!("{{{}: {}}}", key.describe(), value.describe()),
            Type::Tuple(items) => {
                let parts: Vec<String> = items.iter().map(|t| t.describe()).collect();
                format!("({})", parts.join(", "))
            }
            Type::Function(params, ret) => {
                let params_str = params
                    .iter()
                    .map(|p| p.describe())
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("function({}) -> {}", params_str, ret.describe())
            }
            Type::Union(types) => types
                .iter()
                .map(|t| t.describe())
                .collect::<Vec<_>>()
                .join(" | "),
            Type::Optional(inner) => {
                let inner_repr = inner.describe();
                match inner.as_ref() {
                    Type::Union(_) | Type::Function(_, _) | Type::Tuple(_) => {
                        format!("({})?", inner_repr)
                    }
                    _ => format!("{}?", inner_repr),
                }
            }
            Type::IntLiteral => "int".to_string(),
            Type::FloatLiteral => "float".to_string(),
            Type::TypeObject(name) => format!("type {}", name),
            Type::Range(inner) => format!("Range<{}>", inner.describe()),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.describe())
    }
}

#[derive(Debug, Clone)]
pub struct FunctionInfo {
    pub params: Vec<Type>,
    pub return_type: Type,
    pub is_method: bool,
    pub is_static: bool,
}

#[derive(Debug, Clone)]
pub struct ClassInfo {
    pub name: String,
    pub instance_fields: HashMap<String, Type>,
    pub static_fields: HashMap<String, Type>,
    pub instance_methods: HashMap<String, FunctionInfo>,
    pub static_methods: HashMap<String, FunctionInfo>,
    pub constructor: Option<FunctionInfo>,
}

impl ClassInfo {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            instance_fields: HashMap::new(),
            static_fields: HashMap::new(),
            instance_methods: HashMap::new(),
            static_methods: HashMap::new(),
            constructor: None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub name: String,
    pub variants: HashSet<String>,
}

#[derive(Debug, Clone, Serialize)]
pub struct ParameterSummary {
    pub name: String,
    pub type_repr: String,
}

#[derive(Debug, Clone, Serialize)]
pub struct LocationSummary {
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
}

fn location_from_span(span: &Span) -> LocationSummary {
    LocationSummary {
        start_line: span.start.line.saturating_sub(1),
        start_column: span.start.column,
        end_line: span.end.line.saturating_sub(1),
        end_column: span.end.column,
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct FunctionSummary {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<String>,
    pub params: Vec<ParameterSummary>,
    pub return_type: String,
    pub is_method: bool,
    pub is_static: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<LocationSummary>,
}

#[derive(Debug, Clone, Serialize)]
pub struct VariableSummary {
    pub name: String,
    pub type_repr: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub container: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub is_static: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<LocationSummary>,
}

#[derive(Debug, Clone, Serialize)]
pub struct FieldSummary {
    pub name: String,
    pub type_repr: String,
    pub is_static: bool,
}

#[derive(Debug, Clone, Serialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum TypeSummary {
    Class {
        name: String,
        fields: Vec<FieldSummary>,
        #[serde(skip_serializing_if = "Option::is_none")]
        location: Option<LocationSummary>,
    },
    Struct {
        name: String,
        fields: Vec<FieldSummary>,
        #[serde(skip_serializing_if = "Option::is_none")]
        location: Option<LocationSummary>,
    },
    Enum {
        name: String,
        variants: Vec<String>,
        #[serde(skip_serializing_if = "Option::is_none")]
        location: Option<LocationSummary>,
    },
}

#[derive(Debug, Clone, Serialize, Default)]
pub struct AnalysisSummary {
    pub functions: Vec<FunctionSummary>,
    pub variables: Vec<VariableSummary>,
    pub types: Vec<TypeSummary>,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    Primitive,
    Class(ClassInfo),
    Struct(StructInfo),
    Enum(EnumInfo),
}

#[derive(Debug, Clone)]
pub enum SymbolValue {
    Variable { ty: Type, mutable: bool },
    Function(FunctionInfo),
    TypeName(String),
    EnumVariant { parent: String },
    Module(HashMap<String, SymbolValue>),
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub value: SymbolValue,
}

#[derive(Debug, Clone)]
pub struct AnalyzerError {
    pub message: String,
    pub span: Option<Span>,
}

impl AnalyzerError {
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }

    pub fn already_defined(name: &str, span: Option<Span>) -> Self {
        Self::new(format!("{} is already defined", name), span)
    }

    pub fn not_defined(name: &str, span: Option<Span>) -> Self {
        Self::new(format!("{} is not defined", name), span)
    }

    pub fn invalid_operation(message: impl Into<String>, span: Option<Span>) -> Self {
        Self::new(message, span)
    }

    pub fn format_with_source(&self, source: &str) -> String {
        if let Some(span) = self.span {
            let mut result = format!(
                "{} (line {}:{}, line {}:{})",
                self.message, span.start.line, span.start.column, span.end.line, span.end.column
            );
            if let Some(snippet) = span.snippet(source) {
                result.push('\n');
                result.push_str(&snippet);
            }
            result
        } else {
            self.message.clone()
        }
    }
}

struct AnalyzerContext {
    scopes: Vec<HashMap<String, Symbol>>,
    types: HashMap<String, TypeInfo>,
    enum_variants: HashMap<String, String>,
    current_return: Vec<Option<Type>>,
    summary: AnalysisSummary,
}

impl AnalyzerContext {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            types: HashMap::new(),
            enum_variants: HashMap::new(),
            current_return: Vec::new(),
            summary: AnalysisSummary::default(),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, symbol: Symbol) -> Result<(), AnalyzerError> {
        let scope = self.scopes.last_mut().unwrap();
        if scope.contains_key(&symbol.name) {
            return Err(AnalyzerError::already_defined(&symbol.name, None));
        }
        scope.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    fn redeclare(&mut self, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(symbol.name.clone(), symbol);
        }
    }

    fn assign(&mut self, name: &str, ty: Type, span: Option<Span>) -> Result<(), AnalyzerError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(symbol) = scope.get_mut(name) {
                match &mut symbol.value {
                    SymbolValue::Variable {
                        ty: existing,
                        mutable,
                    } => {
                        if !*mutable {
                            return Err(AnalyzerError::invalid_operation(
                                format!("{} is immutable", name),
                                span,
                            ));
                        }
                        *existing = ty;
                        return Ok(());
                    }
                    _ => {
                        return Err(AnalyzerError::invalid_operation(
                            format!("{} is not a variable", name),
                            span,
                        ));
                    }
                }
            }
        }
        Err(AnalyzerError::not_defined(name, span))
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn get_type(&self, name: &str) -> Option<&TypeInfo> {
        self.types.get(name)
    }

    fn insert_type(&mut self, name: String, info: TypeInfo) -> Result<(), AnalyzerError> {
        if self.types.contains_key(&name) {
            return Err(AnalyzerError::already_defined(&name, None));
        }
        self.types.insert(name.clone(), info);
        self.declare(Symbol {
            name: name.clone(),
            value: SymbolValue::TypeName(name),
        })?;
        Ok(())
    }

    fn is_global_scope(&self) -> bool {
        self.scopes.len() == 1
    }
}

pub struct Analyzer {
    pub root_node: AstNode,
    use_std: bool,
    source: Arc<str>,
}

impl Analyzer {
    pub fn new(root_node: AstNode, source: Arc<str>) -> Self {
        Self {
            root_node,
            use_std: true,
            source,
        }
    }

    pub fn with_std(root_node: AstNode, use_std: bool, source: Arc<str>) -> Self {
        Self {
            root_node,
            use_std,
            source,
        }
    }

    pub fn set_use_std(&mut self, use_std: bool) {
        self.use_std = use_std;
    }

    pub fn analyze(&self) -> Result<AnalysisSummary, AnalyzerError> {
        let mut ctx = AnalyzerContext::new();
        if self.use_std {
            self.install_std(&mut ctx);
        }
        self.analyze_node(&self.root_node, &mut ctx)?;
        Ok(ctx.summary)
    }

    pub fn format_error(&self, error: &AnalyzerError) -> String {
        error.format_with_source(&self.source)
    }

    fn analyze_node(&self, node: &AstNode, ctx: &mut AnalyzerContext) -> Result<(), AnalyzerError> {
        match node {
            AstNode::File(items) => {
                for item in items {
                    self.analyze_node(item, ctx)?;
                }
                Ok(())
            }
            AstNode::Import { module, names } => {
                if let Some(module_name) = module {
                    if let Some(std_module) =
                        ctx.lookup(module_name)
                            .and_then(|symbol| match &symbol.value {
                                SymbolValue::Module(map) => Some(map.clone()),
                                _ => None,
                            })
                    {
                        for import in names {
                            let key = import
                                .as_name
                                .clone()
                                .unwrap_or_else(|| import.name.clone());
                            if let Some(value) = std_module.get(&import.name).cloned() {
                                ctx.redeclare(Symbol { name: key, value });
                            } else {
                                ctx.redeclare(Symbol {
                                    name: key,
                                    value: SymbolValue::Module(HashMap::new()),
                                });
                            }
                        }
                        return Ok(());
                    }
                } else {
                    for import in names {
                        let alias = import
                            .as_name
                            .clone()
                            .unwrap_or_else(|| import.name.clone());
                        if let Some(symbol) = ctx.lookup(&import.name) {
                            match &symbol.value {
                                SymbolValue::Module(map) => {
                                    ctx.redeclare(Symbol {
                                        name: alias,
                                        value: SymbolValue::Module(map.clone()),
                                    });
                                }
                                other => {
                                    ctx.redeclare(Symbol {
                                        name: alias,
                                        value: other.clone(),
                                    });
                                }
                            }
                        } else {
                            ctx.redeclare(Symbol {
                                name: alias,
                                value: SymbolValue::Module(HashMap::new()),
                            });
                        }
                    }
                    return Ok(());
                }
                for import in names {
                    let key = import
                        .as_name
                        .clone()
                        .unwrap_or_else(|| import.name.clone());
                    ctx.redeclare(Symbol {
                        name: key,
                        value: SymbolValue::Module(HashMap::new()),
                    });
                }
                Ok(())
            }
            AstNode::ClassDef {
                name,
                members,
                span,
                ..
            } => {
                ctx.insert_type(name.clone(), TypeInfo::Class(ClassInfo::new(name)))?;
                let mut info = ClassInfo::new(name);
                let mut field_summaries: Vec<FieldSummary> = Vec::new();
                let mut method_summaries: Vec<FunctionSummary> = Vec::new();
                for member in members {
                    match member {
                        AstNode::VarDecl {
                            name: field_name,
                            var_type,
                            decl_type: _,
                            expr,
                            is_static,
                            ..
                        } => {
                            let field_type = if let Some(var_ty) = var_type {
                                self.type_from_field(var_ty)?
                            } else if let Some(expr) = expr {
                                self.type_from_expr(expr, ctx)?
                            } else {
                                Type::Unknown
                            };
                            let type_repr = field_type.describe();
                            if *is_static {
                                info.static_fields.insert(field_name.clone(), field_type);
                            } else {
                                info.instance_fields.insert(field_name.clone(), field_type);
                            }
                            field_summaries.push(FieldSummary {
                                name: field_name.clone(),
                                type_repr,
                                is_static: *is_static,
                            });
                        }
                        AstNode::FunctionDef {
                            name: fn_name,
                            params,
                            return_type,
                            body,
                            is_static,
                            span: method_span,
                            ..
                        } => {
                            let mut param_entries: Vec<(String, Type)> = Vec::new();
                            for (param_name, param_field) in params.iter() {
                                let param_ty = self.type_from_field(param_field)?;
                                param_entries.push((param_name.clone(), param_ty));
                            }
                            let param_types =
                                param_entries.iter().map(|(_, ty)| ty.clone()).collect();
                            let ret_type = if let Some(ret) = return_type {
                                self.type_from_field(ret)?
                            } else {
                                Type::None
                            };
                            ctx.push_scope();
                            if !*is_static {
                                ctx.declare(Symbol {
                                    name: "self".to_string(),
                                    value: SymbolValue::Variable {
                                        ty: Type::Named(name.clone()),
                                        mutable: true,
                                    },
                                })?;
                            }
                            for (index, (param_name, param_ty)) in param_entries.iter().enumerate()
                            {
                                if !*is_static && index == 0 && param_name == "self" {
                                    continue;
                                }
                                ctx.declare(Symbol {
                                    name: param_name.clone(),
                                    value: SymbolValue::Variable {
                                        ty: param_ty.clone(),
                                        mutable: true,
                                    },
                                })?;
                            }
                            ctx.current_return.push(Some(ret_type.clone()));
                            for stmt in body {
                                self.analyze_node(stmt, ctx)?;
                            }
                            ctx.current_return.pop();
                            ctx.pop_scope();
                            let fn_info = FunctionInfo {
                                params: param_types,
                                return_type: ret_type.clone(),
                                is_method: !*is_static,
                                is_static: *is_static,
                            };
                            if fn_name == "__init__" {
                                info.constructor = Some(fn_info.clone());
                            }
                            if *is_static {
                                info.static_methods.insert(fn_name.clone(), fn_info);
                            } else {
                                info.instance_methods.insert(fn_name.clone(), fn_info);
                            }
                            let display_params = param_entries
                                .iter()
                                .enumerate()
                                .filter_map(|(index, (param_name, param_ty))| {
                                    if !*is_static && index == 0 && param_name == "self" {
                                        None
                                    } else {
                                        Some(ParameterSummary {
                                            name: param_name.clone(),
                                            type_repr: param_ty.describe(),
                                        })
                                    }
                                })
                                .collect();
                            method_summaries.push(FunctionSummary {
                                name: fn_name.clone(),
                                container: Some(name.clone()),
                                params: display_params,
                                return_type: ret_type.describe(),
                                is_method: !*is_static,
                                is_static: *is_static,
                                location: Some(location_from_span(&method_span)),
                            });
                        }
                        AstNode::PassStmt => {}
                        _ => {
                            self.analyze_node(member, ctx)?;
                        }
                    }
                    ctx.types
                        .insert(name.clone(), TypeInfo::Class(info.clone()));
                }
                ctx.types.insert(name.clone(), TypeInfo::Class(info));
                ctx.summary.types.push(TypeSummary::Class {
                    name: name.clone(),
                    fields: field_summaries,
                    location: Some(location_from_span(&span)),
                });
                ctx.summary.functions.extend(method_summaries);
                Ok(())
            }
            AstNode::StructDef { name, fields, span } => {
                let mut struct_fields = Vec::new();
                for field in fields {
                    struct_fields.push((field.name.clone(), self.type_from_field(&field.ty)?));
                }
                let field_summaries: Vec<FieldSummary> = struct_fields
                    .iter()
                    .map(|(field_name, field_type)| FieldSummary {
                        name: field_name.clone(),
                        type_repr: field_type.describe(),
                        is_static: false,
                    })
                    .collect();
                let info = StructInfo {
                    name: name.clone(),
                    fields: struct_fields.clone(),
                };
                ctx.insert_type(name.clone(), TypeInfo::Struct(info))?;
                ctx.summary.types.push(TypeSummary::Struct {
                    name: name.clone(),
                    fields: field_summaries,
                    location: Some(location_from_span(&span)),
                });
                Ok(())
            }
            AstNode::EnumDef {
                name,
                variants,
                span,
            } => {
                let mut info = EnumInfo {
                    name: name.clone(),
                    variants: HashSet::new(),
                };
                let mut enum_variants = Vec::new();
                for variant in variants {
                    info.variants.insert(variant.clone());
                    ctx.enum_variants.insert(variant.clone(), name.clone());
                    enum_variants.push(variant.clone());
                }
                ctx.insert_type(name.clone(), TypeInfo::Enum(info))?;
                ctx.summary.types.push(TypeSummary::Enum {
                    name: name.clone(),
                    variants: enum_variants,
                    location: Some(location_from_span(&span)),
                });
                Ok(())
            }
            AstNode::FunctionDef {
                name,
                params,
                return_type,
                body,
                decorators,
                span,
                ..
            } => {
                let mut param_entries = Vec::new();
                for (param_name, ty) in params {
                    let param_ty = self.type_from_field(ty)?;
                    param_entries.push((param_name.clone(), param_ty));
                }
                let param_types = param_entries.iter().map(|(_, ty)| ty.clone()).collect();
                let ret_type = if let Some(ret) = return_type {
                    self.type_from_field(ret)?
                } else {
                    Type::None
                };
                let fn_info = FunctionInfo {
                    params: param_types,
                    return_type: ret_type.clone(),
                    is_method: false,
                    is_static: false,
                };
                ctx.declare(Symbol {
                    name: name.clone(),
                    value: SymbolValue::Function(fn_info.clone()),
                })?;
                ctx.push_scope();
                ctx.current_return.push(Some(ret_type.clone()));
                for (param_name, param_ty) in &param_entries {
                    ctx.declare(Symbol {
                        name: param_name.clone(),
                        value: SymbolValue::Variable {
                            ty: param_ty.clone(),
                            mutable: true,
                        },
                    })?;
                }
                for stmt in body {
                    self.analyze_node(stmt, ctx)?;
                }
                ctx.current_return.pop();
                ctx.pop_scope();
                if !decorators.is_empty() {
                    self.apply_decorators(name, decorators, false, false, ctx)?;
                }
                if ctx.is_global_scope() {
                    let params_summary = param_entries
                        .iter()
                        .map(|(param_name, param_ty)| ParameterSummary {
                            name: param_name.clone(),
                            type_repr: param_ty.describe(),
                        })
                        .collect();
                    ctx.summary.functions.push(FunctionSummary {
                        name: name.clone(),
                        container: None,
                        params: params_summary,
                        return_type: ret_type.describe(),
                        is_method: false,
                        is_static: false,
                        location: Some(location_from_span(&span)),
                    });
                }
                Ok(())
            }
            AstNode::VarDecl {
                name,
                var_type,
                expr,
                decl_type,
                span,
                ..
            } => {
                let ty = if let Some(var_ty) = var_type {
                    self.type_from_field(var_ty)?
                } else if let Some(expr) = expr {
                    self.type_from_expr(expr, ctx)?
                } else {
                    Type::Unknown
                };
                if ctx.is_global_scope() {
                    let type_repr = ty.describe();
                    ctx.summary.variables.push(VariableSummary {
                        name: name.clone(),
                        type_repr,
                        container: None,
                        is_static: None,
                        location: Some(location_from_span(&span)),
                    });
                }
                ctx.redeclare(Symbol {
                    name: name.clone(),
                    value: SymbolValue::Variable {
                        ty,
                        mutable: *decl_type == VarDeclType::Let,
                    },
                });
                Ok(())
            }
            AstNode::ExprStmt(expr) => {
                self.type_from_expr(expr, ctx)?;
                Ok(())
            }
            AstNode::ReturnStmt(expr) => {
                if let Some(expected) = ctx.current_return.last().cloned().flatten() {
                    if let Some(value_expr) = expr {
                        let _ty = self.type_from_expr(value_expr, ctx)?;
                        let _ = expected;
                    }
                }
                Ok(())
            }
            AstNode::SwitchStmt {
                expr,
                cases,
                default,
            } => {
                let switch_ty = self.type_from_expr(expr, ctx)?;
                for case in cases {
                    self.analyze_switch_case(case, &switch_ty, ctx)?;
                }
                if let Some(default_body) = default {
                    ctx.push_scope();
                    for stmt in default_body {
                        self.analyze_node(stmt, ctx)?;
                    }
                    ctx.pop_scope();
                }
                Ok(())
            }
            AstNode::IfStmt {
                condition,
                body,
                else_body,
            } => {
                self.type_from_expr(condition, ctx)?;
                ctx.push_scope();
                for stmt in body {
                    self.analyze_node(stmt, ctx)?;
                }
                ctx.pop_scope();
                if let Some(else_block) = else_body {
                    ctx.push_scope();
                    for stmt in else_block {
                        self.analyze_node(stmt, ctx)?;
                    }
                    ctx.pop_scope();
                }
                Ok(())
            }
            AstNode::GuardStmt {
                condition,
                else_body,
            } => {
                match condition.as_ref() {
                    Expr::VarDecl { name, expr, .. } => {
                        let condition_ty = self.type_from_expr(expr, ctx)?;
                        if let Some(inner) = condition_ty.unwrap_optional() {
                            ctx.redeclare(Symbol {
                                name: name.clone(),
                                value: SymbolValue::Variable {
                                    ty: inner,
                                    mutable: false,
                                },
                            });
                        }
                    }
                    other => {
                        self.type_from_expr(other, ctx)?;
                    }
                }
                if let Some(else_block) = else_body {
                    ctx.push_scope();
                    for stmt in else_block {
                        self.analyze_node(stmt, ctx)?;
                    }
                    ctx.pop_scope();
                }
                Ok(())
            }
            AstNode::ForStmt {
                var_name,
                iterable,
                body,
            } => {
                let iter_ty = self.type_from_expr(iterable, ctx)?;
                ctx.push_scope();
                ctx.declare(Symbol {
                    name: var_name.clone(),
                    value: SymbolValue::Variable {
                        ty: match iter_ty {
                            Type::Array(inner) => (*inner).clone(),
                            Type::Range(inner) => (*inner).clone(),
                            _ => Type::Unknown,
                        },
                        mutable: true,
                    },
                })?;
                for stmt in body {
                    self.analyze_node(stmt, ctx)?;
                }
                ctx.pop_scope();
                Ok(())
            }
            AstNode::TryStmt {
                body,
                catch_blocks,
                finally_block,
            } => {
                ctx.push_scope();
                for stmt in body {
                    self.analyze_node(stmt, ctx)?;
                }
                ctx.pop_scope();
                for catch in catch_blocks {
                    self.analyze_catch_block(catch, ctx)?;
                }
                if let Some(finally_body) = finally_block {
                    ctx.push_scope();
                    for stmt in finally_body {
                        self.analyze_node(stmt, ctx)?;
                    }
                    ctx.pop_scope();
                }
                Ok(())
            }
            AstNode::ThrowStmt(expr) => {
                self.type_from_expr(expr, ctx)?;
                Ok(())
            }
            AstNode::PassStmt => Ok(()),
            AstNode::BreakStmt => Ok(()),
            AstNode::ContinueStmt => Ok(()),
        }
    }

    fn apply_decorators(
        &self,
        function_name: &str,
        decorators: &[Decorator],
        is_method: bool,
        is_static: bool,
        ctx: &mut AnalyzerContext,
    ) -> Result<(), AnalyzerError> {
        for decorator in decorators.iter().rev() {
            let callee_expr = self.decorator_to_expr(&decorator.name);
            let decorator_call = if decorator.args.is_empty() {
                callee_expr
            } else {
                Expr::Call {
                    callee: Box::new(callee_expr),
                    args: decorator.args.clone(),
                }
            };
            let call_expr = Expr::Call {
                callee: Box::new(decorator_call),
                args: vec![Expr::Identifier {
                    name: function_name.to_string(),
                    span: Span::default(),
                }],
            };
            let ty = self.type_from_expr(&call_expr, ctx)?;
            let value = match ty {
                Type::Function(params, ret) => SymbolValue::Function(FunctionInfo {
                    params,
                    return_type: *ret,
                    is_method,
                    is_static,
                }),
                other => SymbolValue::Variable {
                    ty: other,
                    mutable: false,
                },
            };
            ctx.redeclare(Symbol {
                name: function_name.to_string(),
                value,
            });
        }
        Ok(())
    }

    fn decorator_to_expr(&self, name: &str) -> Expr {
        let mut parts = name.split('.');
        if let Some(first) = parts.next() {
            let mut expr = Expr::Identifier {
                name: first.to_string(),
                span: Span::default(),
            };
            for part in parts {
                expr = Expr::MemberAccess {
                    target: Some(Box::new(expr)),
                    member: part.to_string(),
                    span: Span::default(),
                };
            }
            expr
        } else {
            Expr::Identifier {
                name: name.to_string(),
                span: Span::default(),
            }
        }
    }

    fn analyze_switch_case(
        &self,
        case: &SwitchCase,
        switch_ty: &Type,
        ctx: &mut AnalyzerContext,
    ) -> Result<(), AnalyzerError> {
        ctx.push_scope();
        if let Type::Named(enum_name) = switch_ty {
            if let Some(TypeInfo::Enum(enum_info)) = ctx.get_type(enum_name) {
                if !enum_info.variants.contains(&case.pattern) {
                    return Err(AnalyzerError::not_defined(&case.pattern, None));
                }
            }
        }
        for stmt in &case.body {
            self.analyze_node(stmt, ctx)?;
        }
        ctx.pop_scope();
        Ok(())
    }

    fn analyze_catch_block(
        &self,
        catch: &CatchBlock,
        ctx: &mut AnalyzerContext,
    ) -> Result<(), AnalyzerError> {
        ctx.push_scope();
        if let Some(name) = &catch.exception_name {
            ctx.declare(Symbol {
                name: name.clone(),
                value: SymbolValue::Variable {
                    ty: Type::Unknown,
                    mutable: true,
                },
            })?;
        }
        for stmt in &catch.body {
            self.analyze_node(stmt, ctx)?;
        }
        ctx.pop_scope();
        Ok(())
    }

    fn type_from_field(&self, field: &VarTypeField) -> Result<Type, AnalyzerError> {
        let base = match field.name.as_str() {
            "array" => {
                let inner = self.type_from_field(&field.args[0])?;
                Type::Array(Box::new(inner))
            }
            "dict" => {
                let key = self.type_from_field(&field.args[0])?;
                let value = self.type_from_field(&field.args[1])?;
                Type::Dict(Box::new(key), Box::new(value))
            }
            "tuple" => {
                let mut items = Vec::new();
                for arg in &field.args {
                    items.push(self.type_from_field(arg)?);
                }
                Type::Tuple(items)
            }
            "function" => {
                let mut args = Vec::new();
                for arg in &field.args[..field.args.len() - 1] {
                    args.push(self.type_from_field(arg)?);
                }
                let ret = self.type_from_field(&field.args[field.args.len() - 1])?;
                Type::Function(args, Box::new(ret))
            }
            "union" => {
                let mut args = Vec::new();
                for arg in &field.args {
                    args.push(self.type_from_field(arg)?);
                }
                Type::union(args)
            }
            "None" => Type::None,
            other => Type::Named(Type::normalize_name(other)),
        };
        if field.is_optional {
            Ok(Type::optional(base))
        } else {
            Ok(base)
        }
    }

    fn type_from_expr(
        &self,
        expr: &Expr,
        ctx: &mut AnalyzerContext,
    ) -> Result<Type, AnalyzerError> {
        match expr {
            Expr::Identifier { name, span } => {
                if let Some(symbol) = ctx.lookup(name) {
                    match &symbol.value {
                        SymbolValue::Variable { ty, .. } => Ok(ty.clone()),
                        SymbolValue::Function(info) => Ok(Type::Function(
                            info.params.clone(),
                            Box::new(info.return_type.clone()),
                        )),
                        SymbolValue::TypeName(type_name) => Ok(Type::TypeObject(type_name.clone())),
                        SymbolValue::EnumVariant { parent } => Ok(Type::Named(parent.clone())),
                        SymbolValue::Module(_) => Ok(Type::Any),
                    }
                } else {
                    Err(AnalyzerError::not_defined(name, Some(*span)))
                }
            }
            Expr::IntLiteral(_) => Ok(Type::IntLiteral),
            Expr::FloatLiteral(_) => Ok(Type::FloatLiteral),
            Expr::StringLiteral { .. } => Ok(Type::Named("string".to_string())),
            Expr::BoolLiteral(_) => Ok(Type::Named("bool".to_string())),
            Expr::NoneLiteral => Ok(Type::None),
            Expr::Binary { left, op, right } => {
                let _ = self.type_from_expr(left, ctx)?;
                let _ = self.type_from_expr(right, ctx)?;
                match op {
                    BinOp::Equal
                    | BinOp::NotEqual
                    | BinOp::LessThan
                    | BinOp::LessThanOrEqual
                    | BinOp::GreaterThan
                    | BinOp::GreaterThanOrEqual
                    | BinOp::And
                    | BinOp::Or => Ok(Type::Named("bool".to_string())),
                    BinOp::RangeClosed | BinOp::RangeHalfOpen => {
                        let inner = Type::Named("int64".to_string());
                        Ok(Type::Range(Box::new(inner)))
                    }
                    _ => Ok(Type::Named("int64".to_string())),
                }
            }
            Expr::Unary { expr, .. } => self.type_from_expr(expr, ctx),
            Expr::Call { callee, args } => {
                let callee_ty = self.type_from_expr(callee, ctx)?;
                match callee_ty {
                    Type::Function(params, ret) => {
                        let _ = params;
                        for arg in args {
                            self.type_from_expr(arg, ctx)?;
                        }
                        Ok(*ret)
                    }
                    Type::TypeObject(name) => {
                        if let Some(info) = ctx.get_type(&name).cloned() {
                            match info {
                                TypeInfo::Class(class_info) => {
                                    for arg in args {
                                        self.type_from_expr(arg, ctx)?;
                                    }
                                    Ok(Type::Named(class_info.name))
                                }
                                TypeInfo::Struct(struct_info) => {
                                    for arg in args {
                                        self.type_from_expr(arg, ctx)?;
                                    }
                                    Ok(Type::Named(struct_info.name))
                                }
                                _ => Ok(Type::Any),
                            }
                        } else {
                            Ok(Type::Any)
                        }
                    }
                    _ => Ok(Type::Any),
                }
            }
            Expr::Function { params, body } => {
                ctx.push_scope();
                let result = (|| {
                    let mut param_types = Vec::new();
                    for (param_name, param_ty) in params.iter() {
                        let ty = self.type_from_field(param_ty)?;
                        param_types.push(ty.clone());
                        ctx.declare(Symbol {
                            name: param_name.clone(),
                            value: SymbolValue::Variable { ty, mutable: true },
                        })?;
                    }
                    for stmt in body {
                        self.analyze_node(stmt, ctx)?;
                    }
                    Ok(Type::Function(param_types, Box::new(Type::None)))
                })();
                ctx.pop_scope();
                result
            }
            Expr::Lambda { params, body } => {
                ctx.push_scope();
                let result = (|| {
                    let mut param_types = Vec::new();
                    for (param_name, param_ty) in params.iter() {
                        let ty = self.type_from_field(param_ty)?;
                        param_types.push(ty.clone());
                        ctx.declare(Symbol {
                            name: param_name.clone(),
                            value: SymbolValue::Variable { ty, mutable: true },
                        })?;
                    }
                    let mut return_ty = Type::None;
                    match body {
                        LambdaBody::Expr(expr) => {
                            return_ty = self.type_from_expr(expr, ctx)?;
                        }
                        LambdaBody::Block(block) => {
                            for stmt in block {
                                self.analyze_node(stmt, ctx)?;
                            }
                        }
                    }
                    Ok(Type::Function(param_types, Box::new(return_ty)))
                })();
                ctx.pop_scope();
                result
            }
            Expr::VarDecl { name, expr, .. } => {
                let ty = self.type_from_expr(expr, ctx)?;
                ctx.redeclare(Symbol {
                    name: name.clone(),
                    value: SymbolValue::Variable {
                        ty: ty.clone(),
                        mutable: true,
                    },
                });
                Ok(ty)
            }
            Expr::Assign { target, value } => {
                let value_ty = self.type_from_expr(value, ctx)?;
                match &**target {
                    Expr::Identifier { name, span } => {
                        ctx.assign(name, value_ty.clone(), Some(*span))?;
                        Ok(value_ty)
                    }
                    _ => Ok(value_ty),
                }
            }
            Expr::MemberAccess {
                target,
                member,
                span,
            } => {
                if let Some(inner) = target {
                    let base_ty = self.type_from_expr(inner, ctx)?;
                    self.resolve_member_access(base_ty, member, *span, ctx)
                } else {
                    if let Some(parent) = ctx.enum_variants.get(member) {
                        return Ok(Type::Named(parent.clone()));
                    }
                    Err(AnalyzerError::not_defined(member, Some(*span)))
                }
            }
            Expr::CompoundAssign { target, value, .. } => {
                let value_ty = self.type_from_expr(value, ctx)?;
                match &**target {
                    Expr::Identifier { name, span } => {
                        ctx.assign(name, value_ty.clone(), Some(*span))?;
                    }
                    _ => {}
                }
                Ok(value_ty)
            }
            Expr::ArrayLiteral(items) => {
                if let Some(first) = items.first() {
                    let inner_ty = self.type_from_expr(first, ctx)?;
                    for item in items.iter().skip(1) {
                        let _ = self.type_from_expr(item, ctx)?;
                    }
                    Ok(Type::Array(Box::new(inner_ty)))
                } else {
                    Ok(Type::Array(Box::new(Type::Unknown)))
                }
            }
            Expr::DictLiteral(items) => {
                if let Some((key, value)) = items.first() {
                    let key_ty = self.type_from_expr(key, ctx)?;
                    let value_ty = self.type_from_expr(value, ctx)?;
                    Ok(Type::Dict(Box::new(key_ty), Box::new(value_ty)))
                } else {
                    Ok(Type::Dict(Box::new(Type::Unknown), Box::new(Type::Unknown)))
                }
            }
            Expr::TupleLiteral(items) => {
                let mut types = Vec::new();
                for item in items {
                    types.push(self.type_from_expr(item, ctx)?);
                }
                Ok(Type::Tuple(types))
            }
            Expr::Range { start, end, .. } => {
                self.type_from_expr(start, ctx)?;
                self.type_from_expr(end, ctx)?;
                Ok(Type::Range(Box::new(Type::Named("int64".to_string()))))
            }
            Expr::Index { target, index } => {
                let base_ty = self.type_from_expr(target, ctx)?;
                self.type_from_expr(index, ctx)?;
                match base_ty {
                    Type::Array(inner) => Ok(*inner),
                    Type::Dict(_, value) => Ok(*value),
                    _ => Ok(Type::Unknown),
                }
            }
            Expr::Unwrap { target } => {
                let base = self.type_from_expr(target, ctx)?;
                if let Some(inner) = base.unwrap_optional() {
                    Ok(inner)
                } else {
                    Ok(base)
                }
            }
            Expr::OptionalChaining { target } => {
                let base = self.type_from_expr(target, ctx)?;
                Ok(base)
            }
            Expr::NullCoalescing { left, right } => {
                let left_ty = self.type_from_expr(left, ctx)?;
                let right_ty = self.type_from_expr(right, ctx)?;
                Ok(Type::union(vec![left_ty, right_ty]))
            }
        }
    }

    fn resolve_member_access(
        &self,
        base_ty: Type,
        member: &str,
        span: Span,
        ctx: &mut AnalyzerContext,
    ) -> Result<Type, AnalyzerError> {
        match base_ty {
            Type::TypeObject(name) => {
                if let Some(TypeInfo::Class(info)) = ctx.get_type(&name) {
                    if let Some(field_ty) = info.static_fields.get(member) {
                        return Ok(field_ty.clone());
                    }
                    if let Some(method) = info.static_methods.get(member) {
                        return Ok(Type::Function(
                            method.params.clone(),
                            Box::new(method.return_type.clone()),
                        ));
                    }
                }
                Err(AnalyzerError::not_defined(
                    &format!("{}.{}", name, member),
                    Some(span),
                ))
            }
            Type::Named(name) => {
                if let Some(TypeInfo::Class(info)) = ctx.get_type(&name) {
                    if let Some(field_ty) = info.instance_fields.get(member) {
                        return Ok(field_ty.clone());
                    }
                    if let Some(method) = info.instance_methods.get(member) {
                        return Ok(Type::Function(
                            method.params.clone(),
                            Box::new(method.return_type.clone()),
                        ));
                    }
                }
                if let Some(TypeInfo::Struct(info)) = ctx.get_type(&name) {
                    for (field_name, ty) in &info.fields {
                        if field_name == member {
                            return Ok(ty.clone());
                        }
                    }
                }
                if name == "string" {
                    return match member {
                        "upper" | "lower" | "trim" => Ok(Type::Function(
                            Vec::new(),
                            Box::new(Type::Named("string".to_string())),
                        )),
                        "length" => Ok(Type::Named("uint64".to_string())),
                        _ => Err(AnalyzerError::not_defined(
                            &format!("{}.{}", name, member),
                            Some(span),
                        )),
                    };
                }
                Err(AnalyzerError::not_defined(
                    &format!("{}.{}", name, member),
                    Some(span),
                ))
            }
            Type::Array(inner) => match member {
                "length" => Ok(Type::Named("uint64".to_string())),
                "append" | "removeAt" => {
                    Ok(Type::Function(vec![(*inner).clone()], Box::new(Type::None)))
                }
                _ => Err(AnalyzerError::not_defined(member, Some(span))),
            },
            Type::Dict(_, value) => match member {
                "get" => Ok(Type::Function(vec![Type::Unknown], Box::new(*value))),
                _ => Err(AnalyzerError::not_defined(member, Some(span))),
            },
            Type::Optional(inner) => self.resolve_member_access(*inner, member, span, ctx),
            Type::Any | Type::Unknown => Ok(Type::Any),
            _ => Err(AnalyzerError::not_defined(member, Some(span))),
        }
    }

    fn install_std(&self, ctx: &mut AnalyzerContext) {
        let primitives = vec![
            "int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "float32",
            "float64", "bool", "string",
        ];
        for primitive in primitives {
            ctx.types.insert(primitive.to_string(), TypeInfo::Primitive);
        }
        ctx.declare(Symbol {
            name: "print".to_string(),
            value: SymbolValue::Function(FunctionInfo {
                params: vec![Type::Any],
                return_type: Type::None,
                is_method: false,
                is_static: false,
            }),
        })
        .ok();
        ctx.declare(Symbol {
            name: "exit".to_string(),
            value: SymbolValue::Function(FunctionInfo {
                params: vec![Type::Named("int32".to_string())],
                return_type: Type::None,
                is_method: false,
                is_static: false,
            }),
        })
        .ok();

        let mut random_module = HashMap::new();
        random_module.insert(
            "random".to_string(),
            SymbolValue::Function(FunctionInfo {
                params: Vec::new(),
                return_type: Type::Named("float64".to_string()),
                is_method: false,
                is_static: false,
            }),
        );
        ctx.declare(Symbol {
            name: "random".to_string(),
            value: SymbolValue::Module(random_module),
        })
        .ok();

        let view_fn_type = Type::Function(Vec::new(), Box::new(Type::Named("string".to_string())));
        let route_return =
            Type::Function(vec![view_fn_type.clone()], Box::new(view_fn_type.clone()));
        let mut app_module = HashMap::new();
        app_module.insert(
            "route".to_string(),
            SymbolValue::Function(FunctionInfo {
                params: vec![Type::Named("string".to_string())],
                return_type: route_return,
                is_method: false,
                is_static: false,
            }),
        );
        ctx.declare(Symbol {
            name: "app".to_string(),
            value: SymbolValue::Module(app_module),
        })
        .ok();

        let mut test_module = HashMap::new();
        test_module.insert(
            "test".to_string(),
            SymbolValue::Function(FunctionInfo {
                params: Vec::new(),
                return_type: Type::None,
                is_method: false,
                is_static: false,
            }),
        );
        test_module.insert(
            "test2".to_string(),
            SymbolValue::Function(FunctionInfo {
                params: Vec::new(),
                return_type: Type::None,
                is_method: false,
                is_static: false,
            }),
        );
        ctx.declare(Symbol {
            name: "test".to_string(),
            value: SymbolValue::Module(test_module),
        })
        .ok();

        ctx.types.insert(
            "ZeroDivisionError".to_string(),
            TypeInfo::Class(ClassInfo::new("ZeroDivisionError")),
        );
        ctx.declare(Symbol {
            name: "ZeroDivisionError".to_string(),
            value: SymbolValue::TypeName("ZeroDivisionError".to_string()),
        })
        .ok();
        ctx.types.insert(
            "Exception".to_string(),
            TypeInfo::Class(ClassInfo::new("Exception")),
        );
        ctx.declare(Symbol {
            name: "Exception".to_string(),
            value: SymbolValue::TypeName("Exception".to_string()),
        })
        .ok();
    }
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::path::Path;
    use std::sync::Arc;

    use crate::analyzer::{Analyzer, AnalyzerError};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn analyze_source(source: &str) -> Result<(), AnalyzerError> {
        let source_arc: Arc<str> = Arc::from(source);
        let mut lexer = Lexer::new(&source_arc);
        let tokens = lexer.tokenize();
        let mut parser = Parser::new(source_arc.clone(), tokens);
        let ast = parser
            .parse_file()
            .map_err(|e| AnalyzerError::new(e.to_string(), Some(e.span)))?;
        let analyzer = Analyzer::new(ast, source_arc.clone());
        analyzer.analyze().map(|_| ())
    }

    #[test]
    fn examples_should_analyze_successfully() {
        let example_dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("example");
        let expected_failures = ["control_flow.tpy"];
        for entry in fs::read_dir(example_dir).expect("read example dir") {
            let entry = entry.expect("entry");
            if !entry.file_type().expect("file type").is_file() {
                continue;
            }
            if entry.path().extension().and_then(|s| s.to_str()) != Some("tpy") {
                continue;
            }
            let file_name = entry
                .path()
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or_default()
                .to_string();
            let content = fs::read_to_string(entry.path()).expect("read example");
            if expected_failures.contains(&file_name.as_str()) {
                let err = analyze_source(&content).expect_err("analysis unexpectedly succeeded");
                let formatted = err.format_with_source(&content);
                assert!(
                    formatted.contains("line"),
                    "expected location info in error: {}",
                    formatted
                );
                continue;
            }
            analyze_source(&content).unwrap_or_else(|err| {
                panic!(
                    "analysis failed for {:?}: {}",
                    entry.path(),
                    err.format_with_source(&content)
                );
            });
        }
    }

    #[test]
    fn reports_undefined_variable() {
        let source = "def broken() -> uint32 { return missing; }";
        let result = analyze_source(source);
        assert!(result.is_err());
        let message = result
            .err()
            .map(|err| err.format_with_source(source))
            .unwrap();
        assert!(message.contains("missing is not defined"));
        assert!(message.contains("line 1:"));
    }
}
