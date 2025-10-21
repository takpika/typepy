use std::collections::{HashMap, HashSet};

use crate::parser::{
    AstNode, BinOp, CatchBlock, Expr, LambdaBody, StringLiteralType, SwitchCase, VarTypeField,
};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum ContextKind {
    Module,
    Class,
    Function,
}

pub struct PythonTranspiler {
    indent_level: usize,
    typing_imports: HashSet<String>,
    needs_dataclass: bool,
    needs_enum: bool,
    enum_variants: HashMap<String, String>,
    temp_counter: usize,
    context_stack: Vec<ContextKind>,
    needs_unwrap_helper: bool,
}

impl PythonTranspiler {
    pub fn new() -> Self {
        Self {
            indent_level: 0,
            typing_imports: HashSet::new(),
            needs_dataclass: false,
            needs_enum: false,
            enum_variants: HashMap::new(),
            temp_counter: 0,
            context_stack: vec![ContextKind::Module],
            needs_unwrap_helper: false,
        }
    }

    fn indent(&self) -> String {
        "    ".repeat(self.indent_level)
    }

    fn push_context(&mut self, ctx: ContextKind) {
        self.context_stack.push(ctx);
    }

    fn pop_context(&mut self) {
        self.context_stack.pop();
    }

    fn current_context(&self) -> ContextKind {
        *self.context_stack.last().unwrap_or(&ContextKind::Module)
    }

    fn new_temp(&mut self) -> String {
        self.temp_counter += 1;
        format!("__temp{}", self.temp_counter)
    }

    pub fn transpile(&mut self, node: &AstNode) -> String {
        let body = if let AstNode::File(items) = node {
            self.transpile_statements(items)
        } else {
            self.transpile_statement(node)
        };
        self.assemble_file(body)
    }

    fn assemble_file(&self, body: String) -> String {
        let mut lines: Vec<String> = Vec::new();

        if self.needs_dataclass {
            lines.push("from dataclasses import dataclass".to_string());
        }
        if self.needs_enum {
            lines.push("from enum import Enum".to_string());
        }
        if !self.typing_imports.is_empty() {
            let mut imports: Vec<_> = self.typing_imports.iter().cloned().collect();
            imports.sort();
            lines.push(format!("from typing import {}", imports.join(", ")));
        }

        let mut sections: Vec<String> = Vec::new();
        if !lines.is_empty() {
            sections.push(lines.join("\n"));
        }

        if self.needs_unwrap_helper {
            sections.push(
                "def __unwrap(value):\n    if value is None:\n        raise ValueError(\"Tried to unwrap None\")\n    return value"
                    .to_string(),
            );
        }

        let mut result = String::new();
        if !sections.is_empty() {
            result.push_str(&sections.join("\n\n"));
            result.push_str("\n\n");
        }
        result.push_str(body.trim_end());
        result.push('\n');
        result
    }

    fn transpile_statements(&mut self, nodes: &[AstNode]) -> String {
        let mut result = String::new();
        for node in nodes {
            result.push_str(&self.transpile_statement(node));
        }
        result
    }

    fn transpile_statement(&mut self, node: &AstNode) -> String {
        match node {
            AstNode::Import { module, names } => {
                let indent = self.indent();
                let joined = names
                    .iter()
                    .map(|item| match &item.as_name {
                        Some(alias) => format!("{} as {}", item.name, alias),
                        None => item.name.clone(),
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                if let Some(module_name) = module {
                    format!("{}from {} import {}\n", indent, module_name, joined)
                } else {
                    format!("{}import {}\n", indent, joined)
                }
            }
            AstNode::ClassDef {
                name,
                members,
                parents,
            } => {
                let indent = self.indent();
                let parent_str = if parents.is_empty() {
                    String::new()
                } else {
                    format!("({})", parents.join(", "))
                };
                let mut result = format!("{}class {}{}:\n", indent, name, parent_str);
                self.indent_level += 1;
                self.push_context(ContextKind::Class);
                let body = self.transpile_statements(members);
                if body.trim().is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    result.push_str(&body);
                }
                self.pop_context();
                self.indent_level -= 1;
                result
            }
            AstNode::StructDef { name, fields } => {
                self.needs_dataclass = true;
                let indent = self.indent();
                let mut result = format!("{}@dataclass\n{}class {}:\n", indent, indent, name);
                self.indent_level += 1;
                if fields.is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    for field in fields {
                        let field_indent = self.indent();
                        let ty = self.render_type(&field.ty);
                        let mut line = format!("{}{}", field_indent, field.name);
                        if let Some(ty_str) = ty {
                            line.push_str(&format!(": {}", ty_str));
                        }
                        if field.ty.is_optional {
                            line.push_str(" = None");
                        }
                        line.push('\n');
                        result.push_str(&line);
                    }
                }
                self.indent_level -= 1;
                result
            }
            AstNode::FunctionDef {
                name,
                params,
                return_type,
                body,
                is_static,
                is_private,
                decorators,
            } => {
                let indent = self.indent();
                let mut result = String::new();
                if self.current_context() == ContextKind::Class && *is_static {
                    result.push_str(&format!("{}@staticmethod\n", indent));
                }
                for decorator in decorators {
                    let args = decorator
                        .args
                        .iter()
                        .map(|expr| self.transpile_expr(expr))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if args.is_empty() {
                        result.push_str(&format!("{}@{}\n", indent, decorator.name));
                    } else {
                        result.push_str(&format!("{}@{}({})\n", indent, decorator.name, args));
                    }
                }
                let mut python_name = name.clone();
                if *is_private && !python_name.starts_with('_') {
                    python_name = format!("_{}", python_name);
                }
                let mut param_strings = Vec::new();
                let in_class = self.current_context() == ContextKind::Class;
                let is_method = in_class && !*is_static;
                let has_explicit_self = is_method
                    && params
                        .first()
                        .map(|(name, _)| name == "self")
                        .unwrap_or(false);
                if is_method && !has_explicit_self {
                    param_strings.push("self".to_string());
                }
                for (param_name, ty) in params.iter() {
                    if let Some(ty_str) = self.render_type(ty) {
                        param_strings.push(format!("{}: {}", param_name, ty_str));
                    } else {
                        param_strings.push(param_name.clone());
                    }
                }
                let params_joined = param_strings.join(", ");
                let return_annotation = return_type
                    .as_ref()
                    .and_then(|ty| self.render_type(ty))
                    .map(|ty| format!(" -> {}", ty))
                    .unwrap_or_default();
                result.push_str(&format!(
                    "{}def {}({}){}:\n",
                    indent, python_name, params_joined, return_annotation
                ));
                self.indent_level += 1;
                self.push_context(ContextKind::Function);
                let body_str = self.transpile_statements(body);
                if body_str.trim().is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    result.push_str(&body_str);
                }
                self.pop_context();
                self.indent_level -= 1;
                result
            }
            AstNode::VarDecl {
                name,
                var_type,
                expr,
                ..
            } => {
                if let Some(init_expr) = expr {
                    if let Some(definition) =
                        self.maybe_emit_named_function_literal(name, init_expr)
                    {
                        return definition;
                    }
                }
                let indent = self.indent();
                let mut line = format!("{}{}", indent, name);
                let expr_str = expr.as_ref().map(|e| self.transpile_expr(e));
                if let Some(ty) = var_type.as_ref().and_then(|ty| self.render_type(ty)) {
                    if let Some(expr_value) = expr_str {
                        line.push_str(&format!(": {} = {}\n", ty, expr_value));
                    } else {
                        line.push_str(&format!(": {}\n", ty));
                    }
                } else if let Some(expr_value) = expr_str {
                    line.push_str(&format!(" = {}\n", expr_value));
                } else {
                    line.push_str(" = None\n");
                }
                line
            }
            AstNode::EnumDef { name, variants } => {
                self.needs_enum = true;
                for variant in variants {
                    self.enum_variants.insert(variant.clone(), name.clone());
                }
                let indent = self.indent();
                let mut result = format!("{}class {}(Enum):\n", indent, name);
                self.indent_level += 1;
                if variants.is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    for variant in variants {
                        let variant_indent = self.indent();
                        result.push_str(&format!(
                            "{}{} = \"{}\"\n",
                            variant_indent, variant, variant
                        ));
                    }
                }
                self.indent_level -= 1;
                result
            }
            AstNode::ExprStmt(expr) => match expr {
                Expr::StringLiteral { .. } => {
                    let indent = self.indent();
                    format!("{}{}\n", indent, self.transpile_expr(expr))
                }
                Expr::CompoundAssign { op, target, value } => {
                    let indent = self.indent();
                    let op_str = self.binary_op_symbol(op);
                    let target_str = self.transpile_expr(target);
                    let value_str = self.transpile_expr(value);
                    format!("{}{} {}= {}\n", indent, target_str, op_str, value_str)
                }
                _ => {
                    if let Expr::Assign { target, value } = expr {
                        if let Expr::Identifier(name) = target.as_ref() {
                            if let Some(definition) =
                                self.maybe_emit_named_function_literal(name, value.as_ref())
                            {
                                return definition;
                            }
                        }
                    }
                    let indent = self.indent();
                    let expr_str = self.transpile_expr(expr);
                    format!("{}{}\n", indent, expr_str)
                }
            },
            AstNode::ReturnStmt(expr) => {
                let indent = self.indent();
                match expr {
                    Some(Expr::Function { params, body }) => {
                        let func_name = self.new_temp();
                        let mut result =
                            self.emit_function_literal_definition(&func_name, params, body);
                        result.push_str(&format!("{}return {}\n", indent, func_name));
                        result
                    }
                    Some(
                        value @ Expr::Lambda {
                            params,
                            body: LambdaBody::Block(block),
                        },
                    ) => {
                        if self.block_to_expression(block).is_none() {
                            let func_name = self.new_temp();
                            let mut result =
                                self.emit_function_literal_definition(&func_name, params, block);
                            result.push_str(&format!("{}return {}\n", indent, func_name));
                            result
                        } else {
                            format!("{}return {}\n", indent, self.transpile_expr(value))
                        }
                    }
                    Some(value) => format!("{}return {}\n", indent, self.transpile_expr(value)),
                    None => format!("{}return\n", indent),
                }
            }
            AstNode::SwitchStmt {
                expr,
                cases,
                default,
            } => {
                let indent = self.indent();
                let mut result = String::new();
                let expr_str = self.transpile_expr(expr);
                if cases.is_empty() && default.is_none() {
                    result.push_str(&format!("{}pass\n", indent));
                    return result;
                }
                for (index, case) in cases.iter().enumerate() {
                    let keyword = if index == 0 { "if" } else { "elif" };
                    let pattern = self.render_switch_pattern(case);
                    result.push_str(&format!(
                        "{}{} {} == {}:\n",
                        indent, keyword, expr_str, pattern
                    ));
                    self.indent_level += 1;
                    let body = self.transpile_statements(&case.body);
                    if body.trim().is_empty() {
                        result.push_str(&format!("{}pass\n", self.indent()));
                    } else {
                        result.push_str(&body);
                    }
                    self.indent_level -= 1;
                }
                if let Some(default_body) = default {
                    result.push_str(&format!("{}else:\n", indent));
                    self.indent_level += 1;
                    let body = self.transpile_statements(default_body);
                    if body.trim().is_empty() {
                        result.push_str(&format!("{}pass\n", self.indent()));
                    } else {
                        result.push_str(&body);
                    }
                    self.indent_level -= 1;
                }
                result
            }
            AstNode::IfStmt {
                condition,
                body,
                else_body,
            } => {
                let indent = self.indent();
                let cond_str = self.render_condition(condition);
                let mut result = format!("{}if {}:\n", indent, cond_str);
                self.indent_level += 1;
                let body_str = self.transpile_statements(body);
                if body_str.trim().is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    result.push_str(&body_str);
                }
                self.indent_level -= 1;
                if let Some(else_nodes) = else_body {
                    result.push_str(&format!("{}else:\n", indent));
                    self.indent_level += 1;
                    let else_str = self.transpile_statements(else_nodes);
                    if else_str.trim().is_empty() {
                        result.push_str(&format!("{}pass\n", self.indent()));
                    } else {
                        result.push_str(&else_str);
                    }
                    self.indent_level -= 1;
                }
                result
            }
            AstNode::GuardStmt {
                condition,
                else_body,
            } => {
                let indent = self.indent();
                let mut result = String::new();
                match &**condition {
                    Expr::VarDecl { name, expr, .. } => {
                        let value = self.transpile_expr(expr);
                        result.push_str(&format!("{}{} = {}\n", indent, name, value));
                        result.push_str(&format!("{}if {} is None:\n", indent, name));
                    }
                    other => {
                        let cond = self.transpile_expr(other);
                        result.push_str(&format!("{}if not ({}):\n", indent, cond));
                    }
                }
                self.indent_level += 1;
                if let Some(else_nodes) = else_body {
                    let else_str = self.transpile_statements(else_nodes);
                    if else_str.trim().is_empty() {
                        result.push_str(&format!("{}pass\n", self.indent()));
                    } else {
                        result.push_str(&else_str);
                    }
                } else {
                    result.push_str(&format!("{}pass\n", self.indent()));
                }
                self.indent_level -= 1;
                result
            }
            AstNode::ForStmt {
                var_name,
                iterable,
                body,
            } => {
                let indent = self.indent();
                let iter_str = self.transpile_expr(iterable);
                let mut result = format!("{}for {} in {}:\n", indent, var_name, iter_str);
                self.indent_level += 1;
                let body_str = self.transpile_statements(body);
                if body_str.trim().is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    result.push_str(&body_str);
                }
                self.indent_level -= 1;
                result
            }
            AstNode::TryStmt {
                body,
                catch_blocks,
                finally_block,
            } => {
                let indent = self.indent();
                let mut result = format!("{}try:\n", indent);
                self.indent_level += 1;
                let body_str = self.transpile_statements(body);
                if body_str.trim().is_empty() {
                    result.push_str(&format!("{}pass\n", self.indent()));
                } else {
                    result.push_str(&body_str);
                }
                self.indent_level -= 1;

                for catch in catch_blocks {
                    result.push_str(&self.transpile_catch_block(catch));
                }

                if let Some(finally_body) = finally_block {
                    result.push_str(&format!("{}finally:\n", indent));
                    self.indent_level += 1;
                    let body_str = self.transpile_statements(finally_body);
                    if body_str.trim().is_empty() {
                        result.push_str(&format!("{}pass\n", self.indent()));
                    } else {
                        result.push_str(&body_str);
                    }
                    self.indent_level -= 1;
                }
                result
            }
            AstNode::ThrowStmt(expr) => {
                let indent = self.indent();
                let value = self.transpile_expr(expr);
                format!("{}raise {}\n", indent, value)
            }
            AstNode::PassStmt => format!("{}pass\n", self.indent()),
            AstNode::BreakStmt => format!("{}break\n", self.indent()),
            AstNode::ContinueStmt => format!("{}continue\n", self.indent()),
            AstNode::File(items) => self.transpile_statements(items),
        }
    }

    fn transpile_catch_block(&mut self, block: &CatchBlock) -> String {
        let indent = self.indent();
        let mut exception_ty = block
            .exception_type
            .as_ref()
            .and_then(|ty| self.render_type(ty))
            .unwrap_or_else(|| "Exception".to_string());
        if exception_ty.starts_with('"') && exception_ty.ends_with('"') && exception_ty.len() > 1 {
            exception_ty = exception_ty[1..exception_ty.len() - 1].to_string();
        }
        let handler = match &block.exception_name {
            Some(name) => format!("{} as {}", exception_ty, name),
            None => exception_ty,
        };
        let mut result = format!("{}except {}:\n", indent, handler);
        self.indent_level += 1;
        let body = self.transpile_statements(&block.body);
        if body.trim().is_empty() {
            result.push_str(&format!("{}pass\n", self.indent()));
        } else {
            result.push_str(&body);
        }
        self.indent_level -= 1;
        result
    }

    fn render_condition(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::VarDecl { name, expr, .. } => {
                let value = self.transpile_expr(expr);
                format!("({} := {}) is not None", name, value)
            }
            other => self.transpile_expr(other),
        }
    }

    fn render_switch_pattern(&mut self, case: &SwitchCase) -> String {
        if let Some(enum_name) = self.enum_variants.get(&case.pattern) {
            format!("{}.{}", enum_name, case.pattern)
        } else {
            format!("\"{}\"", case.pattern)
        }
    }

    fn render_type(&mut self, ty: &VarTypeField) -> Option<String> {
        let mut rendered = match ty.name.as_str() {
            "int8" | "int16" | "int32" | "int64" | "uint8" | "uint16" | "uint32" | "uint64" => {
                Some("int".to_string())
            }
            "float32" | "float64" => Some("float".to_string()),
            "bool" => Some("bool".to_string()),
            "string" | "str" => Some("str".to_string()),
            "None" => Some("None".to_string()),
            "Any" | "any" => {
                self.typing_imports.insert("Any".to_string());
                Some("Any".to_string())
            }
            "array" => {
                if let Some(inner) = ty.args.first() {
                    if let Some(inner_ty) = self.render_type(inner) {
                        self.typing_imports.insert("List".to_string());
                        Some(format!("List[{}]", inner_ty))
                    } else {
                        self.typing_imports.insert("List".to_string());
                        Some("List[Any]".to_string())
                    }
                } else {
                    self.typing_imports.insert("List".to_string());
                    Some("List[Any]".to_string())
                }
            }
            "dict" => {
                let key = ty
                    .args
                    .get(0)
                    .and_then(|k| self.render_type(k))
                    .unwrap_or_else(|| {
                        self.typing_imports.insert("Any".to_string());
                        "Any".to_string()
                    });
                let value = ty
                    .args
                    .get(1)
                    .and_then(|v| self.render_type(v))
                    .unwrap_or_else(|| {
                        self.typing_imports.insert("Any".to_string());
                        "Any".to_string()
                    });
                self.typing_imports.insert("Dict".to_string());
                Some(format!("Dict[{}, {}]", key, value))
            }
            "tuple" => {
                let mut parts = Vec::new();
                for arg in &ty.args {
                    parts.push(self.render_type(arg).unwrap_or_else(|| {
                        self.typing_imports.insert("Any".to_string());
                        "Any".to_string()
                    }));
                }
                if parts.is_empty() {
                    None
                } else {
                    self.typing_imports.insert("Tuple".to_string());
                    Some(format!("Tuple[{}]", parts.join(", ")))
                }
            }
            "union" => {
                let mut parts = Vec::new();
                for arg in &ty.args {
                    parts.push(self.render_type(arg).unwrap_or_else(|| {
                        self.typing_imports.insert("Any".to_string());
                        "Any".to_string()
                    }));
                }
                self.typing_imports.insert("Union".to_string());
                Some(format!("Union[{}]", parts.join(", ")))
            }
            "function" => {
                let mut params = Vec::new();
                let mut return_ty = "Any".to_string();
                if !ty.args.is_empty() {
                    let last = ty.args.len() - 1;
                    for arg in &ty.args[..last] {
                        params.push(self.render_type(arg).unwrap_or_else(|| {
                            self.typing_imports.insert("Any".to_string());
                            "Any".to_string()
                        }));
                    }
                    return_ty = self.render_type(&ty.args[last]).unwrap_or_else(|| {
                        self.typing_imports.insert("Any".to_string());
                        "Any".to_string()
                    });
                }
                self.typing_imports.insert("Callable".to_string());
                let params_str = if params.is_empty() {
                    "[]".to_string()
                } else {
                    format!("[{}]", params.join(", "))
                };
                Some(format!("Callable[{}, {}]", params_str, return_ty))
            }
            "parent" => None,
            other => {
                if other
                    .chars()
                    .next()
                    .map(|c| c.is_uppercase())
                    .unwrap_or(false)
                {
                    Some(format!("\"{}\"", other))
                } else {
                    Some(other.to_string())
                }
            }
        };

        if let Some(ref mut ty_str) = rendered {
            if ty.is_optional && ty_str != "None" {
                self.typing_imports.insert("Optional".to_string());
                *ty_str = format!("Optional[{}]", ty_str);
            }
        }

        rendered
    }

    fn transpile_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Identifier(name) => name.clone(),
            Expr::IntLiteral(value) => value.to_string(),
            Expr::FloatLiteral(value) => {
                let mut s = value.to_string();
                if !s.contains('.') {
                    s.push_str(".0");
                }
                s
            }
            Expr::StringLiteral { value, ty, .. } => self.render_string(value, ty),
            Expr::BoolLiteral(value) => {
                if *value {
                    "True".to_string()
                } else {
                    "False".to_string()
                }
            }
            Expr::Call { callee, args } => {
                if let Expr::MemberAccess {
                    target: Some(target),
                    member,
                } = callee.as_ref()
                {
                    if let Expr::OptionalChaining { target: inner } = target.as_ref() {
                        let base = self.transpile_expr(inner.as_ref());
                        let temp = self.new_temp();
                        let args_str = args
                            .iter()
                            .map(|arg| self.transpile_expr(arg))
                            .collect::<Vec<_>>()
                            .join(", ");
                        return format!(
                            "(lambda {t}: {t}.{m}({a}) if {t} is not None else None)({b})",
                            t = temp,
                            m = member,
                            a = args_str,
                            b = base
                        );
                    }
                }
                let callee_str = self.transpile_expr(callee);
                let args_str = args
                    .iter()
                    .map(|arg| self.transpile_expr(arg))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", callee_str, args_str)
            }
            Expr::Function { params, body } => {
                if let Some(body_expr) = self.block_to_expression(body) {
                    let params_str = params
                        .iter()
                        .map(|(name, _)| name.clone())
                        .collect::<Vec<_>>()
                        .join(", ");
                    return format!("(lambda {}: {})", params_str, body_expr);
                }
                "(lambda *args, **kwargs: None)".to_string()
            }
            Expr::Lambda { params, body } => {
                let params_str = params
                    .iter()
                    .map(|(name, _)| name.clone())
                    .collect::<Vec<_>>()
                    .join(", ");
                match body {
                    LambdaBody::Expr(expr) => {
                        let expr_str = self.transpile_expr(expr);
                        format!("(lambda {}: {})", params_str, expr_str)
                    }
                    LambdaBody::Block(block) => {
                        if let Some(expr_str) = self.block_to_expression(block) {
                            format!("(lambda {}: {})", params_str, expr_str)
                        } else {
                            "(lambda *args, **kwargs: None)".to_string()
                        }
                    }
                }
            }
            Expr::Binary { left, op, right } => {
                let left_str = self.transpile_expr(left);
                let right_str = self.transpile_expr(right);
                let op_str = self.binary_op_symbol(op);
                format!("({} {} {})", left_str, op_str, right_str)
            }
            Expr::Unary { op, expr } => {
                let inner = self.transpile_expr(expr);
                match op {
                    crate::parser::UnaryOp::Negate => format!("(-{})", inner),
                    crate::parser::UnaryOp::Not => format!("(not {})", inner),
                }
            }
            Expr::VarDecl { name, expr, .. } => {
                let value = self.transpile_expr(expr);
                format!("({} := {})", name, value)
            }
            Expr::Assign { target, value } => {
                let target_str = self.transpile_expr(target);
                let value_str = self.transpile_expr(value);
                format!("{} = {}", target_str, value_str)
            }
            Expr::MemberAccess { target, member } => {
                if let Some(target_expr) = target {
                    if let Expr::OptionalChaining { target: inner } = target_expr.as_ref() {
                        let base = self.transpile_expr(inner.as_ref());
                        let temp = self.new_temp();
                        return format!(
                            "(lambda {t}: None if {t} is None else {t}.{m})({b})",
                            t = temp,
                            m = member,
                            b = base
                        );
                    }
                    let target_str = self.transpile_expr(target_expr);
                    format!("{}.{}", target_str, member)
                } else if let Some(enum_name) = self.enum_variants.get(member) {
                    format!("{}.{}", enum_name, member)
                } else {
                    member.clone()
                }
            }
            Expr::CompoundAssign { op, target, value } => {
                let op_str = self.binary_op_symbol(op);
                let target_str = self.transpile_expr(target);
                let value_str = self.transpile_expr(value);
                format!("{} {}= {}", target_str, op_str, value_str)
            }
            Expr::ArrayLiteral(items) => {
                let values = items
                    .iter()
                    .map(|item| self.transpile_expr(item))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", values)
            }
            Expr::DictLiteral(items) => {
                let entries = items
                    .iter()
                    .map(|(key, value)| {
                        let k = self.transpile_expr(key);
                        let v = self.transpile_expr(value);
                        format!("{}: {}", k, v)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{{{}}}", entries)
            }
            Expr::TupleLiteral(items) => {
                let parts = items
                    .iter()
                    .map(|item| self.transpile_expr(item))
                    .collect::<Vec<_>>();
                if parts.len() == 1 {
                    format!("({},)", parts[0])
                } else {
                    format!("({})", parts.join(", "))
                }
            }
            Expr::NoneLiteral => "None".to_string(),
            Expr::Range { start, end, op } => {
                let start_str = self.transpile_expr(start);
                let end_str = self.transpile_expr(end);
                match op {
                    BinOp::RangeHalfOpen => format!("range({}, {})", start_str, end_str),
                    BinOp::RangeClosed => format!("range({}, {} + 1)", start_str, end_str),
                    _ => format!("range({}, {})", start_str, end_str),
                }
            }
            Expr::Index { target, index } => {
                if let Expr::OptionalChaining { target: inner } = target.as_ref() {
                    let base = self.transpile_expr(inner.as_ref());
                    let idx = self.transpile_expr(index);
                    let temp = self.new_temp();
                    return format!(
                        "(lambda {t}: None if {t} is None else {t}[{i}])({b})",
                        t = temp,
                        i = idx,
                        b = base
                    );
                }
                let target_str = self.transpile_expr(target.as_ref());
                let index_str = self.transpile_expr(index);
                format!("{}[{}]", target_str, index_str)
            }
            Expr::Unwrap { target } => {
                self.needs_unwrap_helper = true;
                let value = self.transpile_expr(target.as_ref());
                format!("__unwrap({})", value)
            }
            Expr::OptionalChaining { target } => {
                let base = self.transpile_expr(target.as_ref());
                let temp = self.new_temp();
                format!(
                    "(lambda {t}: {t} if {t} is not None else None)({b})",
                    t = temp,
                    b = base
                )
            }
            Expr::NullCoalescing { left, right } => {
                let left_expr = self.transpile_expr(left);
                let right_expr = self.transpile_expr(right);
                let temp = self.new_temp();
                format!(
                    "(lambda {t}: {t} if {t} is not None else {r})({l})",
                    t = temp,
                    r = right_expr,
                    l = left_expr
                )
            }
        }
    }

    fn emit_function_literal_definition(
        &mut self,
        function_name: &str,
        params: &[(String, VarTypeField)],
        body: &[AstNode],
    ) -> String {
        let indent = self.indent();
        let mut param_strings = Vec::new();
        for (param_name, ty) in params.iter() {
            if let Some(ty_str) = self.render_type(ty) {
                param_strings.push(format!("{}: {}", param_name, ty_str));
            } else {
                param_strings.push(param_name.clone());
            }
        }
        let params_joined = param_strings.join(", ");
        let mut result = format!("{}def {}({}):\n", indent, function_name, params_joined);
        self.indent_level += 1;
        self.push_context(ContextKind::Function);
        let body_str = self.transpile_statements(body);
        if body_str.trim().is_empty() {
            result.push_str(&format!("{}pass\n", self.indent()));
        } else {
            result.push_str(&body_str);
        }
        self.pop_context();
        self.indent_level -= 1;
        result
    }

    fn maybe_emit_named_function_literal(
        &mut self,
        function_name: &str,
        expr: &Expr,
    ) -> Option<String> {
        match expr {
            Expr::Function { params, body } => {
                if self.block_to_expression(body).is_none() {
                    Some(self.emit_function_literal_definition(function_name, params, body))
                } else {
                    None
                }
            }
            Expr::Lambda { params, body } => match body {
                LambdaBody::Expr(_) => None,
                LambdaBody::Block(block) => {
                    if self.block_to_expression(block).is_none() {
                        Some(self.emit_function_literal_definition(function_name, params, block))
                    } else {
                        None
                    }
                }
            },
            _ => None,
        }
    }

    fn binary_op_symbol(&self, op: &BinOp) -> &'static str {
        match op {
            BinOp::Plus => "+",
            BinOp::Minus => "-",
            BinOp::Star => "*",
            BinOp::Slash => "/",
            BinOp::Percent => "%",
            BinOp::Power => "**",
            BinOp::Equal => "==",
            BinOp::NotEqual => "!=",
            BinOp::LessThan => "<",
            BinOp::LessThanOrEqual => "<=",
            BinOp::GreaterThan => ">",
            BinOp::GreaterThanOrEqual => ">=",
            BinOp::And => "and",
            BinOp::Or => "or",
            BinOp::Not => "not",
            BinOp::RangeHalfOpen | BinOp::RangeClosed => "..",
        }
    }

    fn block_to_expression(&mut self, block: &[AstNode]) -> Option<String> {
        if block.len() == 1 {
            match &block[0] {
                AstNode::ReturnStmt(Some(expr)) => Some(self.transpile_expr(expr)),
                AstNode::ExprStmt(expr) => Some(self.transpile_expr(expr)),
                _ => None,
            }
        } else {
            None
        }
    }

    fn render_string(&mut self, value: &str, ty: &StringLiteralType) -> String {
        let escaped = value.replace('\\', "\\\\");
        match ty {
            StringLiteralType::Format => {
                if value.contains('\n') {
                    format!("f\"\"\"{}\"\"\"", value)
                } else {
                    format!("f\"{}\"", escaped.replace('"', "\\\""))
                }
            }
            StringLiteralType::Raw | StringLiteralType::Regex => {
                if value.contains('\n') {
                    format!("r\"\"\"{}\"\"\"", value)
                } else {
                    format!("r\"{}\"", escaped.replace('"', "\\\""))
                }
            }
            StringLiteralType::Regular => {
                if value.contains('\n') {
                    format!("\"\"\"{}\"\"\"", value)
                } else {
                    format!("\"{}\"", escaped.replace('"', "\\\""))
                }
            }
        }
    }
}

pub fn transpile_to_python(ast: &AstNode) -> String {
    let mut transpiler = PythonTranspiler::new();
    transpiler.transpile(ast)
}
