use std::collections::HashMap;

use crate::parser::{AstNode, Expr, BinOp, VarTypeField};
use crate::analyzer::Symbol;

pub struct TypeChecker<'a> {
    globals: &'a HashMap<String, Symbol>,
    scopes: Vec<HashMap<String, VarTypeField>>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(globals: &'a HashMap<String, Symbol>) -> Self {
        Self { globals, scopes: vec![HashMap::new()] }
    }

    pub fn check(&mut self, node: &AstNode) -> Result<(), String> {
        self.check_node(node)
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, VarTypeField> {
        self.scopes.last_mut().unwrap()
    }

    fn with_new_scope<F: FnOnce(&mut Self) -> Result<(), String>>(
        &mut self,
        f: F,
    ) -> Result<(), String> {
        self.scopes.push(HashMap::new());
        let result = f(self);
        self.scopes.pop();
        result
    }

    fn check_node(&mut self, node: &AstNode) -> Result<(), String> {
        match node {
            AstNode::File(nodes) => {
                for n in nodes {
                    self.check_node(n)?;
                }
                Ok(())
            }
            AstNode::FunctionDef { params, return_type, body, .. } => {
                self.with_new_scope(|checker| {
                    for (name, ty) in params {
                        checker.current_scope_mut().insert(name.clone(), ty.clone());
                    }
                    for stmt in body {
                        checker.check_node(stmt)?;
                    }
                    // check return expressions are of correct type if provided
                    if let Some(ret_ty) = return_type {
                        // naive: ignore actual return statements for now
                        checker.current_scope_mut().insert("__return__".to_string(), ret_ty.clone());
                    }
                    Ok(())
                })
            }
            AstNode::VarDecl { name, var_type, expr, .. } => {
                if let Some(e) = expr {
                    let expr_ty = self.infer_expr(e)?;
                    let ty = var_type.as_ref().unwrap_or(&expr_ty);
                    if var_type.is_some() && !self.types_equal(ty, &expr_ty) {
                        return Err(format!("Type mismatch for {}: expected {:?}, got {:?}", name, ty, expr_ty));
                    }
                    self.current_scope_mut().insert(name.clone(), ty.clone());
                }
                Ok(())
            }
            AstNode::ExprStmt(expr) => {
                self.infer_expr(expr).map(|_| ())
            }
            AstNode::ReturnStmt(opt) => {
                if let Some(e) = opt {
                    self.infer_expr(e)?;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }

    fn infer_expr(&mut self, expr: &Expr) -> Result<VarTypeField, String> {
        match expr {
            Expr::IntLiteral(_) => Ok(self.simple_type("int64")),
            Expr::FloatLiteral(_) => Ok(self.simple_type("float64")),
            Expr::StringLiteral { .. } => Ok(self.simple_type("string")),
            Expr::BoolLiteral(_) => Ok(self.simple_type("bool")),
            Expr::Identifier(name) => {
                // search from inner scope to outer
                for scope in self.scopes.iter().rev() {
                    if let Some(t) = scope.get(name) {
                        return Ok(t.clone());
                    }
                }
                if let Some(sym) = self.globals.get(name) {
                    return Ok(sym.var_type.clone());
                }
                Err(format!("Undefined identifier {}", name))
            }
            Expr::Binary { left, op, right } => {
                let left_ty = self.infer_expr(left)?;
                let right_ty = self.infer_expr(right)?;
                match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Slash | BinOp::Power => {
                        if left_ty.name == "int64" && right_ty.name == "int64" {
                            Ok(self.simple_type("int64"))
                        } else if left_ty.name == "float64" && right_ty.name == "float64" {
                            Ok(self.simple_type("float64"))
                        } else {
                            Err(format!("Type mismatch: {:?} vs {:?}", left_ty, right_ty))
                        }
                    }
                    BinOp::Equal | BinOp::NotEqual | BinOp::LessThan | BinOp::LessThanOrEqual |
                    BinOp::GreaterThan | BinOp::GreaterThanOrEqual => Ok(self.simple_type("bool")),
                    _ => Err("Unsupported binary operation".to_string()),
                }
            }
            Expr::Assign { target, value } => {
                let value_ty = self.infer_expr(value)?;
                if let Expr::Identifier(name) = &**target {
                    // find variable in scopes
                    for scope in self.scopes.iter().rev() {
                        if let Some(t) = scope.get(name) {
                            if !self.types_equal(t, &value_ty) {
                                return Err(format!("Type mismatch in assignment to {}", name));
                            }
                            return Ok(t.clone());
                        }
                    }
                    if let Some(sym) = self.globals.get(name) {
                        if !self.types_equal(&sym.var_type, &value_ty) {
                            return Err(format!("Type mismatch in assignment to {}", name));
                        }
                        return Ok(sym.var_type.clone());
                    }
                    Err(format!("Undefined identifier {}", name))
                } else {
                    Err("Left side of assignment must be identifier".to_string())
                }
            }
            Expr::Call { callee, args } => {
                let func_ty = self.infer_expr(callee)?;
                if func_ty.name != "function" {
                    return Err("Attempt to call non-function".to_string());
                }
                if func_ty.args.len() != args.len() + 1 {
                    return Err("Argument count mismatch".to_string());
                }
                for (arg_expr, expected_ty) in args.iter().zip(&func_ty.args) {
                    let ty = self.infer_expr(arg_expr)?;
                    if !self.types_equal(&ty, expected_ty) {
                        return Err("Argument type mismatch".to_string());
                    }
                }
                Ok(func_ty.args.last().unwrap().clone())
            }
            _ => Err("Unsupported expression".to_string()),
        }
    }

    fn simple_type(&self, name: &str) -> VarTypeField {
        VarTypeField { name: name.to_string(), is_optional: false, args: vec![] }
    }

    fn types_equal(&self, a: &VarTypeField, b: &VarTypeField) -> bool {
        a == b
    }
}

