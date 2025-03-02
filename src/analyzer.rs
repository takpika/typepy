use std::collections::HashMap;

use crate::parser::AstNode;
use crate::parser::BinOp;
use crate::parser::Expr;
use crate::parser::VarDeclType;
use crate::parser::VarTypeField;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub var_type: VarTypeField,
    pub read_only: bool,
    pub children: HashMap<String, Symbol>,
}

pub struct Analyzer {
    pub root_node: AstNode,
}

#[derive(Debug)]
pub enum AnalyzerError {
    AlreadyDefined(String),
    NotDefined(String),
    NotImplemented,
    NotType(String),
    NotFunction(String),
    FailedToDetermineType,
    TypeNotMatched {
        left: String,
        right: String,
    },
}

impl Analyzer {
    pub fn new(root_node: AstNode) -> Self {
        Self { root_node }
    }

    pub fn analyze(&mut self) -> Result<(), String> {
        let mut symbols = HashMap::new();
        match self.analyze_node(&self.root_node, &mut symbols) {
            Ok(Some(symbol)) => {
                symbols.insert(symbol.name.clone(), symbol);
            }
            Ok(None) => {}
            Err(e) => {
                return Err(format!("{:?}", e));
            }
        }
        Ok(())
    }

    fn analyze_node(&self, node: &AstNode, symbols: &mut HashMap<String, Symbol>) -> Result<Option<Symbol>, AnalyzerError> {
        let mut this_symbols = symbols.clone();
        match node {
            AstNode::File(nodes) => {
                for n in nodes {
                    match self.analyze_node(n, &mut this_symbols) {
                        Ok(Some(symbol)) => {
                            if this_symbols.contains_key(&symbol.name) {
                                return Err(AnalyzerError::AlreadyDefined(symbol.name));
                            }
                            this_symbols.insert(symbol.name.clone(), symbol);
                        }
                        Ok(None) => {}
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(None)
            },
            AstNode::ClassDef { name, members, parents } => {
                let mut children_symbols = HashMap::new();
                for m in members {
                    match self.analyze_node(m, &mut this_symbols) {
                        Ok(Some(symbol)) => {
                            let symbol_name = symbol.name.clone();
                            if this_symbols.contains_key(&symbol_name) {
                                return Err(AnalyzerError::AlreadyDefined(symbol_name));
                            }
                            this_symbols.insert(symbol_name.clone(), symbol.clone());
                            children_symbols.insert(symbol_name, symbol);
                        }
                        Ok(None) => {}
                        Err(e) => {
                            return Err(e);
                        }
                    }
                }
                Ok(Some(Symbol {
                    name: name.clone(),
                    var_type: VarTypeField {
                        name: "class".to_string(),
                        is_optional: false,
                        args: vec![],
                    },
                    read_only: false,
                    children: children_symbols,
                }))
            }
            AstNode::StructDef { name, fields } => {
                let mut children_symbols = HashMap::new();
                for field in fields {
                    self.check_type(&field.ty, symbols);
                    let symbol = Symbol {
                        name: field.name.clone(),
                        var_type: field.ty.clone(),
                        read_only: false,
                        children: HashMap::new(),
                    };
                    if children_symbols.contains_key(&symbol.name) {
                        return Err(AnalyzerError::AlreadyDefined(symbol.name));
                    }
                    children_symbols.insert(symbol.name.clone(), symbol);
                }
                Ok(Some(Symbol {
                    name: name.clone(),
                    var_type: VarTypeField {
                        name: "struct".to_string(),
                        is_optional: false,
                        args: vec![],
                    },
                    read_only: false,
                    children: children_symbols,
                }))
            }
            AstNode::EnumDef { name, variants } => {
                let mut children_symbols = HashMap::new();
                for v in variants {
                    let symbol = Symbol {
                        name: v.clone(),
                        var_type: VarTypeField {
                            name: "enum_variant".to_string(),
                            is_optional: false,
                            args: vec![],
                        },
                        read_only: true,
                        children: HashMap::new(),
                    };
                    if children_symbols.contains_key(&symbol.name) {
                        return Err(AnalyzerError::AlreadyDefined(symbol.name));
                    }
                    children_symbols.insert(symbol.name.clone(), symbol);
                }
                Ok(Some(Symbol {
                    name: name.clone(),
                    var_type: VarTypeField {
                        name: "enum".to_string(),
                        is_optional: false,
                        args: vec![],
                    },
                    read_only: false,
                    children: children_symbols,
                }))
            }
            AstNode::FunctionDef { name, params, return_type, body, is_static, is_private, decorators } => {
                let mut type_args = vec![];
                for param in params {
                    let var_type = param.1.clone();
                    self.check_type(&var_type, &this_symbols)?;
                    type_args.push(var_type);
                }
                let return_type = return_type.as_ref().unwrap();
                self.check_type(return_type, &this_symbols)?;
                type_args.push(return_type.clone());
                for b in body {
                    match self.analyze_node(b, &mut this_symbols) {
                        Ok(Some(symbol)) => {
                            let symbol_name = symbol.name.clone();
                            if this_symbols.contains_key(&symbol_name) {
                                return Err(AnalyzerError::AlreadyDefined(symbol_name));
                            }
                            this_symbols.insert(symbol_name.clone(), symbol.clone());
                        }
                        Ok(None) => {}
                        Err(e) => {
                            return Err(e);
                        }
                    };
                }
                Ok(Some(Symbol {
                    name: name.clone(),
                    var_type: VarTypeField {
                        name: "function".to_string(),
                        is_optional: false,
                        args: type_args,
                    },
                    read_only: false,
                    children: HashMap::new(),
                }))
            }
            AstNode::VarDecl { name, var_type, decl_type, expr, is_static, is_private } => {
                let ty = if var_type.is_none() {
                    self.determine_type(expr.as_ref().unwrap())?
                } else {
                    var_type.as_ref().unwrap().clone()
                };
                self.check_type(&ty, &this_symbols)?;
                Ok(Some(Symbol {
                    name: name.clone(),
                    var_type: ty,
                    read_only: *decl_type != VarDeclType::Let,
                    children: HashMap::new(),
                }))
            }
            AstNode::Import { module, names } => {
                Ok(None)
            }
            _ => {
                return Err(AnalyzerError::NotImplemented);
            }
        }
    }

    fn check_type(&self, var_type: &VarTypeField, symbols: &HashMap<String, Symbol>) -> Result<(), AnalyzerError> {
        const BUILTIN_TYPES: [&str; 12] = ["int8", "int16", "int32", "int64", "uint8", "uint16", "uint32", "uint64", "float32", "float64", "string", "bool"];
        if BUILTIN_TYPES.contains(&var_type.name.as_str()) {
            return Ok(());
        }
        if let Some(symbol) = symbols.get(&var_type.name) {
            match symbol.var_type.name.as_str() {
                "class" | "struct" | "enum" => {
                    return Ok(());
                }
                _ => {
                    return Err(AnalyzerError::NotType(var_type.name.clone()));
                }
            }
        }
        Err(AnalyzerError::NotDefined(var_type.name.clone()))
    }

    fn determine_type(&self, expr: &Expr) -> Result<VarTypeField, AnalyzerError> {
        match expr {
            Expr::IntLiteral(_) => Ok(VarTypeField {
                name: "int64".to_string(),
                is_optional: false,
                args: vec![],
            }),
            Expr::FloatLiteral(_) => Ok(VarTypeField {
                name: "float64".to_string(),
                is_optional: false,
                args: vec![],
            }),
            Expr::StringLiteral { value, ty, vars } => Ok(VarTypeField {
                name: "string".to_string(),
                is_optional: false,
                args: vec![],
            }),
            Expr::BoolLiteral(_) => Ok(VarTypeField {
                name: "bool".to_string(),
                is_optional: false,
                args: vec![],
            }),
            Expr::Binary {left, op, right} => {
                let left_type = self.determine_type(left)?;
                let right_type = self.determine_type(right)?;
                match op {
                    BinOp::Plus | BinOp::Minus | BinOp::Star | BinOp::Slash | BinOp::Power => {
                        if left_type.name == "int64" && right_type.name == "int64" {
                            return Ok(VarTypeField {
                                name: "int64".to_string(),
                                is_optional: false,
                                args: vec![],
                            });
                        }
                        if left_type.name == "float64" && right_type.name == "float64" {
                            return Ok(VarTypeField {
                                name: "float64".to_string(),
                                is_optional: false,
                                args: vec![],
                            });
                        }
                        if (left_type.name == "int64" && right_type.name == "float64") || (left_type.name == "float64" && right_type.name == "int64") {
                            return Ok(VarTypeField {
                                name: "float64".to_string(),
                                is_optional: false,
                                args: vec![],
                            });
                        }
                        return Err(AnalyzerError::TypeNotMatched {
                            left: left_type.name.clone(),
                            right: right_type.name.clone(),
                        });
                    },
                    BinOp::Percent => {
                        if left_type.name == "int64" && right_type.name == "int64" {
                            return Ok(VarTypeField {
                                name: "int64".to_string(),
                                is_optional: false,
                                args: vec![],
                            });
                        }
                        return Err(AnalyzerError::TypeNotMatched {
                            left: left_type.name.clone(),
                            right: right_type.name.clone(),
                        });
                    }
                    BinOp::Equal | BinOp::NotEqual => {
                        Ok(VarTypeField {
                            name: "bool".to_string(),
                            is_optional: false,
                            args: vec![],
                        })
                    }
                    BinOp::LessThan | BinOp::LessThanOrEqual | BinOp::GreaterThan | BinOp::GreaterThanOrEqual => {
                        if (left_type.name == "int64" || left_type.name == "float64") && (right_type.name == "int64" || right_type.name == "float64") {
                            Ok(VarTypeField {
                                name: "bool".to_string(),
                                is_optional: false,
                                args: vec![],
                            })
                        } else {
                            Err(AnalyzerError::TypeNotMatched {
                                left: left_type.name.clone(),
                                right: right_type.name.clone(),
                            })
                        }
                    }
                    BinOp::And | BinOp::Not => {
                        if (left_type.name != "bool" || right_type.name != "bool") {
                            return Err(AnalyzerError::TypeNotMatched {
                                left: left_type.name.clone(),
                                right: right_type.name.clone(),
                            });
                        }
                        Ok(VarTypeField {
                            name: "bool".to_string(),
                            is_optional: false,
                            args: vec![],
                        })
                    }
                    _ => Err(AnalyzerError::NotImplemented),
                }
            }
            _ => {
                Err(AnalyzerError::FailedToDetermineType)
            }
        }
    }
}
