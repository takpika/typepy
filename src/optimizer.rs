use crate::parser::{AstNode, BinOp, CatchBlock, Decorator, Expr, LambdaBody, SwitchCase, UnaryOp};

pub fn optimize_ast(ast: AstNode) -> AstNode {
    optimize_node(ast)
}

fn optimize_node(node: AstNode) -> AstNode {
    match node {
        AstNode::File(items) => AstNode::File(items.into_iter().map(optimize_node).collect()),
        AstNode::Import { module, names } => AstNode::Import { module, names },
        AstNode::ClassDef {
            name,
            members,
            parents,
            span,
        } => AstNode::ClassDef {
            name,
            members: members.into_iter().map(optimize_node).collect(),
            parents,
            span,
        },
        AstNode::FunctionDef {
            name,
            params,
            return_type,
            body,
            is_static,
            is_private,
            decorators,
            span,
        } => AstNode::FunctionDef {
            name,
            params,
            return_type,
            body: body.into_iter().map(optimize_node).collect(),
            is_static,
            is_private,
            decorators: decorators.into_iter().map(optimize_decorator).collect(),
            span,
        },
        AstNode::StructDef { name, fields, span } => AstNode::StructDef { name, fields, span },
        AstNode::VarDecl {
            name,
            var_type,
            decl_type,
            expr,
            is_static,
            is_private,
            span,
        } => AstNode::VarDecl {
            name,
            var_type,
            decl_type,
            expr: expr.map(optimize_expr),
            is_static,
            is_private,
            span,
        },
        AstNode::EnumDef {
            name,
            variants,
            span,
        } => AstNode::EnumDef {
            name,
            variants,
            span,
        },
        AstNode::ExprStmt { expr, span } => AstNode::ExprStmt {
            expr: optimize_expr(expr),
            span,
        },
        AstNode::ReturnStmt { value, span } => AstNode::ReturnStmt {
            value: value.map(optimize_expr),
            span,
        },
        AstNode::SwitchStmt {
            expr,
            cases,
            default,
            span,
        } => AstNode::SwitchStmt {
            expr: optimize_expr(expr),
            cases: cases.into_iter().map(optimize_switch_case).collect(),
            default: default.map(|body| body.into_iter().map(optimize_node).collect()),
            span,
        },
        AstNode::IfStmt {
            condition,
            body,
            else_body,
            span,
        } => AstNode::IfStmt {
            condition: Box::new(optimize_expr(*condition)),
            body: body.into_iter().map(optimize_node).collect(),
            else_body: else_body.map(|stmts| stmts.into_iter().map(optimize_node).collect()),
            span,
        },
        AstNode::GuardStmt {
            condition,
            else_body,
            span,
        } => AstNode::GuardStmt {
            condition: Box::new(optimize_expr(*condition)),
            else_body: else_body.map(|stmts| stmts.into_iter().map(optimize_node).collect()),
            span,
        },
        AstNode::ForStmt {
            var_name,
            iterable,
            body,
            span,
        } => AstNode::ForStmt {
            var_name,
            iterable: optimize_expr(iterable),
            body: body.into_iter().map(optimize_node).collect(),
            span,
        },
        AstNode::TryStmt {
            body,
            catch_blocks,
            finally_block,
            span,
        } => AstNode::TryStmt {
            body: body.into_iter().map(optimize_node).collect(),
            catch_blocks: catch_blocks.into_iter().map(optimize_catch_block).collect(),
            finally_block: finally_block
                .map(|stmts| stmts.into_iter().map(optimize_node).collect()),
            span,
        },
        AstNode::ThrowStmt { expr, span } => AstNode::ThrowStmt {
            expr: optimize_expr(expr),
            span,
        },
        AstNode::PassStmt { span } => AstNode::PassStmt { span },
        AstNode::BreakStmt { span } => AstNode::BreakStmt { span },
        AstNode::ContinueStmt { span } => AstNode::ContinueStmt { span },
    }
}

fn optimize_switch_case(case: SwitchCase) -> SwitchCase {
    SwitchCase {
        pattern: case.pattern,
        body: case.body.into_iter().map(optimize_node).collect(),
    }
}

fn optimize_catch_block(block: CatchBlock) -> CatchBlock {
    CatchBlock {
        exception_type: block.exception_type,
        exception_name: block.exception_name,
        body: block.body.into_iter().map(optimize_node).collect(),
    }
}

fn optimize_decorator(decorator: Decorator) -> Decorator {
    Decorator {
        name: decorator.name,
        args: decorator.args.into_iter().map(optimize_expr).collect(),
    }
}

fn optimize_lambda_body(body: LambdaBody) -> LambdaBody {
    match body {
        LambdaBody::Expr(expr) => LambdaBody::Expr(Box::new(optimize_expr(*expr))),
        LambdaBody::Block(stmts) => {
            LambdaBody::Block(stmts.into_iter().map(optimize_node).collect())
        }
    }
}

fn optimize_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Identifier { .. }
        | Expr::IntLiteral(_)
        | Expr::FloatLiteral(_)
        | Expr::StringLiteral { .. }
        | Expr::BoolLiteral(_)
        | Expr::NoneLiteral => expr,
        Expr::Binary { left, op, right } => {
            let left = optimize_expr(*left);
            let right = optimize_expr(*right);
            if let Some(folded) = fold_binary(&left, &op, &right) {
                folded
            } else if let Some(simplified) = simplify_binary(&left, &op, &right) {
                simplified
            } else {
                Expr::Binary {
                    left: Box::new(left),
                    op,
                    right: Box::new(right),
                }
            }
        }
        Expr::Unary { op, expr: inner } => {
            let inner = optimize_expr(*inner);
            if let Some(folded) = fold_unary(&op, &inner) {
                folded
            } else {
                Expr::Unary {
                    op,
                    expr: Box::new(inner),
                }
            }
        }
        Expr::Call { callee, args } => Expr::Call {
            callee: Box::new(optimize_expr(*callee)),
            args: args.into_iter().map(optimize_expr).collect(),
        },
        Expr::Function { params, body } => Expr::Function {
            params,
            body: body.into_iter().map(optimize_node).collect(),
        },
        Expr::Lambda { params, body } => Expr::Lambda {
            params,
            body: optimize_lambda_body(body),
        },
        Expr::VarDecl {
            name,
            decl_type,
            expr,
        } => Expr::VarDecl {
            name,
            decl_type,
            expr: Box::new(optimize_expr(*expr)),
        },
        Expr::Assign { target, value } => Expr::Assign {
            target: Box::new(optimize_expr(*target)),
            value: Box::new(optimize_expr(*value)),
        },
        Expr::MemberAccess {
            target,
            member,
            span,
        } => Expr::MemberAccess {
            target: target.map(|inner| Box::new(optimize_expr(*inner))),
            member,
            span,
        },
        Expr::CompoundAssign { op, target, value } => Expr::CompoundAssign {
            op,
            target: Box::new(optimize_expr(*target)),
            value: Box::new(optimize_expr(*value)),
        },
        Expr::ArrayLiteral(items) => {
            Expr::ArrayLiteral(items.into_iter().map(optimize_expr).collect())
        }
        Expr::DictLiteral(entries) => Expr::DictLiteral(
            entries
                .into_iter()
                .map(|(k, v)| (optimize_expr(k), optimize_expr(v)))
                .collect(),
        ),
        Expr::TupleLiteral(items) => {
            Expr::TupleLiteral(items.into_iter().map(optimize_expr).collect())
        }
        Expr::Range { start, end, op } => Expr::Range {
            start: Box::new(optimize_expr(*start)),
            end: Box::new(optimize_expr(*end)),
            op,
        },
        Expr::Index { target, index } => Expr::Index {
            target: Box::new(optimize_expr(*target)),
            index: Box::new(optimize_expr(*index)),
        },
        Expr::Unwrap { target } => Expr::Unwrap {
            target: Box::new(optimize_expr(*target)),
        },
        Expr::OptionalChaining { target } => Expr::OptionalChaining {
            target: Box::new(optimize_expr(*target)),
        },
        Expr::NullCoalescing { left, right } => Expr::NullCoalescing {
            left: Box::new(optimize_expr(*left)),
            right: Box::new(optimize_expr(*right)),
        },
    }
}

fn simplify_binary(left: &Expr, op: &BinOp, right: &Expr) -> Option<Expr> {
    match op {
        BinOp::Plus => simplify_addition(left, right),
        BinOp::Star => simplify_multiplication(left, right),
        _ => None,
    }
}

#[derive(Clone)]
enum AddTerm {
    Expr(Expr),
    Int(i64),
    Float(f64),
}

fn collect_add_terms(expr: &Expr, terms: &mut Vec<AddTerm>) {
    match expr {
        Expr::Binary {
            left,
            op: BinOp::Plus,
            right,
        } => {
            collect_add_terms(left, terms);
            collect_add_terms(right, terms);
        }
        Expr::IntLiteral(value) => terms.push(AddTerm::Int(*value)),
        Expr::FloatLiteral(value) => terms.push(AddTerm::Float(*value)),
        other => terms.push(AddTerm::Expr(other.clone())),
    }
}

fn simplify_addition(left: &Expr, right: &Expr) -> Option<Expr> {
    let mut terms = Vec::new();
    collect_add_terms(left, &mut terms);
    collect_add_terms(right, &mut terms);

    let mut int_sum: i64 = 0;
    let mut has_int = false;
    let mut float_sum: f64 = 0.0;
    let mut has_float = false;
    let mut constant_count = 0;
    for term in &terms {
        match term {
            AddTerm::Int(value) => {
                constant_count += 1;
                has_int = true;
                int_sum = int_sum.checked_add(*value)?;
            }
            AddTerm::Float(value) => {
                constant_count += 1;
                has_float = true;
                float_sum += *value;
            }
            AddTerm::Expr(_) => {}
        }
    }

    if constant_count == 0 {
        return None;
    }

    let has_non_constant = terms.iter().any(|term| matches!(term, AddTerm::Expr(_)));
    let mut constant_expr = if has_float {
        let total = float_sum + if has_int { int_sum as f64 } else { 0.0 };
        if total == 0.0 && has_non_constant {
            None
        } else {
            Some(Expr::FloatLiteral(total))
        }
    } else {
        if int_sum == 0 && has_non_constant {
            None
        } else {
            Some(Expr::IntLiteral(int_sum))
        }
    };

    let mut changed = constant_count > 1;
    if constant_expr.is_none() {
        changed = true;
    }

    if !changed {
        return None;
    }

    let mut result_terms: Vec<Expr> = Vec::new();
    let mut constant_inserted = false;
    for term in terms {
        match term {
            AddTerm::Expr(expr) => result_terms.push(expr),
            AddTerm::Int(_) | AddTerm::Float(_) => {
                if !constant_inserted {
                    if let Some(expr) = constant_expr.take() {
                        result_terms.push(expr);
                        constant_inserted = true;
                    }
                }
            }
        }
    }

    if !constant_inserted {
        if let Some(expr) = constant_expr.take() {
            result_terms.push(expr);
        }
    }

    if result_terms.is_empty() {
        return Some(Expr::IntLiteral(0));
    }

    let mut iter = result_terms.into_iter();
    let mut result = iter.next().unwrap();
    for term in iter {
        result = Expr::Binary {
            left: Box::new(result),
            op: BinOp::Plus,
            right: Box::new(term),
        };
    }

    Some(result)
}

#[derive(Clone)]
enum MulFactor {
    Expr(Expr),
    Int(i64),
    Float(f64),
}

fn collect_mul_factors(expr: &Expr, factors: &mut Vec<MulFactor>) {
    match expr {
        Expr::Binary {
            left,
            op: BinOp::Star,
            right,
        } => {
            collect_mul_factors(left, factors);
            collect_mul_factors(right, factors);
        }
        Expr::IntLiteral(value) => factors.push(MulFactor::Int(*value)),
        Expr::FloatLiteral(value) => factors.push(MulFactor::Float(*value)),
        other => factors.push(MulFactor::Expr(other.clone())),
    }
}

fn simplify_multiplication(left: &Expr, right: &Expr) -> Option<Expr> {
    let mut factors = Vec::new();
    collect_mul_factors(left, &mut factors);
    collect_mul_factors(right, &mut factors);

    let mut int_product: i64 = 1;
    let mut has_int = false;
    let mut float_product: f64 = 1.0;
    let mut has_float = false;
    let mut constant_count = 0;
    for factor in &factors {
        match factor {
            MulFactor::Int(value) => {
                constant_count += 1;
                has_int = true;
                int_product = int_product.checked_mul(*value)?;
            }
            MulFactor::Float(value) => {
                constant_count += 1;
                has_float = true;
                float_product *= *value;
            }
            MulFactor::Expr(_) => {}
        }
    }

    if constant_count == 0 {
        return None;
    }

    let has_non_constant = factors
        .iter()
        .any(|factor| matches!(factor, MulFactor::Expr(_)));
    let mut constant_expr = if has_float {
        let total = float_product * if has_int { int_product as f64 } else { 1.0 };
        if total == 1.0 && has_non_constant {
            None
        } else {
            Some(Expr::FloatLiteral(total))
        }
    } else {
        if int_product == 1 && has_non_constant {
            None
        } else {
            Some(Expr::IntLiteral(int_product))
        }
    };

    let mut changed = constant_count > 1;
    if constant_expr.is_none() {
        changed = true;
    }

    if !changed {
        return None;
    }

    let mut result_factors: Vec<Expr> = Vec::new();
    let mut constant_inserted = false;
    for factor in factors {
        match factor {
            MulFactor::Expr(expr) => result_factors.push(expr),
            MulFactor::Int(_) | MulFactor::Float(_) => {
                if !constant_inserted {
                    if let Some(expr) = constant_expr.take() {
                        result_factors.push(expr);
                        constant_inserted = true;
                    }
                }
            }
        }
    }

    if !constant_inserted {
        if let Some(expr) = constant_expr.take() {
            result_factors.push(expr);
        }
    }

    if result_factors.is_empty() {
        return Some(Expr::IntLiteral(1));
    }

    let mut iter = result_factors.into_iter();
    let mut result = iter.next().unwrap();
    for factor in iter {
        result = Expr::Binary {
            left: Box::new(result),
            op: BinOp::Star,
            right: Box::new(factor),
        };
    }

    Some(result)
}

fn fold_binary(left: &Expr, op: &BinOp, right: &Expr) -> Option<Expr> {
    match (left, right) {
        (Expr::IntLiteral(lv), Expr::IntLiteral(rv)) => fold_ints(*lv, op, *rv),
        (Expr::FloatLiteral(lv), Expr::FloatLiteral(rv)) => fold_floats(*lv, op, *rv),
        (Expr::IntLiteral(lv), Expr::FloatLiteral(rv)) => fold_floats(*lv as f64, op, *rv),
        (Expr::FloatLiteral(lv), Expr::IntLiteral(rv)) => fold_floats(*lv, op, *rv as f64),
        _ => None,
    }
}

fn fold_ints(left: i64, op: &BinOp, right: i64) -> Option<Expr> {
    match op {
        BinOp::Plus => left.checked_add(right).map(Expr::IntLiteral),
        BinOp::Minus => left.checked_sub(right).map(Expr::IntLiteral),
        BinOp::Star => left.checked_mul(right).map(Expr::IntLiteral),
        BinOp::Slash => {
            if right == 0 {
                None
            } else {
                let (quotient, remainder) = (left / right, left % right);
                if remainder == 0 {
                    Some(Expr::IntLiteral(quotient))
                } else {
                    None
                }
            }
        }
        BinOp::Percent => {
            if right == 0 {
                None
            } else {
                Some(Expr::IntLiteral(left % right))
            }
        }
        _ => None,
    }
}

fn fold_floats(left: f64, op: &BinOp, right: f64) -> Option<Expr> {
    match op {
        BinOp::Plus => Some(Expr::FloatLiteral(left + right)),
        BinOp::Minus => Some(Expr::FloatLiteral(left - right)),
        BinOp::Star => Some(Expr::FloatLiteral(left * right)),
        _ => None,
    }
}

fn fold_unary(op: &UnaryOp, expr: &Expr) -> Option<Expr> {
    match (op, expr) {
        (UnaryOp::Negate, Expr::IntLiteral(value)) => {
            if *value == i64::MIN {
                None
            } else {
                Some(Expr::IntLiteral(-*value))
            }
        }
        (UnaryOp::Negate, Expr::FloatLiteral(value)) => Some(Expr::FloatLiteral(-*value)),
        (UnaryOp::Not, Expr::BoolLiteral(value)) => Some(Expr::BoolLiteral(!value)),
        _ => None,
    }
}
