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
        } => AstNode::ClassDef {
            name,
            members: members.into_iter().map(optimize_node).collect(),
            parents,
        },
        AstNode::FunctionDef {
            name,
            params,
            return_type,
            body,
            is_static,
            is_private,
            decorators,
        } => AstNode::FunctionDef {
            name,
            params,
            return_type,
            body: body.into_iter().map(optimize_node).collect(),
            is_static,
            is_private,
            decorators: decorators.into_iter().map(optimize_decorator).collect(),
        },
        AstNode::StructDef { name, fields } => AstNode::StructDef { name, fields },
        AstNode::VarDecl {
            name,
            var_type,
            decl_type,
            expr,
            is_static,
            is_private,
        } => AstNode::VarDecl {
            name,
            var_type,
            decl_type,
            expr: expr.map(optimize_expr),
            is_static,
            is_private,
        },
        AstNode::EnumDef { name, variants } => AstNode::EnumDef { name, variants },
        AstNode::ExprStmt(expr) => AstNode::ExprStmt(optimize_expr(expr)),
        AstNode::ReturnStmt(expr) => AstNode::ReturnStmt(expr.map(optimize_expr)),
        AstNode::SwitchStmt {
            expr,
            cases,
            default,
        } => AstNode::SwitchStmt {
            expr: optimize_expr(expr),
            cases: cases.into_iter().map(optimize_switch_case).collect(),
            default: default.map(|body| body.into_iter().map(optimize_node).collect()),
        },
        AstNode::IfStmt {
            condition,
            body,
            else_body,
        } => AstNode::IfStmt {
            condition: Box::new(optimize_expr(*condition)),
            body: body.into_iter().map(optimize_node).collect(),
            else_body: else_body.map(|stmts| stmts.into_iter().map(optimize_node).collect()),
        },
        AstNode::GuardStmt {
            condition,
            else_body,
        } => AstNode::GuardStmt {
            condition: Box::new(optimize_expr(*condition)),
            else_body: else_body.map(|stmts| stmts.into_iter().map(optimize_node).collect()),
        },
        AstNode::ForStmt {
            var_name,
            iterable,
            body,
        } => AstNode::ForStmt {
            var_name,
            iterable: optimize_expr(iterable),
            body: body.into_iter().map(optimize_node).collect(),
        },
        AstNode::TryStmt {
            body,
            catch_blocks,
            finally_block,
        } => AstNode::TryStmt {
            body: body.into_iter().map(optimize_node).collect(),
            catch_blocks: catch_blocks.into_iter().map(optimize_catch_block).collect(),
            finally_block: finally_block
                .map(|stmts| stmts.into_iter().map(optimize_node).collect()),
        },
        AstNode::ThrowStmt(expr) => AstNode::ThrowStmt(optimize_expr(expr)),
        AstNode::PassStmt => AstNode::PassStmt,
        AstNode::BreakStmt => AstNode::BreakStmt,
        AstNode::ContinueStmt => AstNode::ContinueStmt,
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
        Expr::Identifier(_)
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
        Expr::MemberAccess { target, member } => Expr::MemberAccess {
            target: target.map(|inner| Box::new(optimize_expr(*inner))),
            member,
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
