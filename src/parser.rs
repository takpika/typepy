use crate::lexer::Lexer;
use crate::token::{Position, Span, SpannedToken, Token};
use std::fmt;
use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub struct ParserError {
    pub message: String,
    pub span: Span,
    pub snippet: Option<String>,
}

impl ParserError {
    pub fn new<S: Into<String>>(message: S, span: Span, snippet: Option<String>) -> Self {
        Self {
            message: message.into(),
            span,
            snippet,
        }
    }
}

impl fmt::Display for ParserError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} (line {}:{}, line {}:{})",
            self.message,
            self.span.start.line,
            self.span.start.column,
            self.span.end.line,
            self.span.end.column
        )?;

        if let Some(snippet) = &self.snippet {
            write!(f, "\n{}", snippet)?;
        }

        Ok(())
    }
}

impl std::error::Error for ParserError {}

pub type ParseResult<T> = Result<T, ParserError>;

/// ========================
///        AST定義
/// ========================

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    /// ソースファイル全体
    File(Vec<AstNode>),

    /// import 文 (例: `import os;`)
    Import {
        module: Option<String>,
        names: Vec<ImportName>,
    },

    /// class定義 (例: `class User { ... }`)
    ClassDef {
        name: String,
        members: Vec<AstNode>,
        parents: Vec<String>,
        span: Span,
    },

    /// 関数定義 (例: `def greet(user: User) -> None { ... }`)
    FunctionDef {
        name: String,
        params: Vec<(String, VarTypeField)>, // (param_name, param_type)
        return_type: Option<VarTypeField>,   // -> Some("None")など
        body: Vec<AstNode>,                  // 関数本体のステートメント
        is_static: bool,                     // static 修飾子があれば true
        is_private: bool,                    // private 修飾子があれば true
        decorators: Vec<Decorator>,          // デコレータ (@deprecated など)
        span: Span,
    },

    StructDef {
        name: String,
        fields: Vec<StructField>,
        span: Span,
    },

    /// 変数宣言 (例: `final name: str = "Alice";`)
    VarDecl {
        name: String,
        var_type: Option<VarTypeField>,
        decl_type: VarDeclType,
        expr: Option<Expr>, // 初期化式
        is_static: bool,    // static 修飾子があれば true
        is_private: bool,
        span: Span,
    },

    EnumDef {
        name: String,
        variants: Vec<String>,
        span: Span,
    },

    /// 式ステートメント (例: `print("hello");`)
    ExprStmt(Expr),

    /// return文 (例: `return x + 1;`)
    ReturnStmt(Option<Expr>),

    SwitchStmt {
        expr: Expr,
        cases: Vec<SwitchCase>,
        default: Option<Vec<AstNode>>,
    },

    IfStmt {
        condition: Box<Expr>,
        body: Vec<AstNode>,
        else_body: Option<Vec<AstNode>>,
    },

    GuardStmt {
        condition: Box<Expr>,
        else_body: Option<Vec<AstNode>>,
    },

    ForStmt {
        var_name: String,
        iterable: Expr,
        body: Vec<AstNode>,
    },

    TryStmt {
        body: Vec<AstNode>,
        catch_blocks: Vec<CatchBlock>,
        finally_block: Option<Vec<AstNode>>,
    },

    ThrowStmt(Expr),

    PassStmt,
    BreakStmt,
    ContinueStmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub name: String,
    pub ty: VarTypeField,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarTypeField {
    pub name: String,
    pub is_optional: bool,
    pub args: Vec<VarTypeField>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SwitchCase {
    pub pattern: String,    // 例: "admin", "user", etc.
    pub body: Vec<AstNode>, // caseの中身
}

#[derive(Debug, Clone, PartialEq)]
pub struct CatchBlock {
    pub exception_type: Option<VarTypeField>,
    pub exception_name: Option<String>,
    pub body: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportName {
    pub name: String,
    pub as_name: Option<String>,
}

/// 式(Expressions)のAST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier {
        name: String,
        span: Span,
    },
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral {
        value: String,
        ty: StringLiteralType,
        vars: Vec<String>,
    },
    BoolLiteral(bool),

    /// 関数呼び出し: foo(...)
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    Function {
        params: Vec<(String, VarTypeField)>,
        body: Vec<AstNode>,
    },

    Lambda {
        params: Vec<(String, VarTypeField)>, // 引数名, 型
        body: LambdaBody,                    // 例えば式 or 複数ステートメント
    },

    /// 2項演算子など(今回はサンプル)
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },

    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
    },

    VarDecl {
        name: String,
        decl_type: VarDeclType,
        expr: Box<Expr>,
    },

    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },

    MemberAccess {
        target: Option<Box<Expr>>,
        member: String,
        span: Span,
    },

    CompoundAssign {
        op: BinOp,
        target: Box<Expr>,
        value: Box<Expr>,
    },

    ArrayLiteral(Vec<Expr>),
    DictLiteral(Vec<(Expr, Expr)>),
    TupleLiteral(Vec<Expr>),
    NoneLiteral,

    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        op: BinOp,
    },

    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    },

    Unwrap {
        target: Box<Expr>,
    },

    OptionalChaining {
        target: Box<Expr>,
    },

    NullCoalescing {
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringLiteralType {
    Regular,
    Raw,
    Format,
    Regex,
}

#[derive(Debug, Clone, PartialEq)]
pub enum VarDeclType {
    Let,
    Final,
    Const,
}

/// 2項演算子の種類
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Power,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
    Not,
    RangeHalfOpen,
    RangeClosed,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Negate,
    Not,
}

/// ボディが `{ ... }` の複数ステートメントか、`=> expr` の一行表記かなど
#[derive(Debug, Clone, PartialEq)]
pub enum LambdaBody {
    Expr(Box<Expr>),
    Block(Vec<AstNode>),
}

/// ========================
///        パーサー本体
/// ========================

pub struct Parser {
    source: Arc<str>,
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    fn fallback_span(&self) -> Span {
        self.tokens.last().map(|t| t.span).unwrap_or_else(|| Span {
            start: Position::new(0, 1, 0),
            end: Position::new(0, 1, 0),
        })
    }

    fn error<S: Into<String>>(&self, message: S) -> ParserError {
        self.error_at(self.current_span(), message)
    }

    fn error_at<S: Into<String>>(&self, span: Option<Span>, message: S) -> ParserError {
        let span = span.unwrap_or_else(|| self.fallback_span());
        let snippet = self.make_snippet(span);
        ParserError::new(message, span, snippet)
    }

    /// コンストラクタ
    pub fn new(source: Arc<str>, tokens: Vec<SpannedToken>) -> Self {
        Parser {
            source,
            tokens,
            pos: 0,
        }
    }

    fn make_snippet(&self, span: Span) -> Option<String> {
        span.snippet(&self.source)
    }

    /// Indentトークンを連続でスキップするヘルパー
    fn skip_indent(&mut self) {
        while let Token::Indent(_) = self.current_token() {
            self.advance();
        }
    }

    /// 指定したトークンが現在のトークンと一致するかチェックし、
    /// 一致していればそれを返して pos を1進める。
    /// 一致しなければエラーを返す。
    pub fn expect(&mut self, expected: &Token) -> ParseResult<Token> {
        let current = self.current_token();
        if current == *expected {
            // OK → advance() して返す
            self.advance();
            Ok(current)
        } else {
            // NG → エラー
            Err(self.error_at(
                self.current_span(),
                format!("Expected {:?}, but found {:?}", expected, current),
            ))
        }
    }

    /// parse_type():
    ///  1) parse union type (one or more primary types joined by '|')
    ///  2) if there's a trailing '?', mark is_optional = true
    pub fn parse_type(&mut self) -> ParseResult<VarTypeField> {
        // 1) parse union
        let mut types = vec![self.parse_primary_type()?];

        // 連続で "|" が来たら、さらに primary_type を読む
        while self.current_token() == Token::Pipe {
            self.advance(); // consume '|'
            let rhs = self.parse_primary_type()?;
            types.push(rhs);
        }

        // unionかどうか判定
        let mut result = if types.len() == 1 {
            // 単独 => unionじゃない
            types.remove(0)
        } else {
            // union => name="union", args=[typeA, typeB, ...]
            VarTypeField {
                name: "union".to_string(),
                is_optional: false,
                args: types,
            }
        };

        // 2) optional '?'
        if self.current_token() == Token::Question {
            self.advance(); // consume '?'
            result.is_optional = true;
        }

        Ok(result)
    }

    /// parse_primary_type():
    ///   functionType | arrayType | dictType | tupleType | identifier
    fn parse_primary_type(&mut self) -> ParseResult<VarTypeField> {
        match self.current_token() {
            // function(...) -> T
            Token::Identifier(ref s) if s == "function" => self.parse_function_type(),
            // [ T ]
            Token::LBracket => self.parse_array_type(),
            // { K : V }
            Token::LBrace => self.parse_dict_type(),
            // ( T1, T2, ... )
            Token::LParen => self.parse_tuple_type(),
            // identifier
            Token::Identifier(ref s) => {
                // e.g. "str", "int", or custom type name
                let name = s.clone();
                self.advance(); // consume the identifier

                // それだけで1つの型
                Ok(VarTypeField {
                    name,
                    is_optional: false,
                    args: vec![],
                })
            }
            Token::None => {
                self.advance();
                Ok(VarTypeField {
                    name: "None".to_string(),
                    is_optional: false,
                    args: vec![],
                })
            }
            ref other => {
                Err(self.error(format!("parse_primary_type: Unexpected token: {:?}", other)))
            }
        }
    }

    /// function(uint64) -> str
    /// function(...) -> Type
    fn parse_function_type(&mut self) -> ParseResult<VarTypeField> {
        // consume "function"
        if let Token::Identifier(ref s) = self.current_token() {
            if s == "function" {
                self.advance();
            }
        }

        // expect '('
        self.expect(&Token::LParen)?;

        // parse zero or more types, comma separated
        let mut param_types = Vec::new();
        if self.current_token() != Token::RParen {
            // at least one type
            loop {
                let ty = self.parse_type()?;
                param_types.push(ty);

                if self.current_token() == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // expect ')'
        self.expect(&Token::RParen)?;

        // expect '->'
        if self.current_token() != Token::Arrow {
            return Err(self.error(format!(
                "Expected '->' in function type, got {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '->'

        // parse return type
        let return_ty = self.parse_type()?;

        // function type: name="function", args=[ param1, param2, ..., returnType ]
        let mut args = param_types;
        args.push(return_ty);

        Ok(VarTypeField {
            name: "function".to_string(),
            is_optional: false,
            args,
        })
    }

    /// array type: [T]
    fn parse_array_type(&mut self) -> ParseResult<VarTypeField> {
        // consume '['
        self.expect(&Token::LBracket)?;

        // parse inner type
        let inner_ty = self.parse_type()?;

        // expect ']'
        self.expect(&Token::RBracket)?;

        Ok(VarTypeField {
            name: "array".to_string(),
            is_optional: false,
            args: vec![inner_ty],
        })
    }

    /// dict type: {K : V}
    fn parse_dict_type(&mut self) -> ParseResult<VarTypeField> {
        // consume '{'
        self.expect(&Token::LBrace)?;

        // parse key type
        let key_ty = self.parse_type()?;

        // expect ':'
        if self.current_token() != Token::Colon {
            return Err(self.error(format!(
                "Expected ':' in dict type, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume ':'

        // parse value type
        let val_ty = self.parse_type()?;

        // expect '}'
        self.expect(&Token::RBrace)?;

        Ok(VarTypeField {
            name: "dict".to_string(),
            is_optional: false,
            args: vec![key_ty, val_ty],
        })
    }

    fn parse_tuple_type(&mut self) -> ParseResult<VarTypeField> {
        // 先頭の '(' を expect
        self.expect(&Token::LParen)?;

        let mut tuple_elems = Vec::new();

        if self.current_token() != Token::RParen {
            // parse at least one type
            loop {
                let elem_ty = self.parse_type()?;
                tuple_elems.push(elem_ty);

                if self.current_token() == Token::Comma {
                    self.advance(); // consume ','
                } else {
                    break;
                }
            }
        }

        // expect ')'
        self.expect(&Token::RParen)?;

        // もし要素数が1つなら「タプル」ではなく単なるグルーピング
        if tuple_elems.len() == 1 {
            // 例: (function(uint64) -> str)?
            // → 単なる function(uint64) -> str という型のグルーピング
            Ok(tuple_elems.remove(0)) // 1つ取り出して返す
        } else {
            // それ以外（要素2つ以上）は本当にタプル型
            Ok(VarTypeField {
                name: "tuple".to_string(),
                is_optional: false,
                args: tuple_elems,
            })
        }
    }

    /// メインエントリ: ファイル全体をパース
    pub fn parse_file(&mut self) -> ParseResult<AstNode> {
        let mut items = Vec::new();

        loop {
            // まず Indent を飛ばす
            self.skip_indent();
            if self.current_token() == Token::Eof {
                break;
            }
            // トップレベル要素を1つずつ解析
            let item = self.parse_statement(false)?;
            items.push(item);
        }

        Ok(AstNode::File(items))
    }

    fn parse_switch_stmt(&mut self) -> ParseResult<AstNode> {
        // 1) parse the expression after "switch"
        let switch_expr = self.parse_expr()?;

        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after switch expression, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        let mut cases = Vec::new();
        let mut default = None;

        // 3) parse case... or default... until '}'
        loop {
            self.skip_indent();

            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }

            match self.current_token() {
                // case
                Token::Case => {
                    self.advance(); // consume 'case'

                    // next is typically an identifier or maybe .identifier
                    // ここではシンプルに "admin", "user"などを想定
                    let pattern_str = match self.current_token() {
                        Token::Identifier(ref s) => {
                            let name = s.clone();
                            self.advance(); // consume identifier
                            name
                        }
                        Token::Dot => {
                            // もし "case .admin" みたいな書き方なら
                            // consume '.'
                            self.advance();
                            // expect identifier
                            if let Token::Identifier(ref s) = self.current_token() {
                                let name = s.clone();
                                self.advance();
                                name
                            } else {
                                return Err(
                                    self.error("Expected identifier after '.' in case pattern")
                                );
                            }
                        }
                        ref other => {
                            return Err(self.error(format!(
                                "Expected identifier after 'case', found {:?}",
                                other
                            )));
                        }
                    };

                    // expect '{'
                    if self.current_token() != Token::LBrace {
                        return Err(self.error(format!(
                            "Expected '{{' after case pattern, found {:?}",
                            self.current_token()
                        )));
                    }
                    self.advance(); // consume '{'

                    // parse statements until '}'
                    let mut body_stmts = Vec::new();
                    loop {
                        self.skip_indent();
                        if self.current_token() == Token::RBrace
                            || self.current_token() == Token::Eof
                        {
                            break;
                        }
                        let stmt = self.parse_statement(false)?;
                        body_stmts.push(stmt);
                    }

                    // expect '}'
                    if self.current_token() == Token::RBrace {
                        self.advance();
                    } else {
                        return Err(self.error("Missing '}' after case body"));
                    }

                    cases.push(SwitchCase {
                        pattern: pattern_str,
                        body: body_stmts,
                    });
                }

                // default
                Token::Default => {
                    self.advance(); // consume 'default'
                    if self.current_token() != Token::LBrace {
                        return Err(self.error(format!(
                            "Expected '{{' after default, found {:?}",
                            self.current_token()
                        )));
                    }
                    self.advance(); // consume '{'

                    let mut body_stmts = Vec::new();
                    loop {
                        self.skip_indent();
                        if self.current_token() == Token::RBrace
                            || self.current_token() == Token::Eof
                        {
                            break;
                        }
                        let stmt = self.parse_statement(false)?;
                        body_stmts.push(stmt);
                    }

                    if self.current_token() == Token::RBrace {
                        self.advance();
                    } else {
                        return Err(self.error("Missing '}' after default body"));
                    }
                    default = Some(body_stmts);
                }

                // unexpected token
                ref other => {
                    return Err(
                        self.error(format!("Expected 'case' or 'default', found {:?}", other))
                    );
                }
            }
        }

        // 4) now expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else if self.current_token() != Token::Eof {
            return Err(self.error(format!(
                "Expected '}}' at end of switch, found {:?}",
                self.current_token()
            )));
        }

        Ok(AstNode::SwitchStmt {
            expr: switch_expr,
            cases,
            default,
        })
    }

    fn parse_try_stmt(&mut self) -> ParseResult<AstNode> {
        // 1) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after 'try', found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        // 2) parse statements until '}'
        let mut try_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            try_stmts.push(stmt);
        }

        // 3) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of try block"));
        }

        // 4) parse catch blocks
        // sample: catch e: Exception { ... }
        let mut catch_blocks = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() != Token::Catch {
                break;
            }
            self.advance(); // consume 'catch'

            let exception_name = match self.current_token() {
                Token::Identifier(ref s) => {
                    let name = s.clone();
                    self.advance();
                    Some(name)
                }
                _ => None,
            };
            if exception_name != None {
                self.expect(&Token::Colon)?;
            }
            let exception_type = if exception_name != None {
                Some(self.parse_type()?)
            } else {
                None
            };

            // expect '{'
            self.expect(&Token::LBrace)?;

            // parse statements until '}'
            let mut catch_stmts = Vec::new();
            loop {
                self.skip_indent();
                if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                    break;
                }
                let stmt = self.parse_statement(false)?;
                catch_stmts.push(stmt);
            }

            // expect '}'
            self.expect(&Token::RBrace)?;

            catch_blocks.push(CatchBlock {
                exception_type,
                exception_name,
                body: catch_stmts,
            });
        }

        // 5) parse finally block
        let mut finally_block = None;
        if self.current_token() == Token::Finally {
            self.advance(); // consume 'finally'
            if self.current_token() != Token::LBrace {
                return Err(self.error(format!(
                    "Expected '{{' after 'finally', found {:?}",
                    self.current_token()
                )));
            }
            self.advance(); // consume '{'

            let mut finally_stmts = Vec::new();
            loop {
                self.skip_indent();
                if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                    break;
                }
                let stmt = self.parse_statement(false)?;
                finally_stmts.push(stmt);
            }

            if self.current_token() == Token::RBrace {
                self.advance();
            } else {
                return Err(self.error("Missing '}' at end of finally block"));
            }

            finally_block = Some(finally_stmts);
        }

        Ok(AstNode::TryStmt {
            body: try_stmts,
            catch_blocks,
            finally_block,
        })
    }

    fn parse_if_stmt(&mut self) -> ParseResult<AstNode> {
        // 1) parse the condition
        let condition = match self.current_token() {
            Token::Final | Token::Let => {
                let mut conditions = Vec::new();
                loop {
                    match self.current_token() {
                        Token::Final | Token::Let => {
                            let token = self.current_token().clone();
                            self.advance();
                            match self.parse_var_decl(token, false, false)? {
                                AstNode::VarDecl {
                                    name,
                                    expr,
                                    decl_type,
                                    ..
                                } => {
                                    conditions.push(Expr::VarDecl {
                                        name: name,
                                        decl_type: decl_type,
                                        expr: Box::new(expr.unwrap()),
                                    });
                                }
                                _ => return Err(self.error("Expected VarDecl in if condition")),
                            }
                        }
                        _ => break,
                    }
                }
                if conditions.len() == 1 {
                    conditions.remove(0)
                } else {
                    let mut expr = conditions.remove(0);
                    for cond in conditions {
                        expr = Expr::Binary {
                            left: Box::new(expr),
                            op: BinOp::And,
                            right: Box::new(cond),
                        };
                    }
                    expr
                }
            }
            _ => self.parse_expr()?,
        };

        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after if condition, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        // 3) parse statements until '}'
        let mut body_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            body_stmts.push(stmt);
        }

        // 4) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of if body"));
        }

        // 5) check for "else" or "elif"
        let mut else_body = None;
        if self.current_token() == Token::Else {
            self.advance(); // consume 'else'

            // 6) expect '{'
            if self.current_token() != Token::LBrace {
                return Err(self.error(format!(
                    "Expected '{{' after else, found {:?}",
                    self.current_token()
                )));
            }
            self.advance(); // consume '{'

            // 7) parse statements until '}'
            let mut else_stmts = Vec::new();
            loop {
                self.skip_indent();
                if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                    break;
                }
                let stmt = self.parse_statement(false)?;
                else_stmts.push(stmt);
            }

            // 8) expect '}'
            if self.current_token() == Token::RBrace {
                self.advance();
            } else {
                return Err(self.error("Missing '}' at end of else body"));
            }

            else_body = Some(else_stmts);
        }

        Ok(AstNode::IfStmt {
            condition: Box::new(condition),
            body: body_stmts,
            else_body,
        })
    }

    fn parse_for_stmt(&mut self) -> ParseResult<AstNode> {
        // 1) expect identifier
        let var_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => {
                return Err(self.error(format!(
                    "Expected identifier after 'for', found {:?}",
                    other
                )));
            }
        };

        // 2) expect "in"
        if self.current_token() != Token::In {
            return Err(self.error(format!(
                "Expected 'in' after for loop variable, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume 'in'

        // 3) parse the iterable expression
        let iterable = self.parse_expr()?;

        // 4) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after for loop, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        // 5) parse statements until '}'
        let mut body_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            body_stmts.push(stmt);
        }

        // 6) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of for loop body"));
        }

        Ok(AstNode::ForStmt {
            var_name,
            iterable,
            body: body_stmts,
        })
    }

    fn parse_guard_stmt(&mut self) -> ParseResult<AstNode> {
        let condition = match self.current_token() {
            Token::Final | Token::Let => {
                let mut conditions = Vec::new();
                loop {
                    match self.current_token() {
                        Token::Final | Token::Let => {
                            let token = self.current_token().clone();
                            self.advance();
                            match self.parse_var_decl(token, false, false)? {
                                AstNode::VarDecl {
                                    name,
                                    expr,
                                    decl_type,
                                    ..
                                } => {
                                    conditions.push(Expr::VarDecl {
                                        name: name,
                                        decl_type: decl_type,
                                        expr: Box::new(expr.unwrap()),
                                    });
                                }
                                _ => return Err(self.error("Expected VarDecl in guard condition")),
                            }
                        }
                        _ => break,
                    }
                }
                if conditions.len() == 1 {
                    conditions.remove(0)
                } else {
                    let mut expr = conditions.remove(0);
                    for cond in conditions {
                        expr = Expr::Binary {
                            left: Box::new(expr),
                            op: BinOp::And,
                            right: Box::new(cond),
                        };
                    }
                    expr
                }
            }
            _ => return Err(self.error("Expected condition after 'guard'")),
        };

        if self.current_token() != Token::Else {
            return Err(self.error(format!(
                "Expected 'else' after guard condition, found {:?}",
                self.current_token()
            )));
        }
        self.advance();
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after 'else', found {:?}",
                self.current_token()
            )));
        }
        self.advance();
        let mut else_elms = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            else_elms.push(stmt);
        }
        if self.current_token() != Token::RBrace {
            return Err(self.error("Missing '}' at end of guard statement"));
        }
        self.advance();
        Ok(AstNode::GuardStmt {
            condition: Box::new(condition),
            else_body: Some(else_elms),
        })
    }

    /// import構文をパース (import xxx;)
    fn parse_import(&mut self) -> ParseResult<AstNode> {
        let mut names = Vec::new();

        loop {
            // expect module name
            let module_name = match self.current_token() {
                Token::Identifier(ref name) => {
                    let nm = name.clone();
                    self.advance(); // consume identifier
                    nm
                }
                ref other => {
                    return Err(self.error(format!(
                        "Expected module name after 'import', got {:?}",
                        other
                    )));
                }
            };

            // optional 'as' alias
            let mut alias = None;
            if self.current_token() == Token::As {
                self.advance(); // consume 'as'
                match self.current_token() {
                    Token::Identifier(ref alias_name) => {
                        alias = Some(alias_name.clone());
                        self.advance();
                    }
                    ref other => {
                        return Err(
                            self.error(format!("Expected alias name after 'as', got {:?}", other))
                        );
                    }
                }
            }

            // 追加
            names.push(ImportName {
                name: module_name,
                as_name: alias,
            });

            // 次のトークンが ',' なら続ける
            if self.current_token() == Token::Comma {
                self.advance(); // consume ','
            } else {
                break; // import 列挙が終わった
            }
        }

        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }

        Ok(AstNode::Import {
            module: None,
            names,
        })
    }

    fn parse_from_import(&mut self) -> ParseResult<AstNode> {
        // 例: "from <module> import <item>[, <item>]..."
        // 1) "from" はすでに消費済み
        // 2) expect module name => Identifier
        let module_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            other => {
                return Err(self.error(format!(
                    "Expected module name after 'from', got {:?}",
                    other
                )));
            }
        };

        // 3) expect "import"
        if self.current_token() != Token::Import {
            return Err(self.error(format!(
                "Expected 'import', found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume 'import'

        let mut names = Vec::new();

        loop {
            // 4) expect item name => Identifier
            let item_name = match self.current_token() {
                Token::Identifier(ref s) => {
                    let name = s.clone();
                    self.advance();
                    name
                }
                other => {
                    return Err(self.error(format!(
                        "Expected identifier after 'import', got {:?}",
                        other
                    )));
                }
            };

            // もし "as alias" があれば読む
            let mut alias = None;
            if self.current_token() == Token::As {
                self.advance(); // consume 'as'
                match self.current_token() {
                    Token::Identifier(ref s) => {
                        alias = Some(s.clone());
                        self.advance();
                    }
                    other => {
                        return Err(
                            self.error(format!("Expected alias name after 'as', got {:?}", other))
                        );
                    }
                }
            }

            names.push(ImportName {
                name: item_name,
                as_name: alias,
            });

            // 次のトークンが ',' なら続ける
            if self.current_token() == Token::Comma {
                self.advance(); // consume ','
            } else {
                break; // import 列挙が終わった
            }
        }

        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }

        Ok(AstNode::Import {
            module: Some(module_name),
            names,
        })
    }

    fn parse_enum(&mut self) -> ParseResult<AstNode> {
        let enum_start_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let enum_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => return Err(self.error(format!("Expected enum name, found {:?}", other))),
        };
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after enum name, found {:?}",
                self.current_token()
            )));
        }
        self.advance();

        let mut variants = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let variant_name = match self.current_token() {
                Token::Identifier(ref s) => {
                    let name = s.clone();
                    self.advance();
                    name
                }
                ref other => {
                    return Err(self.error(format!("Expected enum variant name, found {:?}", other)))
                }
            };
            variants.push(variant_name);
            if self.current_token() == Token::Comma {
                self.advance();
            }
        }

        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of enum definition"));
        }
        let enum_end_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let enum_span = Span::new(enum_start_span.start, enum_end_span.end);

        Ok(AstNode::EnumDef {
            name: enum_name,
            variants,
            span: enum_span,
        })
    }

    fn parse_struct(&mut self) -> ParseResult<AstNode> {
        let struct_start_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        // たとえば "struct Point { x: int64; y: int64; }"
        // 1) expect Identifier (構造体名)
        let struct_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance(); // consume the identifier
                name
            }
            ref other => return Err(self.error(format!("Expected struct name, found {:?}", other))),
        };

        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after struct name, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        // 3) parse fields until '}'
        let mut fields = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            // 例: "x: int64;" のようなフィールド宣言を parse
            let field = self.parse_struct_field()?;
            fields.push(field);
        }

        // 4) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of struct definition"));
        }
        let struct_end_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let struct_span = Span::new(struct_start_span.start, struct_end_span.end);

        // ここでASTノードを返す
        // 構造体を表すAstNodeを追加するなり、VarDeclのようなものを使うなり好きに
        Ok(AstNode::StructDef {
            name: struct_name,
            fields,
            span: struct_span,
        })
    }

    fn parse_struct_field(&mut self) -> ParseResult<StructField> {
        self.skip_indent();
        // expect Identifier (フィールド名)
        let field_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let f = s.clone();
                self.advance();
                f
            }
            ref other => return Err(self.error(format!("Expected field name, found {:?}", other))),
        };

        // expect ':'
        if self.current_token() != Token::Colon {
            return Err(self.error(format!(
                "Expected ':' after field name, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume ':'

        // expect type => Identifier
        let field_type = self.parse_type()?;

        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }

        Ok(StructField {
            name: field_name,
            ty: field_type,
        })
    }

    fn parse_decorators(&mut self) -> ParseResult<Vec<Decorator>> {
        let mut result = Vec::new();
        loop {
            self.skip_indent();
            match self.current_token() {
                Token::Decorator(name) => {
                    self.advance(); // consume the decorator
                    let mut args = Vec::new();
                    if self.current_token() == Token::LParen {
                        self.advance(); // consume '('
                        while self.current_token() != Token::RParen {
                            let arg = self.parse_expr()?;
                            args.push(arg);
                            if self.current_token() == Token::Comma {
                                self.advance(); // consume ','
                            }
                        }
                        self.expect(&Token::RParen)?;
                    }
                    result.push(Decorator { name, args });
                }
                _ => break,
            }
        }
        Ok(result)
    }

    /// class定義: class クラス名 { ... }
    fn parse_class(&mut self) -> ParseResult<AstNode> {
        let class_start_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        // expect identifier
        let class_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let n = s.clone();
                self.advance();
                n
            }
            ref other => {
                return Err(self.error(format!("Expected class name, found {:?}", other)));
            }
        };

        let mut parents = Vec::new();
        if self.current_token() == Token::LParen {
            self.advance(); // consume '('
            while self.current_token() != Token::RParen {
                let parent = match self.current_token() {
                    Token::Identifier(ref s) => {
                        let n = s.clone();
                        self.advance();
                        n
                    }
                    ref other => {
                        return Err(
                            self.error(format!("Expected parent class name, found {:?}", other))
                        );
                    }
                };
                parents.push(parent);
                if self.current_token() == Token::Comma {
                    self.advance(); // consume ','
                }
            }
            self.expect(&Token::RParen)?;
        }

        // expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error(format!(
                "Expected '{{' after class name, found {:?}",
                self.current_token()
            )));
        }
        self.advance(); // consume '{'

        // parse members until '}'
        let mut members = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let member = self.parse_statement(true)?;
            members.push(member);
        }

        // expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' in class definition"));
        }
        let class_end_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let class_span = Span::new(class_start_span.start, class_end_span.end);

        Ok(AstNode::ClassDef {
            name: class_name,
            members,
            parents,
            span: class_span,
        })
    }

    /// 関数定義: def funcName(...) -> type { ... }
    fn parse_function(
        &mut self,
        is_static: bool,
        is_private: bool,
        has_parent: bool,
        decorators: Vec<Decorator>,
    ) -> ParseResult<AstNode> {
        let func_start_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        // 関数名
        let func_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => {
                return Err(self.error(format!("Expected function name, found {:?}", other)))
            }
        };

        // expect '('
        if self.current_token() != Token::LParen {
            return Err(self.error("Expected '(' after function name"));
        }
        self.advance(); // consume '('

        // parse params
        let mut params = Vec::new();
        let mut is_first = true;
        while self.current_token() != Token::RParen && self.current_token() != Token::Eof {
            // param_name
            let param_name = match self.current_token() {
                Token::Identifier(ref s) => {
                    let p = s.clone();
                    self.advance();
                    p
                }
                ref other => {
                    return Err(self.error(format!("Expected parameter name, got {:?}", other)))
                }
            };

            if is_first && has_parent {
                if self.current_token() == Token::Colon {
                    return Err(self.error("Expected ':' in parameter"));
                }
            } else if self.current_token() == Token::Colon {
                self.advance();
            } else {
                return Err(self.error("Expected ':' in parameter"));
            }

            // param type
            let param_type = if (has_parent && is_first) {
                VarTypeField {
                    name: "parent".to_string(),
                    is_optional: false,
                    args: vec![],
                }
            } else {
                self.parse_type()?
            };

            params.push((param_name, param_type));
            is_first = false;

            // if comma => skip
            if self.current_token() == Token::Comma {
                self.advance();
            } else {
                // no comma => break
            }
        }

        // expect ')'
        if self.current_token() == Token::RParen {
            self.advance();
        } else {
            return Err(self.error("Missing ')' after function params"));
        }

        // optional '-> returnType'
        let mut return_type = None;
        if self.current_token() == Token::Arrow {
            // consume '->'
            self.advance();
            // next should be type
            let mut return_result = Some(self.parse_type()?);
            if let Some(result) = return_result {
                if result.name != "None" {
                    return_type = Some(result);
                }
            }
        }

        // expect '{'
        if self.current_token() != Token::LBrace {
            return Err(self.error("Expected '{' in function definition"));
        }
        self.advance(); // consume '{'

        // parse function body statements
        let mut body = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            body.push(stmt);
        }

        // expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of function"));
        }
        let func_end_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let func_span = Span::new(func_start_span.start, func_end_span.end);

        Ok(AstNode::FunctionDef {
            name: func_name,
            params,
            return_type,
            body,
            is_static,
            is_private,
            decorators,
            span: func_span,
        })
    }

    /// 変数宣言: (final|let) name: type (= expr)? ;
    fn parse_var_decl(
        &mut self,
        decl_token: Token,
        is_static: bool,
        is_private: bool,
    ) -> ParseResult<AstNode> {
        let decl_start_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let decl_type = match decl_token {
            Token::Final => VarDeclType::Final,
            Token::Let => VarDeclType::Let,
            Token::Const => VarDeclType::Const,
            _ => return Err(self.error("Expected 'final', 'let', or 'const'")),
        };
        // var name
        let var_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let n = s.clone();
                self.advance();
                n
            }
            ref other => {
                return Err(self.error(format!("Expected variable name, found {:?}", other)))
            }
        };

        let mut var_type = Some(String::new());
        // expect ':'
        if self.current_token() == Token::Colon {
            self.advance();
        } else if self.current_token() == Token::Assign {
            // if it's '=', set var_type to None
            var_type = None;
        } else {
            return Err(self.error("Expected ':' in variable declaration"));
        }

        // var type
        let var_type = if var_type.is_none() {
            None
        } else {
            Some(self.parse_type()?)
        };

        // optional = expr
        let mut init_expr = None;
        if self.current_token() == Token::Assign {
            self.advance(); // consume '='
            let e = self.parse_expr()?;
            init_expr = Some(e);
        }

        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }
        let decl_end_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
        let decl_span = Span::new(decl_start_span.start, decl_end_span.end);

        Ok(AstNode::VarDecl {
            name: var_name,
            var_type,
            decl_type,
            expr: init_expr,
            is_static,
            is_private,
            span: decl_span,
        })
    }

    /// ステートメント: return文 or 式文
    fn parse_statement(&mut self, has_parent: bool) -> ParseResult<AstNode> {
        self.skip_indent();
        let decorators = self.parse_decorators()?;
        match self.current_token() {
            Token::Import => {
                self.advance(); // "import" を消費
                self.parse_import()
            }
            Token::From => {
                self.advance();
                self.parse_from_import()
            }
            Token::Class => {
                self.advance(); // "class" を消費
                self.parse_class()
            }
            Token::Struct => {
                self.advance(); // consume "struct"
                self.parse_struct()
            }
            Token::Def => {
                self.advance(); // "def" を消費
                self.parse_function(false, false, has_parent, decorators)
            }
            Token::Final | Token::Let | Token::Const => {
                let decl_token = self.current_token().clone();
                self.advance(); // consume final/let/const
                self.parse_var_decl(decl_token, false, false)
            }
            Token::Enum => {
                self.advance(); // consume "enum"
                self.parse_enum()
            }
            Token::Try => {
                self.advance(); // consume "try"
                self.parse_try_stmt()
            }
            Token::Throw => {
                self.advance(); // consume "throw"
                let e = self.parse_expr()?;
                self.expect(&Token::Semicolon)?;
                Ok(AstNode::ThrowStmt(e))
            }
            Token::Static => {
                // consume 'static'
                self.advance();

                // 次に来るトークンが何かで分岐
                match self.current_token() {
                    // static let / static final => フィールド
                    Token::Let | Token::Final => {
                        let decl_token = self.current_token().clone();
                        self.advance();
                        // 変数宣言としてパース
                        // ここでAstNodeに「staticである」情報をどう保持するか検討が必要
                        self.parse_var_decl(decl_token, true, false)
                    }
                    // static def => メソッド
                    Token::Def => {
                        self.advance();
                        self.parse_function(true, false, false, decorators)
                    }
                    ref other => Err(self.error(format!(
                        "Expected 'let', 'final', or 'def' after 'static', found {:?}",
                        other
                    ))),
                }
            }
            Token::Private => {
                // consume 'private'
                self.advance();
                // 次に来るトークンが何かで分岐
                match self.current_token() {
                    // private let / private final => フィールド
                    Token::Let | Token::Final | Token::Const => {
                        let decl_token = self.current_token().clone();
                        self.advance();
                        self.parse_var_decl(decl_token, false, true)
                    }
                    Token::Def => {
                        self.advance();
                        self.parse_function(false, true, has_parent, decorators)
                    }
                    ref other => Err(self.error(format!(
                        "Expected 'let', 'final', or 'def' after 'private', found {:?}",
                        other
                    ))),
                }
            }
            Token::Return => {
                self.advance(); // consume 'return'
                                // return文の場合、式があるかもしれない
                if self.current_token() == Token::Semicolon {
                    // `return;`
                    self.advance();
                    Ok(AstNode::ReturnStmt(None))
                } else {
                    let e = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    Ok(AstNode::ReturnStmt(Some(e)))
                }
            }
            Token::Pass => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(AstNode::PassStmt)
            }
            Token::Break => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(AstNode::BreakStmt)
            }
            Token::Continue => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(AstNode::ContinueStmt)
            }
            Token::Switch => {
                self.advance(); // consume 'switch'
                self.parse_switch_stmt()
            }
            Token::If => {
                self.advance(); // consume 'if'
                self.parse_if_stmt()
            }
            Token::For => {
                self.advance(); // consume 'for'
                self.parse_for_stmt()
            }
            Token::Guard => {
                self.advance();
                self.parse_guard_stmt()
            }
            Token::Eof => Err(self.error("Unexpected EOF")),
            _ => {
                // 式ステートメントとみなす
                let expr = self.parse_expr()?;
                // optional semicolon
                if self.current_token() == Token::Semicolon {
                    self.advance();
                }
                Ok(AstNode::ExprStmt(expr))
            }
        }
    }

    fn extract_interpolations(s: &str) -> Vec<String> {
        let mut vars = Vec::new();
        let mut start = 0;
        while let Some(open_index) = s[start..].find('{') {
            let open_index = start + open_index;
            // 次の '}' を探す
            if let Some(close_index_rel) = s[open_index + 1..].find('}') {
                let close_index = open_index + 1 + close_index_rel;
                let var = s[open_index + 1..close_index].trim();
                if !var.is_empty() {
                    vars.push(var.to_string());
                }
                start = close_index + 1;
            } else {
                break;
            }
        }
        vars
    }

    /// 式のエントリポイント。最も低い優先順位である代入式から開始する。
    pub fn parse_expr(&mut self) -> ParseResult<Expr> {
        self.parse_assignment_expr()
    }

    /// 代入式 (右結合)
    fn parse_assignment_expr(&mut self) -> ParseResult<Expr> {
        let lhs = self.parse_logical_or_expr()?;
        // 単純代入
        if self.current_token() == Token::Assign {
            self.advance();
            let rhs = self.parse_assignment_expr()?;
            return Ok(Expr::Assign {
                target: Box::new(lhs),
                value: Box::new(rhs),
            });
        }
        // 複合代入 (例: +=, -=, etc.)
        else if self.current_token() == Token::PlusEqual
            || self.current_token() == Token::MinusEqual
            || self.current_token() == Token::StarEqual
            || self.current_token() == Token::SlashEqual
            || self.current_token() == Token::PercentEqual
        {
            let op = match self.current_token() {
                Token::PlusEqual => BinOp::Plus,
                Token::MinusEqual => BinOp::Minus,
                Token::StarEqual => BinOp::Star,
                Token::SlashEqual => BinOp::Slash,
                Token::PercentEqual => BinOp::Percent,
                _ => unreachable!(),
            };
            self.advance();
            let rhs = self.parse_assignment_expr()?;
            return Ok(Expr::CompoundAssign {
                op,
                target: Box::new(lhs),
                value: Box::new(rhs),
            });
        } else if self.current_token() == Token::RangeHalfOpen
            || self.current_token() == Token::RangeClosed
        {
            let op = match self.current_token() {
                Token::RangeHalfOpen => BinOp::RangeHalfOpen,
                Token::RangeClosed => BinOp::RangeClosed,
                _ => unreachable!(),
            };
            self.advance();
            let rhs = self.parse_assignment_expr()?;
            return Ok(Expr::Range {
                start: Box::new(lhs),
                end: Box::new(rhs),
                op,
            });
        }
        match self.current_token() {
            Token::StringLiteral(ref s) => {
                let st = s.clone();
                self.advance();
                match lhs {
                    Expr::Identifier { name, span } => {
                        if name == "f" {
                            let vars = Self::extract_interpolations(&st);
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Format,
                                vars,
                            });
                        } else if name == "b" {
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Raw,
                                vars: vec![],
                            });
                        } else if name == "r" {
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Regex,
                                vars: vec![],
                            });
                        }
                        return Ok(Expr::Identifier { name, span });
                    }
                    _ => {
                        return Ok(lhs);
                    }
                }
            }
            _ => {
                return Ok(lhs);
            }
        }
    }

    /// 論理和 (OR) のパース
    fn parse_logical_or_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_logical_and_expr()?;
        while self.current_token() == Token::Or {
            self.advance();
            let rhs = self.parse_logical_and_expr()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinOp::Or,
                right: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    /// 論理積 (AND) のパース
    fn parse_logical_and_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_equality_expr()?;
        while self.current_token() == Token::And {
            self.advance();
            let rhs = self.parse_equality_expr()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                op: BinOp::And,
                right: Box::new(rhs),
            };
        }
        Ok(expr)
    }

    /// 等価・不等価のパース
    fn parse_equality_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_relational_expr()?;
        loop {
            match self.current_token() {
                Token::EqualEqual => {
                    self.advance();
                    let rhs = self.parse_relational_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Equal,
                        right: Box::new(rhs),
                    };
                }
                Token::NotEqual => {
                    self.advance();
                    let rhs = self.parse_relational_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::NotEqual,
                        right: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// 比較 (関係) 演算子 (<, <=, >, >=) のパース
    fn parse_relational_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_additive_expr()?;
        loop {
            match self.current_token() {
                Token::Less => {
                    self.advance();
                    let rhs = self.parse_additive_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::LessThan,
                        right: Box::new(rhs),
                    };
                }
                Token::LessEqual => {
                    self.advance();
                    let rhs = self.parse_additive_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::LessThanOrEqual,
                        right: Box::new(rhs),
                    };
                }
                Token::Greater => {
                    self.advance();
                    let rhs = self.parse_additive_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::GreaterThan,
                        right: Box::new(rhs),
                    };
                }
                Token::GreaterEqual => {
                    self.advance();
                    let rhs = self.parse_additive_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::GreaterThanOrEqual,
                        right: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// 加算・減算のパース
    fn parse_additive_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_multiplicative_expr()?;
        loop {
            match self.current_token() {
                Token::Plus => {
                    self.advance();
                    let rhs = self.parse_multiplicative_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Plus,
                        right: Box::new(rhs),
                    };
                }
                Token::Minus => {
                    self.advance();
                    let rhs = self.parse_multiplicative_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Minus,
                        right: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// 乗算・除算・剰余のパース
    fn parse_multiplicative_expr(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary_expr()?;
        loop {
            match self.current_token() {
                Token::Star => {
                    self.advance();
                    let rhs = self.parse_unary_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Star,
                        right: Box::new(rhs),
                    };
                }
                Token::Slash => {
                    self.advance();
                    let rhs = self.parse_unary_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Slash,
                        right: Box::new(rhs),
                    };
                }
                Token::Percent => {
                    self.advance();
                    let rhs = self.parse_unary_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Percent,
                        right: Box::new(rhs),
                    };
                }
                Token::StarStar => {
                    self.advance();
                    let rhs = self.parse_unary_expr()?;
                    expr = Expr::Binary {
                        left: Box::new(expr),
                        op: BinOp::Power,
                        right: Box::new(rhs),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    /// 単項演算子のパース (-, !など)
    fn parse_unary_expr(&mut self) -> ParseResult<Expr> {
        match self.current_token() {
            Token::Minus => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Negate,
                    expr: Box::new(expr),
                })
            }
            Token::Bang => {
                self.advance();
                let expr = self.parse_unary_expr()?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    expr: Box::new(expr),
                })
            }
            _ => self.parse_primary_expr(),
        }
    }

    fn parse_primary_expr(&mut self) -> ParseResult<Expr> {
        let tok = self.current_token();
        let mut expr = match tok {
            // Identifier => 関数呼び出しの可能性もチェック
            Token::Identifier(ref name) => {
                let ident_name = name.clone();
                self.advance(); // consume identifier
                let ident_span = self.previous_span().unwrap_or_else(|| self.fallback_span());

                // もし次が '(' なら関数呼び出し
                if self.current_token() == Token::LParen {
                    self.parse_call_expr(Expr::Identifier {
                        name: ident_name,
                        span: ident_span,
                    })?
                } else {
                    // 単なる識別子
                    Expr::Identifier {
                        name: ident_name,
                        span: ident_span,
                    }
                }
            }
            // リテラル
            Token::IntLiteral(n) => {
                self.advance();
                Expr::IntLiteral(n)
            }
            Token::FloatLiteral(f) => {
                self.advance();
                Expr::FloatLiteral(f)
            }
            Token::StringLiteral(ref s) => {
                let st = s.clone();
                self.advance();
                Expr::StringLiteral {
                    value: st,
                    ty: StringLiteralType::Regular,
                    vars: vec![],
                }
            }
            Token::LBracket => self.parse_array_literal()?,
            Token::LBrace => self.parse_dict_or_function_expr()?,
            Token::LParen => self.parse_paren_expr()?,
            Token::Dot => {
                self.advance();
                let dot_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
                match self.current_token() {
                    Token::Identifier(ref field_name) => {
                        let field = field_name.clone();
                        let ident_span =
                            self.current_span().unwrap_or_else(|| self.fallback_span());
                        self.advance();
                        let span = Span::new(dot_span.start, ident_span.end);
                        Expr::MemberAccess {
                            target: None,
                            member: field,
                            span,
                        }
                    }
                    ref other => {
                        return Err(
                            self.error(format!("Expected identifier after '.', found {:?}", other))
                        );
                    }
                }
            }
            Token::True | Token::False => {
                self.advance();
                Expr::BoolLiteral(tok == Token::True)
            }
            Token::None => {
                self.advance();
                Expr::NoneLiteral
            }
            ref other => {
                return Err(self.error(format!("Unexpected token in expression: {:?}", other)));
            }
        };
        loop {
            if self.current_token() == Token::Dot {
                self.advance();
                let dot_span = self.previous_span().unwrap_or_else(|| self.fallback_span());
                match self.current_token() {
                    Token::Identifier(ref field_name) => {
                        let field = field_name.clone();
                        let ident_span =
                            self.current_span().unwrap_or_else(|| self.fallback_span());
                        self.advance();
                        let span = Span::new(dot_span.start, ident_span.end);
                        let member_expr = Expr::MemberAccess {
                            target: Some(Box::new(expr)),
                            member: field,
                            span,
                        };
                        expr = member_expr;
                    }
                    ref other => {
                        return Err(
                            self.error(format!("Expected identifier after '.', found {:?}", other))
                        );
                    }
                }
            } else if self.current_token() == Token::LParen {
                expr = self.parse_call_expr(expr)?;
            } else if self.current_token() == Token::LBracket {
                expr = self.parse_index_expr(expr)?;
            } else if self.current_token() == Token::Question {
                self.advance();
                expr = Expr::OptionalChaining {
                    target: Box::new(expr),
                }
            } else if self.current_token() == Token::Bang {
                self.advance();
                expr = Expr::Unwrap {
                    target: Box::new(expr),
                }
            } else if self.current_token() == Token::QuestionQuestion {
                self.advance();
                let right = self.parse_expr()?;
                expr = Expr::NullCoalescing {
                    left: Box::new(expr),
                    right: Box::new(right),
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_paren_expr(&mut self) -> ParseResult<Expr> {
        // 1) consume '('
        self.expect(&Token::LParen)?;

        // 2) 判定: もし次がラムダパラメータ形式 (例: `x: uint32`) ならラムダ
        //          そうでなければ(または失敗すれば) タプル とみなす
        if self.check_if_lambda()? {
            // parse lambda
            self.parse_lambda_expr()
        } else {
            // parse tuple
            self.parse_tuple_expr()
        }
    }

    fn check_if_lambda(&mut self) -> ParseResult<bool> {
        let saved_pos = self.pos;

        // スキップ空白など
        self.skip_indent();

        // もしすぐ ')' なら空タプル
        if self.current_token() == Token::RParen {
            self.pos = saved_pos;
            return Ok(false);
        }

        // パターン: ( param1: Type, param2: Type ) => ...
        //           ( param1: Type, param2: Type ) { ... }
        // ただし param0 が無いラムダ "()" => ... は要件次第で対応可

        // 先に "Identifier ':'" が並んでいるかチェック
        let mut saw_any_param = false;

        loop {
            // expect Identifier
            match self.current_token() {
                Token::Identifier(_) => {
                    self.advance();
                }
                _ => {
                    // パラメータ列終わり or ダメ
                    self.pos = saved_pos;
                    return Ok(false);
                }
            }

            // expect ':'
            if self.current_token() != Token::Colon {
                // これでラムダじゃない
                self.pos = saved_pos;
                return Ok(false);
            }
            self.advance(); // consume ':'

            // 型をざっくり読み飛ばす (1トークン～複数トークン?) → ここでは1トークンだけ確認にする例
            // 実際には parse_type() の先頭だけ覗き見するなどが必要
            match self.current_token() {
                Token::Identifier(_) => {
                    self.advance(); // consume the type name
                }
                // ここで "[" / "{" / "(" などが来れば複合型かもしれないが省略
                _ => {
                    self.pos = saved_pos;
                    return Ok(false);
                }
            }

            saw_any_param = true;

            // カンマがあればパラメータ続行
            if self.current_token() == Token::Comma {
                self.advance();
                continue;
            } else {
                break;
            }
        }

        if !saw_any_param {
            // パラメータなし => lambda? 仕様に合わせる
            self.pos = saved_pos;
            return Ok(false);
        }

        // 次が ')' で、その後 '=>' or '{' ならラムダ
        if self.current_token() == Token::RParen {
            self.advance(); // consume ')'

            // ここで '=>' or '{' のどちらかを確認
            if self.current_token() == Token::FatArrow || self.current_token() == Token::LBrace {
                // OK => lambda
                self.pos = saved_pos;
                return Ok(true);
            }
        }

        self.pos = saved_pos;
        Ok(false)
    }

    fn parse_lambda_expr(&mut self) -> ParseResult<Expr> {
        // すでに '(' は消費済み in parse_paren_expr().

        let mut params = Vec::new();

        // parse param list
        loop {
            // expect Identifier
            let param_name = match self.current_token() {
                Token::Identifier(ref s) => s.clone(),
                ref other => {
                    return Err(self.error(format!("Expected param name, got {:?}", other)))
                }
            };
            self.advance();

            // expect ':'
            self.expect(&Token::Colon)?;

            // parse type (簡易: ここで parse_type() 呼ぶなど)
            let param_type = self.parse_type()?;
            // ここでは固定サンプル; 実際は self.parse_type() する
            params.push((param_name, param_type));

            // カンマがあればループ続行
            if self.current_token() == Token::Comma {
                self.advance();
                continue;
            } else {
                break;
            }
        }

        // expect ')'
        self.expect(&Token::RParen)?;

        // 次が '=>' なら 短い式ラムダ
        if self.current_token() == Token::FatArrow {
            // `=>`
            self.advance(); // consume '=>'

            // parse single expression
            let body_expr = self.parse_expr()?;
            Ok(Expr::Lambda {
                params,
                body: LambdaBody::Expr(Box::new(body_expr)),
            })
        }
        // 次が '{' なら ブロックラムダ
        else if self.current_token() == Token::LBrace {
            self.advance(); // consume '{'
            let mut stmts = Vec::new();
            loop {
                self.skip_indent();
                if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                    break;
                }
                let stmt = self.parse_statement(false)?; // ここは好みに合わせる
                stmts.push(stmt);
            }
            self.expect(&Token::RBrace)?;
            Ok(Expr::Lambda {
                params,
                body: LambdaBody::Block(stmts),
            })
        } else {
            Err(self.error("Expected '=>' or '{' after lambda parameter list"))
        }
    }

    fn parse_tuple_expr(&mut self) -> ParseResult<Expr> {
        // '(' は呼び出し元で consume済み
        // 要素が0個以上で、')' まで読み込む
        let mut elems = Vec::new();

        // check if next is ')': empty tuple
        if self.current_token() == Token::RParen {
            // consume ')'
            self.advance();
            // 空タプル
            return Ok(Expr::TupleLiteral(elems));
        }

        loop {
            // parse element as expression
            let elem = self.parse_expr()?;
            elems.push(elem);

            // if comma => continue
            if self.current_token() == Token::Comma {
                self.advance(); // consume ','
                continue;
            }

            // else => break
            break;
        }

        // expect ')'
        self.expect(&Token::RParen)?;

        if elems.len() == 1 {
            // 要素が1つならタプルではなくその要素そのもの
            return Ok(elems.remove(0));
        }
        Ok(Expr::TupleLiteral(elems))
    }

    fn parse_dict_or_function_expr(&mut self) -> ParseResult<Expr> {
        // 1) consume '{'
        self.expect(&Token::LBrace)?;

        // 2) 判定: この後が "param1, param2, ... in" という形ならFunctionリテラル
        //           そうでなければ辞書とみなす
        if self.check_if_function_literal()? {
            // 2-1) parse as function-literal
            self.parse_function_literal_body()
        } else if self.check_if_zero_param_function_literal()? {
            // 2-2) 引数なしの関数リテラル
            self.parse_zero_param_function_literal_body()
        } else {
            // 2-3) parse as dict-literal
            self.parse_dict_literal_body()
        }
    }

    fn parse_dict_literal_body(&mut self) -> ParseResult<Expr> {
        let mut pairs = Vec::new();

        loop {
            self.skip_indent();

            // もし次が '}' なら辞書終了
            if self.current_token() == Token::RBrace {
                // consume '}'
                self.advance();
                // 生成して返す
                return Ok(Expr::DictLiteral(pairs));
            }

            // parse key
            // 例:  "test" : <expr>  or  identifier : <expr>
            let key_expr = match self.current_token() {
                // 文字列キー
                Token::StringLiteral(ref s) => {
                    let st = s.clone();
                    self.advance();
                    Expr::StringLiteral {
                        value: st,
                        ty: crate::parser::StringLiteralType::Regular,
                        vars: vec![],
                    }
                }
                // 識別子キー
                Token::Identifier(ref name) => {
                    let nm = name.clone();
                    self.advance();
                    let span = self.previous_span().unwrap_or_else(|| self.fallback_span());
                    Expr::Identifier { name: nm, span }
                }
                // 他はエラー
                ref other => {
                    return Err(self.error(format!(
                        "Expected string literal or identifier as dict key, found {:?}",
                        other
                    )));
                }
            };

            // expect ':'
            if self.current_token() != Token::Colon {
                return Err(self.error(format!(
                    "Expected ':' after dict key, found {:?}",
                    self.current_token()
                )));
            }
            self.advance(); // consume ':'

            // parse value
            let value_expr = self.parse_expr()?;

            // push to pairs
            pairs.push((key_expr, value_expr));

            // optional comma
            if self.current_token() == Token::Comma {
                self.advance(); // consume ','
                continue;
            }
            // else loop
        }
    }

    fn parse_function_literal_body(&mut self) -> ParseResult<Expr> {
        // いま '{' はすでに消費済み

        let mut params = Vec::new();

        // 1) 解析: param1, param2, ... in
        //    ※ check_if_function_literal で大まかに確認済だが、ここで本実装を行う
        loop {
            match self.current_token() {
                Token::Identifier(ref s) => {
                    let param_name = s.clone();
                    self.advance(); // consume identifier
                                    // 型指定は無いと仮定 / param_type = Something
                                    // 例: ("param1", VarTypeField { name="Any" ... })
                    params.push((
                        param_name,
                        VarTypeField {
                            name: "Any".to_string(),
                            is_optional: false,
                            args: vec![],
                        },
                    ));
                }
                Token::Comma => {
                    self.advance(); // consume ','
                    continue; // parse next param
                }
                Token::In => {
                    self.advance(); // consume 'in'
                    break;
                }
                ref other => {
                    return Err(
                        self.error(format!("Expected param name or 'in', found {:?}", other))
                    );
                }
            }
        }

        // parse statements until '}'
        let mut body_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            // ここはお使いの parse_statement() などを再利用
            let stmt = self.parse_statement(false)?;
            body_stmts.push(stmt);
        }

        // expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of closure body"));
        }

        // 3) 関数リテラル作成
        // 今回は return_type=None, body=body_stmts
        Ok(Expr::Function {
            params,
            body: body_stmts,
        })
    }

    fn parse_zero_param_function_literal_body(&mut self) -> ParseResult<Expr> {
        let mut body_stmts = Vec::new();

        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement(false)?;
            body_stmts.push(stmt);
        }

        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err(self.error("Missing '}' at end of closure body"));
        }

        Ok(Expr::Function {
            params: Vec::new(),
            body: body_stmts,
        })
    }

    /// 簡易実装: { param1, param2 in ... } とマッチすればtrue
    fn check_if_function_literal(&mut self) -> ParseResult<bool> {
        // 一時的に現在のposを保存
        let saved_pos = self.pos;

        // スキップインデントや空白などがあればスキップ (任意)
        self.skip_indent();

        // もしすぐ '}' があるなら空辞書 -> function ではない
        if self.current_token() == Token::RBrace {
            self.pos = saved_pos;
            return Ok(false);
        }

        // 例: param1, param2 in ...
        // 連続して "Identifier" "," "Identifier" ... "in" の形をチェック
        // ただし 0パラメータクロージャ { in ...} を許可したい場合は別途対応

        // 最低限: 1個以上の Identifier
        // 末尾に "in" がなければ function ではない
        let mut saw_identifier = false;

        // まず1つめ
        match self.current_token() {
            Token::Identifier(_) => {
                self.advance(); // consume
                saw_identifier = true;
            }
            // ここで } や他のものが来たら -> dict かな？
            _ => {
                self.pos = saved_pos;
                return Ok(false);
            }
        }

        // その後0回以上の (',' Identifier)
        loop {
            if self.current_token() == Token::Comma {
                self.advance(); // consume ','
                                // expect Identifier
                if let Token::Identifier(_) = self.current_token() {
                    self.advance();
                } else {
                    // ここでダメなら functionでない -> revert
                    self.pos = saved_pos;
                    return Ok(false);
                }
            } else {
                break;
            }
        }

        // 次トークンが 'in' なら function
        if self.current_token() == Token::In {
            // OK => function-literal
            // 位置を復元しないまま true を返すと、呼び出し元がもう1回 parseする際に混乱する
            // → ただし本実装では parse_dict_or_function_expr() が expect('{') したあとに check してる
            //    => ここでは parseそのものをしていないので tokensはいじっている
            self.pos = saved_pos;
            return Ok(true);
        }

        // そうでなければ functionじゃない
        self.pos = saved_pos;
        Ok(false)
    }

    fn check_if_zero_param_function_literal(&mut self) -> ParseResult<bool> {
        let saved_pos = self.pos;
        self.skip_indent();

        if self.current_token() == Token::RBrace {
            self.pos = saved_pos;
            return Ok(false);
        }

        let looks_like_dict_key = match self.current_token() {
            Token::Identifier(_) | Token::StringLiteral(_) => {
                matches!(self.peek_non_indent_token(1), Token::Colon)
            }
            _ => false,
        };

        self.pos = saved_pos;
        Ok(!looks_like_dict_key)
    }

    fn parse_call_expr(&mut self, callee_expr: Expr) -> ParseResult<Expr> {
        // assume current_token() == Token::LParen => consume it
        if self.current_token() == Token::LParen {
            self.advance(); // consume '('
        } else {
            return Err(self.error("Expected '(' in call expression"));
        }

        let mut args = Vec::new();
        while self.current_token() != Token::RParen && self.current_token() != Token::Eof {
            let arg_expr = self.parse_expr()?;
            args.push(arg_expr);
            if self.current_token() == Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        // expect ')'
        if self.current_token() == Token::RParen {
            self.advance();
        } else {
            return Err(self.error("Missing ')' in function call"));
        }

        // return a Call node
        Ok(Expr::Call {
            callee: Box::new(callee_expr),
            args,
        })
    }

    fn parse_index_expr(&mut self, base_expr: Expr) -> ParseResult<Expr> {
        // expect '['
        self.expect(&Token::LBracket)?;

        // parse the index expression (example: `i`, or `i + 1`, or function call, etc.)
        let index_expr = self.parse_expr()?;

        // expect ']'
        self.expect(&Token::RBracket)?;

        Ok(Expr::Index {
            target: Box::new(base_expr),
            index: Box::new(index_expr),
        })
    }

    fn parse_array_literal(&mut self) -> ParseResult<Expr> {
        // consume '['
        self.expect(&Token::LBracket)?;

        let mut elements = Vec::new();

        // チェック: 空配列かもしれない → next が ']' なら空
        if self.current_token() != Token::RBracket {
            loop {
                // parse_expr() で要素を読む
                let elem = self.parse_expr()?;
                elements.push(elem);

                // 次が ',' なら読み続ける
                if self.current_token() == Token::Comma {
                    self.advance(); // consume ','
                } else {
                    break;
                }
            }
        }

        // expect ']'
        self.expect(&Token::RBracket)?;

        Ok(Expr::ArrayLiteral(elements))
    }

    /// 現在のトークンを返す (pos は進めない)
    fn current_token(&self) -> Token {
        if self.pos >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[self.pos].token.clone()
        }
    }

    /// n個先のトークンを覗き見
    fn peek_token(&self, n: usize) -> Token {
        let idx = self.pos + n;
        if idx >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[idx].token.clone()
        }
    }

    fn current_span(&self) -> Option<Span> {
        self.tokens.get(self.pos).map(|tok| tok.span)
    }

    fn peek_span(&self, n: usize) -> Option<Span> {
        self.tokens.get(self.pos + n).map(|tok| tok.span)
    }

    fn previous_span(&self) -> Option<Span> {
        if self.pos == 0 {
            None
        } else {
            self.tokens.get(self.pos - 1).map(|tok| tok.span)
        }
    }

    fn peek_non_indent_token(&self, n: usize) -> Token {
        let mut idx = self.pos + n;
        while idx < self.tokens.len() {
            match &self.tokens[idx].token {
                Token::Indent(_) => idx += 1,
                Token::Newline => idx += 1,
                token => return token.clone(),
            }
        }
        Token::Eof
    }

    /// 今のトークンを返して pos を1進める
    fn advance(&mut self) -> Token {
        let tk = self.current_token();
        self.pos += 1;
        tk
    }
}

pub fn parse_inline_expr(source: &str) -> ParseResult<Expr> {
    let source_arc: Arc<str> = Arc::from(source);
    let mut lexer = Lexer::new(&source_arc);
    let tokens = lexer.tokenize();
    let mut parser = Parser::new(source_arc, tokens);
    let expr = parser.parse_expr()?;
    match parser.current_token() {
        Token::Eof => Ok(expr),
        unexpected => Err(parser.error_at(
            parser.current_span(),
            format!("Unexpected token {:?} in inline expression", unexpected),
        )),
    }
}
