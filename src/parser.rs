use crate::token::Token;

/// ========================
///        AST定義
/// ========================

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    /// ソースファイル全体
    File(Vec<AstNode>),

    /// import 文 (例: `import os;`)
    Import {
        module_name: String,
    },

    /// class定義 (例: `class User { ... }`)
    ClassDef {
        name: String,
        members: Vec<AstNode>,
        parents: Vec<String>,
    },

    /// 関数定義 (例: `def greet(user: User) -> None { ... }`)
    FunctionDef {
        name: String,
        params: Vec<(String, VarTypeField)>,   // (param_name, param_type)
        return_type: Option<VarTypeField>,     // -> Some("None")など
        body: Vec<AstNode>,              // 関数本体のステートメント
        is_static: bool,                 // static 修飾子があれば true
        decorators: Vec<Decorator>,      // デコレータ (@deprecated など)
    },

    StructDef {
        name: String,
        fields: Vec<StructField>,
    },

    /// 変数宣言 (例: `final name: str = "Alice";`)
    VarDecl {
        name: String,
        var_type: Option<VarTypeField>,
        decl_type: VarDeclType,
        expr: Option<Expr>, // 初期化式
        is_static: bool,    // static 修飾子があれば true
    },

    EnumDef {
        name: String,
        variants: Vec<String>,
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

    ForStmt {
        var_name: String,
        iterable: Expr,
        body: Vec<AstNode>,
    },

    PassStmt,
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
    pub pattern: String,         // 例: "admin", "user", etc.
    pub body: Vec<AstNode>,      // caseの中身
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decorator {
    pub name: String,
    pub args: Vec<Expr>,
}

/// 式(Expressions)のAST
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral {
        value: String,
        ty: StringLiteralType,
    },

    /// 関数呼び出し: foo(...) 
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
    },

    /// 2項演算子など(今回はサンプル)
    Binary {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },

    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },

    MemberAccess {
        target: Box<Expr>,
        member: String,
    },

    CompoundAssign {
        op: BinOp,
        target: Box<Expr>,
        value: Box<Expr>,
    },

    ArrayLiteral(Vec<Expr>),

    Range {
        start: Box<Expr>,
        end: Box<Expr>,
        op: BinOp,
    },

    Index {
        target: Box<Expr>,
        index: Box<Expr>,
    }
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

/// ========================
///        パーサー本体
/// ========================

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    /// コンストラクタ
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
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
    pub fn expect(&mut self, expected: &Token) -> Result<Token, String> {
        let current = self.current_token();
        if current == *expected {
            // OK → advance() して返す
            self.advance();
            Ok(current)
        } else {
            // NG → エラー
            Err(format!("Expected {:?}, but found {:?}", expected, current))
        }
    }

    /// parse_type():
    ///  1) parse union type (one or more primary types joined by '|')
    ///  2) if there's a trailing '?', mark is_optional = true
    pub fn parse_type(&mut self) -> Result<VarTypeField, String> {
        // 1) parse union
        let mut types = vec![ self.parse_primary_type()? ];

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
    fn parse_primary_type(&mut self) -> Result<VarTypeField, String> {
        match self.current_token() {
            // function(...) -> T
            Token::Identifier(ref s) if s == "function" => {
                self.parse_function_type()
            }
            // [ T ]
            Token::LBracket => {
                self.parse_array_type()
            }
            // { K : V }
            Token::LBrace => {
                self.parse_dict_type()
            }
            // ( T1, T2, ... )
            Token::LParen => {
                self.parse_tuple_type()
            }
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
                Err(format!("parse_primary_type: Unexpected token: {:?}", other))
            }
        }
    }

    /// function(uint64) -> str
    /// function(...) -> Type
    fn parse_function_type(&mut self) -> Result<VarTypeField, String> {
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
            return Err(format!("Expected '->' in function type, got {:?}", self.current_token()));
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
    fn parse_array_type(&mut self) -> Result<VarTypeField, String> {
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
    fn parse_dict_type(&mut self) -> Result<VarTypeField, String> {
        // consume '{'
        self.expect(&Token::LBrace)?;

        // parse key type
        let key_ty = self.parse_type()?;

        // expect ':'
        if self.current_token() != Token::Colon {
            return Err(format!("Expected ':' in dict type, found {:?}", self.current_token()));
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

    fn parse_tuple_type(&mut self) -> Result<VarTypeField, String> {
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
    pub fn parse_file(&mut self) -> Result<AstNode, String> {
        let mut items = Vec::new();

        while self.current_token() != Token::Eof {
            // まず Indent を飛ばす
            self.skip_indent();
            // トップレベル要素を1つずつ解析
            let item = self.parse_top_level_item()?;
            items.push(item);
        }

        Ok(AstNode::File(items))
    }

    /// トップレベル要素 (import, class, def など) を判別
    fn parse_top_level_item(&mut self) -> Result<AstNode, String> {
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
                self.parse_function(false, false, decorators)
            }
            Token::Final | Token::Let | Token::Const => {
                let decl_token = self.current_token().clone();
                self.advance(); // consume final/let/const
                self.parse_var_decl(decl_token, false)
            }
            Token::Enum => {
                self.advance(); // consume "enum"
                self.parse_enum()
            }
            Token::Eof => Err("Unexpected EOF".to_string()),
            ref other => Err(format!("Unexpected token in top-level: {:?}", other)),
        }
    }

    fn parse_switch_stmt(&mut self) -> Result<AstNode, String> {
        // 1) parse the expression after "switch"
        let switch_expr = self.parse_expr()?;
    
        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(format!("Expected '{{' after switch expression, found {:?}", self.current_token()));
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
                                return Err("Expected identifier after '.' in case pattern".to_string());
                            }
                        }
                        ref other => {
                            return Err(format!("Expected identifier after 'case', found {:?}", other));
                        }
                    };
    
                    // expect '{'
                    if self.current_token() != Token::LBrace {
                        return Err(format!("Expected '{{' after case pattern, found {:?}", self.current_token()));
                    }
                    self.advance(); // consume '{'
    
                    // parse statements until '}'
                    let mut body_stmts = Vec::new();
                    loop {
                        self.skip_indent();
                        if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                            break;
                        }
                        let stmt = self.parse_statement()?;
                        body_stmts.push(stmt);
                    }
    
                    // expect '}'
                    if self.current_token() == Token::RBrace {
                        self.advance();
                    } else {
                        return Err("Missing '}' after case body".to_string());
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
                        return Err(format!("Expected '{{' after default, found {:?}", self.current_token()));
                    }
                    self.advance(); // consume '{'
    
                    let mut body_stmts = Vec::new();
                    loop {
                        self.skip_indent();
                        if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                            break;
                        }
                        let stmt = self.parse_statement()?;
                        body_stmts.push(stmt);
                    }
    
                    if self.current_token() == Token::RBrace {
                        self.advance();
                    } else {
                        return Err("Missing '}' after default body".to_string());
                    }
                    default = Some(body_stmts);
                }
    
                // unexpected token
                ref other => {
                    return Err(format!("Expected 'case' or 'default', found {:?}", other));
                }
            }
        }
    
        // 4) now expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else if self.current_token() != Token::Eof {
            return Err(format!("Expected '}}' at end of switch, found {:?}", self.current_token()));
        }
    
        Ok(AstNode::SwitchStmt {
            expr: switch_expr,
            cases,
            default,
        })
    }

    fn parse_if_stmt(&mut self) -> Result<AstNode, String> {
        // 1) parse the condition
        let condition = self.parse_expr()?;
    
        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(format!("Expected '{{' after if condition, found {:?}", self.current_token()));
        }
        self.advance(); // consume '{'
    
        // 3) parse statements until '}'
        let mut body_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement()?;
            body_stmts.push(stmt);
        }
    
        // 4) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err("Missing '}' at end of if body".to_string());
        }
    
        // 5) check for "else" or "elif"
        let mut else_body = None;
        if self.current_token() == Token::Else {
            self.advance(); // consume 'else'
    
            // 6) expect '{'
            if self.current_token() != Token::LBrace {
                return Err(format!("Expected '{{' after else, found {:?}", self.current_token()));
            }
            self.advance(); // consume '{'
    
            // 7) parse statements until '}'
            let mut else_stmts = Vec::new();
            loop {
                self.skip_indent();
                if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                    break;
                }
                let stmt = self.parse_statement()?;
                else_stmts.push(stmt);
            }
    
            // 8) expect '}'
            if self.current_token() == Token::RBrace {
                self.advance();
            } else {
                return Err("Missing '}' at end of else body".to_string());
            }
    
            else_body = Some(else_stmts);
        }
    
        Ok(AstNode::IfStmt {
            condition: Box::new(condition),
            body: body_stmts,
            else_body,
        })
    }

    fn parse_for_stmt(&mut self) -> Result<AstNode, String> {
        // 1) expect identifier
        let var_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => {
                return Err(format!("Expected identifier after 'for', found {:?}", other));
            }
        };
    
        // 2) expect "in"
        if self.current_token() != Token::In {
            return Err(format!("Expected 'in' after for loop variable, found {:?}", self.current_token()));
        }
        self.advance(); // consume 'in'
    
        // 3) parse the iterable expression
        let iterable = self.parse_expr()?;
    
        // 4) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(format!("Expected '{{' after for loop, found {:?}", self.current_token()));
        }
        self.advance(); // consume '{'
    
        // 5) parse statements until '}'
        let mut body_stmts = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement()?;
            body_stmts.push(stmt);
        }
    
        // 6) expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err("Missing '}' at end of for loop body".to_string());
        }
    
        Ok(AstNode::ForStmt {
            var_name,
            iterable,
            body: body_stmts,
        })
    }

    /// import構文をパース (import xxx;)
    fn parse_import(&mut self) -> Result<AstNode, String> {
        let module_name = match self.current_token() {
            Token::Identifier(ref name) => {
                let nm = name.clone();
                self.advance(); // consume the identifier
                nm
            }
            ref other => {
                return Err(format!("Expected module name after import, got {:?}", other));
            }
        };

        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }

        Ok(AstNode::Import {
            module_name
        })
    }

    fn parse_from_import(&mut self) -> Result<AstNode, String> {
        // 例: "from <module> import <item>;"
        // 1) "from" はすでに消費済み
        // 2) expect module name => Identifier
        let module_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            other => {
                return Err(format!("Expected module name after 'from', got {:?}", other));
            }
        };
    
        // 3) expect "import"
        if self.current_token() != Token::Import {
            return Err(format!("Expected 'import', found {:?}", self.current_token()));
        }
        self.advance(); // consume 'import'
    
        // 4) expect item name => Identifier
        let item_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            other => {
                return Err(format!("Expected identifier after 'import', got {:?}", other));
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
                    return Err(format!("Expected alias name after 'as', got {:?}", other));
                }
            }
        }
    
        // optional semicolon
        if self.current_token() == Token::Semicolon {
            self.advance();
        }
    
        // ここでは統一的に AstNode::Import で扱ってもいいし、別のASTバリアントにしてもよい
        // 例: AstNode::FromImport { module_name, item_name, alias }
        // ただのサンプルとして、同じ Import に詰めるなら:
        let full = if let Some(a) = alias {
            format!("{}.{} as {}", module_name, item_name, a)
        } else {
            format!("{}.{}", module_name, item_name)
        };
    
        Ok(AstNode::Import {
            module_name: full
        })
    }

    fn parse_enum(&mut self) -> Result<AstNode, String> {
        let enum_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => return Err(format!("Expected enum name, found {:?}", other)),
        };
        if self.current_token() != Token::LBrace {
            return Err(format!("Expected '{{' after enum name, found {:?}", self.current_token()));
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
                ref other => return Err(format!("Expected enum variant name, found {:?}", other)),
            };
            variants.push(variant_name);
            if self.current_token() == Token::Comma {
                self.advance();
            }
        }

        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err("Missing '}' at end of enum definition".to_string());
        }

        Ok(AstNode::EnumDef {
            name: enum_name,
            variants,
        })
    }

    fn parse_struct(&mut self) -> Result<AstNode, String> {
        // たとえば "struct Point { x: int64; y: int64; }"
        // 1) expect Identifier (構造体名)
        let struct_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance(); // consume the identifier
                name
            }
            ref other => return Err(format!("Expected struct name, found {:?}", other)),
        };
    
        // 2) expect '{'
        if self.current_token() != Token::LBrace {
            return Err(format!("Expected '{{' after struct name, found {:?}", self.current_token()));
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
            return Err("Missing '}' at end of struct definition".to_string());
        }
    
        // ここでASTノードを返す
        // 構造体を表すAstNodeを追加するなり、VarDeclのようなものを使うなり好きに
        Ok(AstNode::StructDef {
            name: struct_name,
            fields,
        })
    }
    
    fn parse_struct_field(&mut self) -> Result<StructField, String> {
        self.skip_indent();
        // expect Identifier (フィールド名)
        let field_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let f = s.clone();
                self.advance();
                f
            }
            ref other => return Err(format!("Expected field name, found {:?}", other)),
        };
    
        // expect ':'
        if self.current_token() != Token::Colon {
            return Err(format!("Expected ':' after field name, found {:?}", self.current_token()));
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
    
    fn parse_decorators(&mut self) -> Result<Vec<Decorator>, String> {
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
    fn parse_class(&mut self) -> Result<AstNode, String> {
        // expect identifier
        let class_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let n = s.clone();
                self.advance();
                n
            }
            ref other => {
                return Err(format!("Expected class name, found {:?}", other));
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
                        return Err(format!("Expected parent class name, found {:?}", other));
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
            return Err(format!("Expected '{{' after class name, found {:?}", self.current_token()));
        }
        self.advance(); // consume '{'

        // parse members until '}'
        let mut members = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let member = self.parse_class_member()?;
            members.push(member);
        }

        // expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err("Missing '}' in class definition".to_string());
        }

        Ok(AstNode::ClassDef {
            name: class_name,
            members,
            parents,
        })
    }

    /// クラス内のメンバ: var宣言, 関数def など (簡易)
    fn parse_class_member(&mut self) -> Result<AstNode, String> {
        self.skip_indent();
        let decorators = self.parse_decorators()?;
        match self.current_token() {
            // final/let など => 変数宣言
            Token::Final | Token::Let | Token::Const => {
                let decl_token = self.current_token().clone();
                self.advance(); // consume final/let
                self.parse_var_decl(decl_token, false)
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
                        self.parse_var_decl(decl_token, true)
                    }
                    // static def => メソッド
                    Token::Def => {
                        self.advance();
                        // ここも「static method」として扱いたいなら
                        // parse_function() 呼ぶ前に何らかのフラグを付けるなどする
                        self.parse_function(true, false, decorators)
                    }
                    ref other => {
                        Err(format!("Expected 'let', 'final', or 'def' after 'static', found {:?}", other))
                    }
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
                        // 変数宣言としてパース
                        // ここでAstNodeに「privateである」情報をどう保持するか検討が必要
                        self.parse_var_decl(decl_token, false)
                    }
                    // private def => メソッド
                    Token::Def => {
                        self.advance();
                        // ここも「private method」として扱いたいなら
                        // parse_function() 呼ぶ前に何らかのフラグを付けるなどする
                        self.parse_function(false, true, decorators)
                    }
                    ref other => {
                        Err(format!("Expected 'let', 'final', or 'def' after 'private', found {:?}", other))
                    }
                }
            }
            // def => メソッド定義
            Token::Def => {
                self.advance();
                self.parse_function(false, true, decorators)
            }
            Token::Pass => {
                self.advance();
                self.expect(&Token::Semicolon)?;
                Ok(AstNode::PassStmt)
            }
            ref other => Err(format!("Unexpected token in class body: {:?}", other)),
        }
    }

    /// 関数定義: def funcName(...) -> type { ... }
    fn parse_function(&mut self, is_static: bool, has_parent: bool, decorators: Vec<Decorator>) -> Result<AstNode, String> {
        // 関数名
        let func_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let name = s.clone();
                self.advance();
                name
            }
            ref other => return Err(format!("Expected function name, found {:?}", other)),
        };

        // expect '('
        if self.current_token() != Token::LParen {
            return Err("Expected '(' after function name".to_string());
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
                ref other => return Err(format!("Expected parameter name, got {:?}", other)),
            };

            if is_first && has_parent {
                if self.current_token() == Token::Colon {
                    return Err("Expected ':' in parameter".to_string());
                }
            } else if self.current_token() == Token::Colon {
                self.advance();
            } else {
                return Err("Expected ':' in parameter".to_string());
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
            return Err("Missing ')' after function params".to_string());
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
            return Err("Expected '{' in function definition".to_string());
        }
        self.advance(); // consume '{'

        // parse function body statements
        let mut body = Vec::new();
        loop {
            self.skip_indent();
            if self.current_token() == Token::RBrace || self.current_token() == Token::Eof {
                break;
            }
            let stmt = self.parse_statement()?;
            body.push(stmt);
        }

        // expect '}'
        if self.current_token() == Token::RBrace {
            self.advance();
        } else {
            return Err("Missing '}' at end of function".to_string());
        }

        Ok(AstNode::FunctionDef {
            name: func_name,
            params,
            return_type,
            body,
            is_static,
            decorators,
        })
    }

    /// 変数宣言: (final|let) name: type (= expr)? ;
    fn parse_var_decl(&mut self, decl_token: Token, is_static: bool) -> Result<AstNode, String> {
        let decl_type = match decl_token {
            Token::Final => VarDeclType::Final,
            Token::Let => VarDeclType::Let,
            Token::Const => VarDeclType::Const,
            _ => return Err("Expected 'final', 'let', or 'const'".to_string()),
        };
        // var name
        let var_name = match self.current_token() {
            Token::Identifier(ref s) => {
                let n = s.clone();
                self.advance();
                n
            }
            ref other => return Err(format!("Expected variable name, found {:?}", other)),
        };

        let mut var_type = Some(String::new());
        // expect ':'
        if self.current_token() == Token::Colon {
            self.advance();
        } else if self.current_token() == Token::Assign {
            // if it's '=', set var_type to None
            var_type = None;
        } else {
            return Err("Expected ':' in variable declaration".to_string());
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

        Ok(AstNode::VarDecl {
            name: var_name,
            var_type,
            decl_type,
            expr: init_expr,
            is_static,
        })
    }

    /// ステートメント: return文 or 式文
    fn parse_statement(&mut self) -> Result<AstNode, String> {
        self.skip_indent();
        match self.current_token() {
            Token::Return => {
                self.advance(); // consume 'return'
                // return文の場合、式があるかもしれない
                if self.current_token() == Token::Semicolon {
                    // `return;`
                    self.advance();
                    Ok(AstNode::ReturnStmt(None))
                } else {
                    let e = self.parse_expr()?;
                    // optional semicolon
                    if self.current_token() == Token::Semicolon {
                        self.advance();
                    }
                    Ok(AstNode::ReturnStmt(Some(e)))
                }
            }
            Token::Final | Token::Let | Token::Const => {
                // 変数宣言とみなす
                let decl_token = self.current_token().clone();
                self.advance();
                self.parse_var_decl(decl_token, false)
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

    /// 簡易版の式パース: (識別子 / 数値 / 文字列 / 関数呼び出し)
    /// 演算子優先順位などは未対応
    fn parse_expr(&mut self) -> Result<Expr, String> {
        let left = self.parse_primary_expr()?;
        match self.current_token() {
            Token::Assign => {
                self.advance();
                let right = self.parse_expr()?;
                Ok(Expr::Assign {
                    target: Box::new(left),
                    value: Box::new(right),
                })
            }
            Token::PlusEqual | Token::MinusEqual | Token::StarEqual | Token::SlashEqual | Token::PercentEqual => {
                let op = match self.current_token() {
                    Token::PlusEqual => BinOp::Plus,
                    Token::MinusEqual => BinOp::Minus,
                    Token::StarEqual => BinOp::Star,
                    Token::SlashEqual => BinOp::Slash,
                    Token::PercentEqual => BinOp::Percent,
                    _ => unreachable!(),
                };
                self.advance();
                let right = self.parse_expr()?;
                Ok(Expr::CompoundAssign {
                    op,
                    target: Box::new(left),
                    value: Box::new(right),
                })
            }
            Token::EqualEqual | Token::NotEqual | Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => {
                let op = match self.current_token() {
                    Token::EqualEqual => BinOp::Equal,
                    Token::NotEqual => BinOp::NotEqual,
                    Token::Less => BinOp::LessThan,
                    Token::LessEqual => BinOp::LessThanOrEqual,
                    Token::Greater => BinOp::GreaterThan,
                    Token::GreaterEqual => BinOp::GreaterThanOrEqual,
                    _ => unreachable!(),
                };
                self.advance();
                let right = self.parse_expr()?;
                Ok(Expr::Binary {
                    op,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            }
            Token::RangeHalfOpen | Token::RangeClosed => {
                let op = match self.current_token() {
                    Token::RangeHalfOpen => BinOp::RangeHalfOpen,
                    Token::RangeClosed => BinOp::RangeClosed,
                    _ => unreachable!(),
                };
                self.advance();
                let right = self.parse_expr()?;
                Ok(Expr::Range {
                    start: Box::new(left),
                    end: Box::new(right),
                    op,
                })
            }
            Token::StringLiteral(ref s) => {
                match left {
                    Expr::Identifier(name) => {
                        if name == "f" {
                            let st = s.clone();
                            self.advance();
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Format,
                            });
                        } else if name == "b" {
                            let st = s.clone();
                            self.advance();
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Raw,
                            });
                        } else if name == "r" {
                            let st = s.clone();
                            self.advance();
                            return Ok(Expr::StringLiteral {
                                value: st,
                                ty: StringLiteralType::Regex,
                            });
                        }
                        Ok(Expr::Identifier(name.clone()))
                    }
                    _ => {
                        Ok(left)
                    }
                }
            }
            _ => {
                Ok(left)
            }
        }
    }

    fn parse_primary_expr(&mut self) -> Result<Expr, String> {
        let tok = self.current_token();
        let mut expr = match tok {
            // Identifier => 関数呼び出しの可能性もチェック
            Token::Identifier(ref name) => {
                let ident_name = name.clone();
                self.advance(); // consume identifier

                // もし次が '(' なら関数呼び出し
                if self.current_token() == Token::LParen {
                    self.parse_call_expr(Expr::Identifier(ident_name))?
                } else {
                    // 単なる識別子
                    Expr::Identifier(ident_name)
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
                }
            }
            Token::LBracket => {
                self.parse_array_literal()?
            }
            ref other => {
                return Err(format!("Unexpected token in expression: {:?}", other));
            }
        };
        loop {
            if self.current_token() == Token::Dot {
                self.advance();
                match self.current_token() {
                    Token::Identifier(ref field_name) => {
                        let field = field_name.clone();
                        self.advance();
                        let member_expr = Expr::MemberAccess {
                            target: Box::new(expr),
                            member: field,
                        };
                        expr = member_expr;
                    }
                    ref other => {
                        return Err(format!("Expected identifier after '.', found {:?}", other));
                    }
                }
            } else if self.current_token() == Token::LParen {
                expr = self.parse_call_expr(expr)?;
            } else if self.current_token() == Token::LBracket {
                expr = self.parse_index_expr(expr)?;
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn parse_call_expr(&mut self, callee_expr: Expr) -> Result<Expr, String> {
        // assume current_token() == Token::LParen => consume it
        if self.current_token() == Token::LParen {
            self.advance(); // consume '('
        } else {
            return Err("Expected '(' in call expression".to_string());
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
            return Err("Missing ')' in function call".to_string());
        }
    
        // return a Call node
        Ok(Expr::Call {
            callee: Box::new(callee_expr),
            args,
        })
    }

    fn parse_index_expr(&mut self, base_expr: Expr) -> Result<Expr, String> {
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

    fn parse_array_literal(&mut self) -> Result<Expr, String> {
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
            self.tokens[self.pos].clone()
        }
    }

    /// n個先のトークンを覗き見
    fn peek_token(&self, n: usize) -> Token {
        let idx = self.pos + n;
        if idx >= self.tokens.len() {
            Token::Eof
        } else {
            self.tokens[idx].clone()
        }
    }

    /// 今のトークンを返して pos を1進める
    fn advance(&mut self) -> Token {
        let tk = self.current_token();
        self.pos += 1;
        tk
    }
}
