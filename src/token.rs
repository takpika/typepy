/// 入力中の位置を表す構造体。1文字ずつ進んだ際のバイトオフセットと行・列情報を保持する。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// UTF-8バイト列における開始位置
    pub offset: usize,
    /// 1始まりの行番号
    pub line: usize,
    /// 0始まりの桁位置
    pub column: usize,
}

impl Position {
    pub fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            offset: 0,
            line: 1,
            column: 0,
        }
    }
}

/// トークンがどの範囲に対応するかを保持する。
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn snippet(&self, source: &str) -> Option<String> {
        if self.start.line == 0 {
            return None;
        }

        let line_index = self.start.line.checked_sub(1)?;
        let line = source.lines().nth(line_index)?.trim_end_matches('\r');

        let line_len = line.chars().count();
        let mut start_col = self.start.column;
        if start_col > line_len {
            start_col = line_len;
        }

        let caret_len = if self.start.line == self.end.line {
            let mut end_col = self.end.column.max(self.start.column + 1);
            if end_col > line_len {
                end_col = line_len;
            }
            end_col.saturating_sub(start_col).max(1)
        } else {
            line_len.saturating_sub(start_col).max(1)
        };

        let mut caret_line = String::with_capacity(start_col + caret_len);
        caret_line.extend(std::iter::repeat(' ').take(start_col));
        caret_line.extend(std::iter::repeat('^').take(caret_len));

        Some(format!(
            "{:>4} | {}\n     | {}",
            self.start.line, line, caret_line
        ))
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: Position::default(),
            end: Position::default(),
        }
    }
}

/// 字句解析で得られるトークン種別を表す列挙体
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 制御構文キーワード
    If,
    Else,
    For,
    In,
    While, // サンプルには出てきませんが一般的にあると想定
    Switch,
    Case,
    Default,
    Break,
    Continue,

    // 宣言系キーワード
    Class,
    Struct,
    Enum,
    Def,
    Let,
    Final,
    Static,
    Private,
    Public,
    Const,
    Import,
    From,
    As,
    Try,
    Catch,
    Throw,
    Finally,
    Return,
    Guard,
    Pass,

    // その他のキーワード or 組み込み
    None,
    True,
    False,

    // デコレータ (@xxx)
    AtSymbol,          // '@'
    Decorator(String), // 例: "@deprecated" をまるごと一つのトークンにするか分けるかは好み

    // 識別子
    Identifier(String),

    // リテラル
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // 記号・演算子
    LParen,           // (
    RParen,           // )
    LBrace,           // {
    RBrace,           // }
    LBracket,         // [
    RBracket,         // ]
    Comma,            // ,
    Semicolon,        // ;
    Colon,            // :
    Dot,              // .
    Plus,             // +
    Minus,            // -
    Star,             // *
    StarStar,         // **
    Slash,            // /
    Percent,          // %
    Caret,            // ^
    Amp,              // &
    Pipe,             // |
    Bang,             // !
    Question,         // ?
    QuestionQuestion, // ??
    Assign,           // =
    Arrow,            // ->  (例: login(...) -> bool)
    FatArrow,         // =>  (例: (x) => x * x)

    // 二項演算子や比較演算子など、必要に応じて追加
    EqualEqual,    // ==
    NotEqual,      // !=
    Less,          // <
    LessEqual,     // <=
    Greater,       // >
    GreaterEqual,  // >=
    PlusEqual,     // +=
    MinusEqual,    // -=
    StarEqual,     // *=
    SlashEqual,    // /=
    PercentEqual,  // %=
    CaretEqual,    // ^=
    AmpEqual,      // &=
    PipeEqual,     // |=
    And,           // &&
    Or,            // ||
    ShiftLeft,     // <<
    ShiftRight,    // >>
    RangeHalfOpen, // ..<  (半開区間)
    RangeClosed,   // ..   (閉区間)
    // ... 必要なら適宜追加

    // インデント・改行管理（簡易的）
    Indent(usize), // 行頭のスペース数などを保持
    Newline,

    // 終了
    Eof,
}

/// トークン種別とその位置情報をまとめた構造体
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

impl SpannedToken {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}
