/// 字句解析で得られるトークンを表す列挙体
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // 制御構文キーワード
    If,
    Else,
    For,
    In,
    While,    // サンプルには出てきませんが一般的にあると想定
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
    AtSymbol,            // '@'
    Decorator(String),   // 例: "@deprecated" をまるごと一つのトークンにするか分けるかは好み

    // 識別子
    Identifier(String),

    // リテラル
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),

    // 記号・演算子
    LParen,    // (
    RParen,    // )
    LBrace,    // {
    RBrace,    // }
    LBracket,  // [
    RBracket,  // ]
    Comma,     // ,
    Semicolon, // ;
    Colon,     // :
    Dot,       // .
    Plus,      // +
    Minus,     // -
    Star,      // *
    Slash,     // /
    Percent,   // %
    Caret,     // ^
    Amp,       // &
    Pipe,      // |
    Bang,      // !
    Question,  // ?
    Assign,    // =
    Arrow,     // ->  (例: login(...) -> bool)
    FatArrow,  // =>  (例: (x) => x * x)

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
    And,        // &&
    Or,          // ||
    ShiftLeft,     // <<
    ShiftRight,    // >>
    RangeHalfOpen, // ..<  (半開区間)
    RangeClosed,  // ..   (閉区間)
    // ... 必要なら適宜追加

    // インデント・改行管理（簡易的）
    Indent(usize), // 行頭のスペース数などを保持
    Newline,

    // 終了
    Eof,
}
