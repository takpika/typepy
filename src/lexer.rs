use std::collections::HashMap;
use crate::token::Token;
use crate::keyword_map::build_keyword_map;

pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    line: usize,
    column: usize,
    keyword_map: HashMap<&'static str, Token>,
}

impl<'a> Lexer<'a> {
    /// 新しいLexerを作成
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            line: 1,
            column: 0,
            keyword_map: build_keyword_map(),
        }
    }

    /// 入力が終わっているか
    fn is_at_end(&self) -> bool {
        self.pos >= self.input.len()
    }

    /// 現在の文字を取得（範囲外なら '\0'）
    fn current_char(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.input[self.pos..].chars().next().unwrap()
        }
    }

    /// 次の文字に進む
    fn advance(&mut self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        let c = self.current_char();
        self.pos += c.len_utf8();
        self.column += 1;
        c
    }

    /// 次の文字が `expected` と一致すれば消費してtrueを返す
    fn match_char(&mut self, expected: char) -> bool {
        if self.current_char() == expected {
            self.advance();
            true
        } else {
            false
        }
    }

    /// 行末までコメントを読み飛ばす (Pythonスタイル # ...)
    fn skip_comment(&mut self) {
        while !self.is_at_end() {
            let c = self.current_char();
            if c == '\n' {
                break; // 改行までで終わり
            }
            self.advance(); // 文字を消費
        }
    }

    /// 空白やタブ、コメント(#...)をスキップ
    fn skip_whitespace_and_comments(&mut self) {
        loop {
            if self.is_at_end() {
                return;
            }
            let c = self.current_char();
            if c.is_whitespace() && c != '\n' {
                // スペース・タブは進める（ただし改行は別処理で扱いたいので除外）
                self.advance();
            } else if c == '#' {
                // 行末までコメント
                self.skip_comment();
            } else {
                // それ以外ならループを抜ける
                break;
            }
        }
    }

    /// インデント（行頭スペース）を読み取る
    /// ※ シンプルに「行頭の半角スペース数」を数えるだけ。タブや複数行の厳密管理は省略。
    fn read_indent(&mut self) -> Token {
        let mut count = 0;
        while !self.is_at_end() {
            let c = self.current_char();
            if c == ' ' {
                count += 1;
                self.advance();
            } else {
                break;
            }
        }
        Token::Indent(count)
    }

    /// 識別子 or キーワードを読み取る
    fn read_identifier(&mut self) -> Token {
        let start_pos = self.pos;
        while !self.is_at_end() {
            let c = self.current_char();
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }
        let literal = &self.input[start_pos..self.pos];
        if let Some(tok) = self.keyword_map.get(literal) {
            tok.clone()
        } else {
            Token::Identifier(literal.to_string())
        }
    }

    /// 数字リテラル（整数 or 小数）を読み取る
    fn read_number(&mut self) -> Token {
        let start_pos = self.pos;
        let mut seen_dot = false;
    
        // consume all digits first
        while !self.is_at_end() {
            let c = self.current_char();
            if c.is_numeric() {
                self.advance(); // consume digit
            } else if c == '.' && !seen_dot {
                // ここで次の文字が数字かどうかチェック
                if let Some(next_c) = self.peek_char_as_option(1) {
                    if next_c.is_numeric() {
                        // float の一部として扱う (例: 3.14)
                        seen_dot = true;
                        self.advance(); // consume '.'
                    } else {
                        // '.' の後に数字が来ない → 演算子か何かなのでここで数値読み取り終了
                        break;
                    }
                } else {
                    // EOF or invalid
                    break;
                }
            } else {
                break;
            }
        }
    
        let literal = &self.input[start_pos..self.pos];
        // seen_dot が true なら float 解析
        // そうでなければ int 解析
        if seen_dot {
            if let Ok(val) = literal.parse::<f64>() {
                Token::FloatLiteral(val)
            } else {
                // パース失敗 → エラー対応など
                Token::FloatLiteral(0.0)
            }
        } else {
            if let Ok(val) = literal.parse::<i64>() {
                Token::IntLiteral(val)
            } else {
                // パース失敗 → エラー対応
                Token::IntLiteral(0)
            }
        }
    }
    

    fn read_multiline_string(&mut self, quote_char: char) -> Token {
        let mut result = String::new();
    
        loop {
            if self.is_at_end() {
                // EOFに達してしまった => 終端がなかった
                break;
            }
            let c = self.current_char();
    
            // もし `"""` or `'''` の終端を見つけたら抜ける
            if c == quote_char
                && self.peek_char(1) == quote_char
                && self.peek_char(2) == quote_char
            {
                // consume 3つ
                self.advance();
                self.advance();
                self.advance();
                break;
            }
            result.push(c);
            self.advance();
        }
    
        Token::StringLiteral(result)
    }    
    
    /// 現在の位置 self.pos から offset 文字先をのぞき見し、
    /// その文字を Some(char) で返す。
    /// もしファイル終端を超えていれば None を返す。
    fn peek_char_as_option(&self, offset: usize) -> Option<char> {
        let mut idx = self.pos;
        let len = self.input.len();

        for _ in 0..offset {
            if idx >= len {
                return None;
            }
            // 今のidxから次のUTF-8文字を取得
            let ch = self.input[idx..].chars().next()?;
            idx += ch.len_utf8();
        }

        if idx >= len {
            return None;
        }

        self.input[idx..].chars().next()
    }

    /// ヘルパー：現時点から offset だけ先の文字を確認 (EOFなら '\0')
    fn peek_char(&self, offset: usize) -> char {
        let mut idx = self.pos;
        let len = self.input.len();
        let mut c: char = '\0';
        for _ in 0..offset+1 {
            if idx >= len {
                return '\0';
            }
            c = self.input[idx..].chars().next().unwrap();
            idx += c.len_utf8();
        }
        c
    }
    

    /// 文字列リテラルを読み取る (シングルクォート / ダブルクォート / 三重クォート対応)
    fn read_string(&mut self, quote_char: char) -> Token {
        // 現在 self.pos は先頭のクォート直後を指している

        // まず「次の2文字が連続して quote_char なら三重クォート」とみなす
        if self.peek_char(0) == quote_char && self.peek_char(1) == quote_char {
            // 三重クォートモード => consume 2つ
            self.advance(); // 2つ目
            self.advance(); // 3つ目
            return self.read_multiline_string(quote_char);
        } else {
            // 通常のシングルライン文字列
            let mut result = String::new();
        
            loop {
                if self.is_at_end() {
                    // EOF に達した場合は閉じクォートが無いので終了
                    break;
                }
                let c = self.current_char();
                if c == quote_char {
                    // 閉じクォートを検出したら消費して終了
                    self.advance();
                    break;
                }
                // エスケープシーケンス処理
                if c == '\\' {
                    self.advance(); // バックスラッシュを消費
                    if self.is_at_end() {
                        break;
                    }
                    let escaped = self.current_char();
                    self.advance();
                    // 簡易的なエスケープ処理
                    let esc_char = match escaped {
                        'n'  => '\n',
                        't'  => '\t',
                        'r'  => '\r',
                        '\\' => '\\',
                        '"'  => '"',
                        '\'' => '\'',
                        other => other,  // 未対応のものはそのまま
                    };
                    result.push(esc_char);
                } else {
                    result.push(c);
                    self.advance();
                }
            }
        
            return Token::StringLiteral(result);
        }
    }

    /// デコレータか単なる '@' かを判定してトークン生成
    fn read_decorator(&mut self) -> Token {
        // 先頭の '@' はすでに消費している想定
        // 例えば '@deprecated' のように識別子が続くか調べる
        let start_pos = self.pos;
        while !self.is_at_end() {
            let c = self.current_char();
            // スペースや記号などで終わりとする
            if c.is_whitespace() || "(){}[]:;,".contains(c) {
                break;
            }
            self.advance();
        }
        // '@' の後ろを全部読んだ
        let literal = &self.input[start_pos..self.pos];
        if !literal.is_empty() {
            Token::Decorator(literal.to_string())
        } else {
            Token::AtSymbol
        }
    }

    /// 1トークンを読み取る
    fn next_token(&mut self) -> Token {
        // まず空白 + コメントをスキップ
        self.skip_whitespace_and_comments();
    
        if self.is_at_end() {
            return Token::Eof;
        }
    
        let c = self.current_char();
    
        // 改行の場合
        if c == '\n' {
            self.advance();
            // 行数を増やす
            self.line += 1;
            self.column = 0;
            // 次の行頭スペースを読み取る
            let indent_token = self.read_indent();
            return indent_token;
        }

        // 予約された記号類を判定
        match c {
            '{' => { self.advance(); return Token::LBrace; }
            '}' => { self.advance(); return Token::RBrace; }
            '(' => { self.advance(); return Token::LParen; }
            ')' => { self.advance(); return Token::RParen; }
            '[' => { self.advance(); return Token::LBracket; }
            ']' => { self.advance(); return Token::RBracket; }
            ',' => { self.advance(); return Token::Comma; }
            ';' => { self.advance(); return Token::Semicolon; }
            ':' => { self.advance(); return Token::Colon; }
            '|' => {
                self.advance();
                if self.match_char('|') {
                    return Token::Or;
                } else if self.match_char('=') {
                    return Token::PipeEqual;
                }
                return Token::Pipe;
            }
            '&' => {
                self.advance();
                if self.match_char('&') {
                    return Token::And;
                } else if self.match_char('=') {
                    return Token::AmpEqual;
                }
                return Token::Amp;
            }
            '.' => {
                if self.peek_char(1) == '.' && self.peek_char(2) == '<' {
                    // "..<" (half-open range)
                    self.advance();
                    self.advance();
                    self.advance();
                    return Token::RangeHalfOpen;
                } else if self.peek_char(1) == '.' {
                    // ".." (closed range)
                    self.advance();
                    self.advance();
                    return Token::RangeClosed;
                } else {
                    self.advance();
                    return Token::Dot;
                }
            }
            '?' => {
                self.advance();
                if self.match_char('?') {
                    return Token::QuestionQuestion;
                }
                return Token::Question;
            }
            '=' => {
                self.advance();
                if self.match_char('=') {
                    return Token::EqualEqual;
                } else if self.match_char('>') {
                    return Token::FatArrow; // =>
                }
                return Token::Assign;
            }
            '-' => {
                self.advance();
                if self.match_char('>') {
                    return Token::Arrow; // ->
                } else if self.match_char('=') {
                    return Token::MinusEqual;
                }
                return Token::Minus;
            }
            '+' => {
                self.advance();
                if self.match_char('=') {
                    return Token::PlusEqual;
                }
                return Token::Plus;
            }
            '*' => {
                self.advance();
                if self.match_char('=') {
                    return Token::StarEqual;
                } else if self.match_char('*') {
                    return Token::StarStar;
                }
                return Token::Star;
            }
            '/' => {
                self.advance();
                if self.match_char('=') {
                    return Token::SlashEqual;
                }
                return Token::Slash;
            }
            '%' => {
                self.advance();
                if self.match_char('=') {
                    return Token::PercentEqual;
                }
                return Token::Percent;
            }
            '^' => {
                self.advance();
                if self.match_char('=') {
                    return Token::CaretEqual;
                }
                return Token::Caret;
            }
            '!' => {
                self.advance();
                if self.match_char('=') {
                    return Token::NotEqual;
                }
                return Token::Bang;
            }
            '<' => {
                self.advance();
                if self.match_char('=') {
                    return Token::LessEqual;
                } else if self.match_char('<') {
                    return Token::ShiftLeft;
                }
                return Token::Less;
            }
            '>' => {
                self.advance();
                if self.match_char('=') {
                    return Token::GreaterEqual;
                } else if self.match_char('>') {
                    return Token::ShiftRight;
                }
                return Token::Greater;
            }
            '@' => {
                // デコレータ or @シンボル
                self.advance();
                return self.read_decorator();
            }
            '"' => {
                // まずクォートの文字を取得
                let quote_char = self.current_char(); // これは `"` のはず
                self.advance(); // クォートを1回だけ消費
                return self.read_string(quote_char);
            }
            // シングルクォート
            '\'' => {
                let quote_char = self.current_char(); // `\'`
                self.advance();
                return self.read_string(quote_char);
            }
            _ => {
                // 英字 => 識別子 or キーワード
                if c.is_alphabetic() || c == '_' {
                    return self.read_identifier();
                }
                // 数字 => 数値リテラル
                if c.is_numeric() {
                    return self.read_number();
                }

                // それ以外の文字はとりあえず無視 or エラー扱い (本来はエラーにするべき)
                self.advance();
                // エラー処理をどうするかは要検討
                return Token::Eof; 
            }
        }
    }

    /// 入力全体をトークンのベクタに変換する
    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            if tok == Token::Eof {
                tokens.push(Token::Eof);
                break;
            }
            tokens.push(tok);
        }
        tokens
    }
}
