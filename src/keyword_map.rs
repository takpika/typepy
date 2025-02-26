use std::collections::HashMap;
use crate::token::Token;

pub fn build_keyword_map() -> HashMap<&'static str, Token> {
    let mut m = HashMap::new();
    m.insert("if", Token::If);
    m.insert("else", Token::Else);
    m.insert("for", Token::For);
    m.insert("in", Token::In);
    m.insert("while", Token::While);
    m.insert("switch", Token::Switch);
    m.insert("case", Token::Case);
    m.insert("default", Token::Default);
    m.insert("break", Token::Break);
    m.insert("continue", Token::Continue);

    m.insert("class", Token::Class);
    m.insert("struct", Token::Struct);
    m.insert("enum", Token::Enum);
    m.insert("def", Token::Def);
    m.insert("let", Token::Let);
    m.insert("final", Token::Final);
    m.insert("static", Token::Static);
    m.insert("private", Token::Private);
    m.insert("public", Token::Public);
    m.insert("const", Token::Const);
    m.insert("import", Token::Import);
    m.insert("from", Token::From);
    m.insert("as", Token::As);
    m.insert("try", Token::Try);
    m.insert("catch", Token::Catch);
    m.insert("throw", Token::Throw);
    m.insert("finally", Token::Finally);
    m.insert("return", Token::Return);
    m.insert("guard", Token::Guard);
    m.insert("pass", Token::Pass);

    m.insert("None", Token::None);
    m.insert("true", Token::True);
    m.insert("false", Token::False);

    m
}
