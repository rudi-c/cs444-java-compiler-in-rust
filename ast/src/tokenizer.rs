use std::num::FromStrRadix;
use std::char;

#[derive(Show)]
pub enum Token {
    Identifier(String),

    // Literals. Note that FloatingPointLiteral not required in Joos
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    CharacterLiteral(char),
    StringLiteral(String),
    NullLiteral,

    // Keywords ($3.9)
    // Capitalized because the variants in this enum are constants
    // and some of the variants (e.g. 'Char') can be easily confused
    // with Rust's 'Char' type (the syntax highlighter is among the things
    // confused by it).
    ABSTRACT,
    BOOLEAN,
    // BREAK,
    BYTE,
    // CASE,
    // CATCH,
    CHAR,
    CLASS,
    CONST,
    // CONTINUE,
    DEFAULT,
    DO,
    // DOUBLE,
    ELSE,
    EXTENDS,
    FINAL,
    // FINALLY,
    // FLOAT,
    FOR,
    GOTO,
    IF,
    IMPLEMENTS,
    IMPORT,
    INSTANCEOF,
    INT,
    INTERFACE,
    // LONG,
    NATIVE,
    NEW,
    PACKAGE,
    PRIVATE,
    PROTECTED,
    PUBLIC,
    RETURN,
    SHORT,
    STATIC,
    // STRICTFP,
    SUPER,
    // SWITCH,
    // SYNCHRONIZED,
    THIS,
    // THROW,
    // THROWS,
    // TRANSIENT,
    // TRY,
    VOID,
    // VOLATILE,
    WHILE,

    // Separators ($3.11)
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Semicolon,
    Comma,
    Dot,

    // Operators ($3.12)
    Assignment,
    Xor,
    Or,
    And,
    OrOr,
    AndAnd,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,

    Whitespace,
    Comment,
    Error(String),
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    // TODO:
    // Windows line terminators?

    // Whitespace defined in $3.6
    r#"[ \t\n][ \t\n]*"# => (Token::Whitespace, text),

    // Comments defined in $3.7
    r#"/[*](~(.*[*]/.*))[*]/"# => (Token::Comment, text),
    r#"//[^\n]*"# => (Token::Comment, text),

    // Keywords defined in $3.9.
    // Has precedence over identifiers.
    r#"abstract"# => (Token::ABSTRACT, text),
    r#"boolean"# => (Token::BOOLEAN, text),
    r#"byte"# => (Token::BYTE, text),
    r#"char"# => (Token::CHAR, text),
    r#"class"# => (Token::CLASS, text),
    r#"const"# => (Token::CONST, text),
    r#"default"# => (Token::DEFAULT, text),
    r#"do"# => (Token::DO, text),
    r#"else"# => (Token::ELSE, text),
    r#"extends"# => (Token::EXTENDS, text),
    r#"final"# => (Token::FINAL, text),
    r#"for"# => (Token::FOR, text),
    r#"goto"# => (Token::GOTO, text),
    r#"if"# => (Token::IF, text),
    r#"implements"# => (Token::IMPLEMENTS, text),
    r#"import"# => (Token::IMPORT, text),
    r#"instanceof"# => (Token::INSTANCEOF, text),
    r#"int"# => (Token::INT, text),
    r#"interface"# => (Token::INTERFACE, text),
    r#"native"# => (Token::NATIVE, text),
    r#"new"# => (Token::NEW, text),
    r#"package"# => (Token::PACKAGE, text),
    r#"private"# => (Token::PRIVATE, text),
    r#"protected"# => (Token::PROTECTED, text),
    r#"public"# => (Token::PUBLIC, text),
    r#"return"# => (Token::RETURN, text),
    r#"short"# => (Token::SHORT, text),
    r#"static"# => (Token::STATIC, text),
    r#"super"# => (Token::SUPER, text),
    r#"this"# => (Token::THIS, text),
    r#"void"# => (Token::VOID, text),
    r#"while"# => (Token::WHILE, text),

    // Unsupported keywords.
    // We still parse these because we don't want to mistake them as identifiers.
    r#"break"# => (Token::Error(String::from_str("keyword break not supported")), text),
    r#"case"# => (Token::Error(String::from_str("keyword case not supported")), text),
    r#"catch"# => (Token::Error(String::from_str("keyword catch not supported")), text),
    r#"continue"# => (Token::Error(String::from_str("keyword continue not supported")), text),
    r#"double"# => (Token::Error(String::from_str("keyword double not supported")), text),
    r#"finally"# => (Token::Error(String::from_str("keyword finally not supported")), text),
    r#"float"# => (Token::Error(String::from_str("keyword float not supported")), text),
    r#"long"# => (Token::Error(String::from_str("keyword long not supported")), text),
    r#"strictfp"# => (Token::Error(String::from_str("keyword strictfp not supported")), text),
    r#"switch"# => (Token::Error(String::from_str("keyword switch not supported")), text),
    r#"synchronized"# => (Token::Error(String::from_str("keyword synchronized not supported")), text),
    r#"throw"# => (Token::Error(String::from_str("keyword throw not supported")), text),
    r#"throws"# => (Token::Error(String::from_str("keyword throws not supported")), text),
    r#"transient"# => (Token::Error(String::from_str("keyword transient not supported")), text),
    r#"try"# => (Token::Error(String::from_str("keyword try not supported")), text),
    r#"volatile"# => (Token::Error(String::from_str("keyword volatile not supported")), text),


    // Literals defined in $3.10
    // Note that Octal, Hex and Long literals are not required in Joos.
    // Negative literals do not exist: the minus sign is a unary operator.
    r#"0|[1-9][0-9]*"# => {
        if let Some(i) = text.parse() {
            (Token::IntegerLiteral(i), text)
        } else {
            (Token::Error(String::from_str("integer is way too large, did the cat step on the numpad?")), text)
        }
    },
    // String literals
    // Note that Unicode escapes are not required.
    r#""([^"\\]|\\.)*""# => (Token::StringLiteral(unescape(&text[1..text.len()-1])), text),
    // Check for unterminated string constants.
    r#""([^"\\]|\\.)*"# => (Token::Error(String::from_str("unterminated string constant")), text),
    r#"'[^'\\]'"# => (Token::CharacterLiteral(text.char_at(1)), text),
    r#"'\\[0-7]'"# => (Token::CharacterLiteral(unescape(&text[1..text.len()-1]).char_at(0)), text),
    r#"'\\[0-7][0-7]'"# => (Token::CharacterLiteral(unescape(&text[1..text.len()-1]).char_at(0)), text),
    r#"'\\[0-3][0-7][0-7]'"# => (Token::CharacterLiteral(unescape(&text[1..text.len()-1]).char_at(0)), text),
    r#"'\\.'"# => (Token::CharacterLiteral(unescape(&text[1..text.len()-1]).char_at(0)), text),
    r#"'.'"# => panic!("invalid character literal: {}", text),
    r#"'"# => panic!("invalid character literal"),
    r#"true"# => (Token::BooleanLiteral(true), text),
    r#"false"# => (Token::BooleanLiteral(false), text),
    r#"null"# => (Token::NullLiteral, text),


    // Identifiers defined in $3.8
    // Note that $ is considered a Java letter.
    r#"[a-zA-Z_$][a-zA-Z0-9_$]*"# => (Token::Identifier(text.to_string()), text),

    // Separators defined in $3.11
    r#"\("# => (Token::LParen, text),
    r#"\)"# => (Token::RParen, text),
    r#"\["# => (Token::LBracket, text),
    r#"\]"# => (Token::RBracket, text),
    r#"{"# => (Token::LBrace, text),
    r#"}"# => (Token::RBrace, text),
    r#";"# => (Token::Semicolon, text),
    r#","# => (Token::Comma, text),
    r#"\."# => (Token::Dot, text),

    // Operators defined in $3.12
    // Note the none of the following are required :
    //     bitwise ops, increment/decrement, assignment ops,
    //     unary plus, choice (?:), bit shift
    r#"="# => (Token::Assignment, text),
    r#"\^"# => (Token::Xor, text),
    r#"\|"# => (Token::Or, text),
    r#"\&"# => (Token::And, text),
    r#"\|\|"# => (Token::OrOr, text),
    r#"\&\&"# => (Token::AndAnd, text),
    r#"=="# => (Token::Equals, text),
    r#"!="# => (Token::NotEquals, text),
    r#"<"# => (Token::LessThan, text),
    r#">"# => (Token::GreaterThan, text),
    r#"<="# => (Token::LessEqual, text),
    r#">="# => (Token::GreaterEqual, text),
    r#"+"# => (Token::Plus, text),
    r#"-"# => (Token::Minus, text),
    r#"\*"# => (Token::Star, text),
    r#"/"# => (Token::Slash, text),
    r#"%"# => (Token::Percent, text),
    r#"!"# => (Token::Bang, text),

    r#"."# => (Token::Error("unrecognized input".to_string()), text),
}

fn octal(s: &str) -> char {
    char::from_u32(FromStrRadix::from_str_radix(s, 8).unwrap()).unwrap()
}

scanner! {
    unescape_c(text) -> char;

    r"\\[0-3][0-7][0-7]" => octal(&text[1..]),
    r"\\[0-7][0-7]" => octal(&text[1..]),
    r"\\[0-7]" => octal(&text[1..]),
    r"\\b" => '\u{0008}',
    r"\\t" => '\t',
    r"\\n" => '\n',
    r"\\f" => '\u{000c}',
    r"\\r" => '\r',
    r#"\\""# => '"',
    r"\\'" => '\'',
    r"\\\\" => '\\',
    r"\\." => panic!("bad escape sequence: {}", text),
    r"\\" => panic!("unterminated escape sequence"),
    r"[^\\]" => text.char_at(0),
}

fn unescape(mut s: &str) -> String {
    let mut r = String::with_capacity(s.len());
    while let Some(c) = unescape_c(&mut s) {
        r.push(c);
    }
    assert!(s.is_empty());
    r
}

pub struct Tokenizer<'a> {
    slice: &'a str
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            if self.slice.is_empty() {
                return None;
            }
            match next_token(&mut self.slice) {
                Some((token, text)) => {
                    if let &Token::Error(ref error) = &token {
                        println!("invalid token `{}`: {}", text, error);
                        return None;
                    }
                    if token_filter(&token) {
                        // XXX: the "{:?}" formatter doesn't seem to accept
                        // alignment
                        println!("{:<25} {:?}", format!("{:?}", token), text);
                        return Some(token);
                    }
                }
                None => {
                    println!("invalid token");
                    return None;
                }
            }
        }
    }
}

fn token_filter(token: &Token) -> bool {
    match *token {
        Token::Whitespace => false,
        Token::Comment => false,
        _ => true
    }
}

pub fn tokenizer<'a>(s: &'a str) -> Tokenizer<'a> {
    Tokenizer { slice: s }
}
