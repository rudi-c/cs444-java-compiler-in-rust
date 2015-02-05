use span::Span;

use std::num::FromStrRadix;
use std::char;

#[derive(Show, Clone)]
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
    SoftError(Box<Token>, String),
    Error(String),
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    // Whitespace defined in $3.6
    "[ \x0C\t\r\n][ \x0C\t\r\n]*" => (Token::Whitespace, text),

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
    r#"break"# => (Token::Error("keyword break not supported".to_string()), text),
    r#"case"# => (Token::Error("keyword case not supported".to_string()), text),
    r#"catch"# => (Token::Error("keyword catch not supported".to_string()), text),
    r#"continue"# => (Token::Error("keyword continue not supported".to_string()), text),
    r#"double"# => (Token::Error("keyword double not supported".to_string()), text),
    r#"finally"# => (Token::Error("keyword finally not supported".to_string()), text),
    r#"float"# => (Token::Error("keyword float not supported".to_string()), text),
    r#"long"# => (Token::Error("keyword long not supported".to_string()), text),
    r#"strictfp"# => (Token::Error("keyword strictfp not supported".to_string()), text),
    r#"switch"# => (Token::Error("keyword switch not supported".to_string()), text),
    r#"synchronized"# => (Token::Error("keyword synchronized not supported".to_string()), text),
    r#"throw"# => (Token::Error("keyword throw not supported".to_string()), text),
    r#"throws"# => (Token::Error("keyword throws not supported".to_string()), text),
    r#"transient"# => (Token::Error("keyword transient not supported".to_string()), text),
    r#"try"# => (Token::Error("keyword try not supported".to_string()), text),
    r#"volatile"# => (Token::Error("keyword volatile not supported".to_string()), text),


    // Literals defined in $3.10
    // Note that Octal, Hex and Long literals are not required in Joos.
    // Negative literals do not exist: the minus sign is a unary operator.
    r#"0|[1-9][0-9]*"# => {
        (if let Some(i) = text.parse() {
            Token::IntegerLiteral(i)
        } else {
            Token::SoftError(
                box Token::IntegerLiteral(0),
                "integer is way too large, did the cat step on the numpad?".to_string())
        }, text)
    },
    // String literals
    // Note that Unicode escapes are not required.
    r#""([^"\\]|\\.)*""# => match unescape(&text[1..text.len()-1]) {
        Ok(s) => (Token::StringLiteral(s), text),
        Err((e, sp)) => (Token::SoftError(box Token::StringLiteral(String::new()), e), sp),
    },
    // Check for unterminated string constants.
    r#""([^"\\]|\\.)*"# => (Token::Error("unterminated string constant".to_string()), text),
    r#"'[^'\\]'"# => (Token::CharacterLiteral(text.char_at(1)), text),

    r#"'\\[0-7]'"# => unescape_char(text),
    r#"'\\[0-7][0-7]'"# => unescape_char(text),
    r#"'\\[0-3][0-7][0-7]'"# => unescape_char(text),
    r#"'\\.'"# => unescape_char(text),

    r#"'(.|[^']*)'"# => (Token::SoftError(box Token::CharacterLiteral(' '), "invalid character literal".to_string()), text),
    r#"'"# => (Token::Error("unterminated character literal".to_string()), text),
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
    unescape_c(text: 'a) -> Result<char, (String, &'a str)>;

    r"\\[0-3][0-7][0-7]" => Ok(octal(&text[1..])),
    r"\\[0-7][0-7]" => Ok(octal(&text[1..])),
    r"\\[0-7]" => Ok(octal(&text[1..])),
    r"\\b" => Ok('\u{0008}'),
    r"\\t" => Ok('\t'),
    r"\\n" => Ok('\n'),
    r"\\f" => Ok('\u{000c}'),
    r"\\r" => Ok('\r'),
    r#"\\""# => Ok('"'),
    r"\\'" => Ok('\''),
    r"\\\\" => Ok('\\'),
    r"\\." => Err(("bad escape sequence".to_string(), text)),
    r"\\" => Err(("unterminated escape sequence".to_string(), text)),
    r"[^\\]" => Ok(text.char_at(0)),
}

fn unescape(mut s: &str) -> Result<String, (String, &str)> {
    let mut r = String::with_capacity(s.len());
    while let Some(result) = unescape_c(&mut s) {
        r.push(try!(result));
    }
    assert!(s.is_empty());
    Ok(r)
}

fn unescape_char(text: &str) -> (Token, &str) {
    match unescape(&text[1..text.len()-1]) {
        Ok(s) => (Token::CharacterLiteral(s.char_at(0)), text),
        Err((e, sp)) => (Token::SoftError(box Token::CharacterLiteral(' '), e), sp),
    }
}

pub struct Tokenizer<'a> {
    slice: &'a str,
    full_slice: &'a str,
    file_ix: usize,
    pub tokens: Vec<(Token, &'a str)>,
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = (Token, Span);

    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            if self.slice.is_empty()
                // Allow input to end with \x1a ($3.5)
                || self.slice == "\x1a" {
                return None;
            }
            // This can never return `None`, since the input is nonempty,
            // and the tokenizer can always match any character.
            let (token, text) = next_token(&mut self.slice).unwrap();
            let offset = self.full_slice.subslice_offset(text);
            let span = Span {
                lo: offset as u32,
                hi: (offset + text.len()) as u32,
                file: self.file_ix,
            };
            let token = match token {
                Token::SoftError(box replacement, error) => {
                    span_error!(span, "{}", error);
                    replacement
                }
                Token::Error(error) => {
                    span_fatal!(span, "{}", error)
                }
                t => t
            };
            if token_filter(&token) {
                self.tokens.push((token.clone(), text));
                return Some((token, span));
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

impl<'a> Tokenizer<'a> {
    pub fn new(s: &'a str, ix: usize) -> Tokenizer<'a> {
        Tokenizer { slice: s, full_slice: s, file_ix: ix, tokens: vec![] }
    }
}
