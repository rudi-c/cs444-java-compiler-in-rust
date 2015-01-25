#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;

#[derive(Show)]
pub enum Token {
    Identifier(String),

    // Literals. Note that FloatingPointLiteral not required in Joos
    IntegerLiteral(i64),
    BooleanLiteral(bool),
    CharacterLiteral(char),
    StringLiteral(String),
    NullLiteral,

    // Keywords ($3.8)
    // Capitalized because the variants in this enum are constants
    // and some of the variants (e.g. 'Char') can be easily confused
    // with Rust's 'Char' type (the syntax highlighter is among the things
    // confused by it).
    // TODO: Trim out unneeded ones.
    ABSTRACT,
    BOOLEAN,
    BREAK,
    BYTE,
    CASE,
    CATCH,
    CHAR,
    CLASS,
    CONST,
    CONTINUE,
    DEFAULT,
    DO,
    DOUBLE,
    ELSE,
    EXTENDS,
    FINAL,
    FINALLY,
    FLOAT,
    FOR,
    GOTO,
    IF,
    IMPLEMENTS,
    IMPORT,
    INSTANCEOF,
    INT,
    INTERFACE,
    LONG,
    NATIVE,
    NEW,
    PACKAGE,
    PRIVATGE,
    PROTECTED,
    PUBLIC,
    RETURN,
    SHORT,
    STATIC,
    STRICTFP,
    SUPER,
    SWITCH,
    SYNCHRONIZED,
    THIS,
    THROW,
    THROWS,
    TRANSIENT,
    TRY,
    VOID,
    VOLATILE,
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
    Percentage,
    Bang,

    Whitespace,
    Comment,
    Other,
    Error,
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    // TODO:
    // Windows line terminators?

    // Whitespace defined in $3.6
    r#"[ \t\n][ \t\n]*"# => (Token::Whitespace, text),

    // Comments defined in $3.7
    // TODO: Make /* */ work for multi-line comments.
    r#"/[*](.|\n)*[*]/"# => (Token::Comment, text),
    r#"//.*\n"# => (Token::Comment, text),

    // Keywords defined in $3.8.
    // Has precedence over identifiers.
    r#"abstract"# => (Token::ABSTRACT, text),
    r#"boolean"# => (Token::BOOLEAN, text),
    r#"break"# => (Token::BREAK, text),
    r#"byte"# => (Token::BYTE, text),
    r#"case"# => (Token::CASE, text),
    r#"catch"# => (Token::CATCH, text),
    r#"char"# => (Token::CHAR, text),
    r#"class"# => (Token::CLASS, text),
    r#"const"# => (Token::CONST, text),
    r#"continue"# => (Token::CONTINUE, text),
    r#"default"# => (Token::DEFAULT, text),
    r#"do"# => (Token::DO, text),
    r#"double"# => (Token::DOUBLE, text),
    r#"else"# => (Token::ELSE, text),
    r#"extends"# => (Token::EXTENDS, text),
    r#"final"# => (Token::FINAL, text),
    r#"finally"# => (Token::FINALLY, text),
    r#"float"# => (Token::FLOAT, text),
    r#"for"# => (Token::FOR, text),
    r#"goto"# => (Token::GOTO, text),
    r#"if"# => (Token::IF, text),
    r#"implements"# => (Token::IMPLEMENTS, text),
    r#"import"# => (Token::IMPORT, text),
    r#"instanceof"# => (Token::INSTANCEOF, text),
    r#"int"# => (Token::INT, text),
    r#"interface"# => (Token::INTERFACE, text),
    r#"long"# => (Token::LONG, text),
    r#"native"# => (Token::NATIVE, text),
    r#"new"# => (Token::NEW, text),
    r#"package"# => (Token::PACKAGE, text),
    r#"privatge"# => (Token::PRIVATGE, text),
    r#"protected"# => (Token::PROTECTED, text),
    r#"public"# => (Token::PUBLIC, text),
    r#"return"# => (Token::RETURN, text),
    r#"short"# => (Token::SHORT, text),
    r#"static"# => (Token::STATIC, text),
    r#"strictfp"# => (Token::STRICTFP, text),
    r#"super"# => (Token::SUPER, text),
    r#"switch"# => (Token::SWITCH, text),
    r#"synchronized"# => (Token::SYNCHRONIZED, text),
    r#"this"# => (Token::THIS, text),
    r#"throw"# => (Token::THROW, text),
    r#"throws"# => (Token::THROWS, text),
    r#"transient"# => (Token::TRANSIENT, text),
    r#"try"# => (Token::TRY, text),
    r#"void"# => (Token::VOID, text),
    r#"volatile"# => (Token::VOLATILE, text),
    r#"while"# => (Token::WHILE, text),

    // Literals defined in $3.10
    // Note that Octal, Hex and Long literals are not required in Joos.
    // TODO: Check that the minus sign should indeed be part of the literal
    // and not a unary op.
    r#"(|-)0|[1-9][0-9]*"# => (Token::IntegerLiteral(text.parse().unwrap()), text),
    // String literals
    // TODO: Character escapes. Note that Unicode escapes are not required.
    r#""([^"]|\\.)*""# => (Token::StringLiteral(text[1..text.len()-1].to_string()), text),
    r#""([^"]|\\.)*"# => (Token::Error, text), // TODO: What is this for?
    // TODO: Character literals.


    // Identifiers defined in $3.8
    // Note that $ is considered a Java letter.
    r#"[a-zA-Z_$][a-zA-Z0-9_$]*"# => (Token::Identifier(text.to_string()), text),
    r#"true"# => (Token::BooleanLiteral(true), text),
    r#"false"# => (Token::BooleanLiteral(false), text),
    r#"null"# => (Token::NullLiteral, text),

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
    r#"||"# => (Token::OrOr, text),
    r#"&&"# => (Token::AndAnd, text),
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
    r#"%"# => (Token::Percentage, text),
    r#"!"# => (Token::Bang, text),

    r#"="# => (Token::Equals, text),
    r#"\*"# => (Token::Star, text),
    r#"."# => (Token::Other, text),
}

pub fn get_tokens<'l>(input : &'l mut &str) -> Vec<Token> {
    let mut tokens = vec![];
    while input.len() > 0 {
        match next_token(input) {
            Some(c) => {
                println!("{:?}", c);
                if token_filter(&c.0) {
                    tokens.push(c.0);
                }
            }
            None => {
                break;
            }
        }
    }
    tokens
}

fn token_filter(token : &Token) -> bool {
    match *token {
        Token::Whitespace => false,
        Token::Comment => false,
        _ => true
    }
}
