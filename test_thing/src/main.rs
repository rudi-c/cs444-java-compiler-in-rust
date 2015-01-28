#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;

use Token::*;

#[derive(Show)]
enum Token {
    Ident(String),
    IntLit(i64),
    StringLit(String),
    Whitespace,
    Other,
    Error,
    Equals,
    Star,
    IF,
    ELSE,
    SEMI,
    LEFT,
    RIGHT,
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    "if" => (IF, text),
    "else" => (ELSE, text),
    ";" => (SEMI, text),
    r"\(" => (LEFT, text),
    r"\)" => (RIGHT, text),

    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => (Token::Ident(text.to_string()), text),
    r#""([^"]|\\.)*""# => (Token::StringLit(text[1..text.len()-1].to_string()), text),
    r#""([^"]|\\.)*"# => (Token::Error, text),
    r#"(|-)0|[1-9][0-9]*"# => (Token::IntLit(text.parse().unwrap()), text),
    r#"[ \t\n][ \t\n]*"# => (Token::Whitespace, text),
    r#"="# => (Token::Equals, text),
    r#"\*"# => (Token::Star, text),
    r#"."# => (Token::Other, text),
}


#[derive(Show)]
enum Ast {
    Assign(Lhs, Rhs),
    Expr(Rhs),
}
#[derive(Show)]
enum Lhs {
    Deref(Rhs),
    Var(String),
}
#[derive(Show)]
enum Rhs {
    Lhs(Box<Lhs>),
}

parser! parse {
    Token;

    stmt: () {
        expr SEMI => (),
        #[no_reduce(ELSE)]
        IF LEFT expr RIGHT stmt => (),
        IF LEFT expr RIGHT stmt ELSE stmt => (),
    }

    expr: () {
        Ident => (),
    }
}

fn main() {
    while let Ok(line) = std::io::stdio::stdin().read_line() {
        let mut to_scan = line.as_slice();
        let mut line = vec![];
        while to_scan.len() > 0 {
            match next_token(&mut to_scan) {
                Some(c) => {
                    println!("{:?}", c);
                    if let Whitespace(..) = c.0 {
                        // skip
                    } else {
                        line.push(c.0);
                    }
                }
                None => {
                    println!("---- {}", to_scan);
                    break;
                }
            }
        }
        println!("{:?}", parse(line.into_iter()));
    }
}
