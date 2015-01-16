#![feature(plugin)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;

#[derive(Show)]
enum Token {
    Ident(String),
    IntLit(i64),
    StringLit(String),
    Whitespace,
    Other,
    Error,
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    r#"[a-zA-Z_][a-zA-Z0-9_]*"# => (Token::Ident(text.to_string()), text),
    r#""([^"]|\\.)*""# => (Token::StringLit(text[1..text.len()-1].to_string()), text),
    r#""([^"]|\\.)*"# => (Token::Error, text),
    r#"(|-)0|[1-9][0-9]*"# => (Token::IntLit(text.parse().unwrap()), text),
    r#"[ \t\n][ \t\n]*"# => (Token::Whitespace, text),
    r#"."# => (Token::Other, text),
}

fn main() {
    while let Ok(line) = std::io::stdio::stdin().read_line() {
        let mut to_scan = line.as_slice();
        while to_scan.len() > 0 {
            match next_token(&mut to_scan) {
                Some(c) => {
                    println!("{:?}", c);
                }
                None => {
                    println!("---- {}", to_scan);
                    break;
                }
            }
        }
    }
}
