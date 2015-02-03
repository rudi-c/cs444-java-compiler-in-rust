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
    LB,
    RB,
}

scanner! {
    next_token(text: 'a) -> (Token, &'a str);

    "if" => (IF, text),
    "else" => (ELSE, text),
    ";" => (SEMI, text),
    r"\(" => (LEFT, text),
    r"\)" => (RIGHT, text),
    r"\[" => (LB, text),
    r"\]" => (RB, text),

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
enum Stmt {
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
}

#[derive(Show)]
enum Ty {
    Id(String),
    Arr(Box<Ty>),
}

#[derive(Show)]
enum Expr {
    Id(String),
    Cast(Ty, Box<Expr>),
    Ix(Box<Expr>, Box<Expr>),
}

#[derive(Show)]
enum TyExpr {
    Id(String),
}

impl TyExpr {
    fn into_expr(self) -> Expr {
        match self {
            TyExpr::Id(id) => Expr::Id(id),
        }
    }
    fn into_ty(self) -> Ty {
        match self {
            TyExpr::Id(id) => Ty::Id(id),
        }
    }
}

macro_rules! spanned {
    () => (span!());
}

parser! parse {
    Token;
    ();
    (a, b) { }

    /*
    stmt: Stmt {
        expr[x] SEMI => Stmt::Expr(x),
        #[no_reduce(ELSE)]
        IF LEFT expr[x] RIGHT stmt[t] => Stmt::If(x, box t, None),
        IF LEFT expr[x] RIGHT stmt[t] ELSE stmt[f] => Stmt::If(x, box t, Some(box f)),
    }

    ty: Ty {
        Ident(n) => Ty::Id(n),
        ty_and_not_expr[t] => t,
    }

    ty_and_not_expr: Ty {
        ty_arr[t] => Ty::Arr(box t),
        ty_and_not_expr[t] LB RB => Ty::Arr(box t),
    }

    ty_arr: Ty {
        Ident(n) LB RB => Ty::Arr(box Ty::Id(n)),
    }

    expr_and_not_ty: Expr {
        LEFT ty_and_not_expr[t] RIGHT expr[x] => Expr::Cast(t, box x),
        LEFT Ident(t) RIGHT expr[x] => Expr::Cast(Ty::Id(t), box x),
        LEFT Ident(x) RIGHT => Expr::Id(x),
        LEFT expr_and_not_ty[x] RIGHT => x,
        Ident(x) LB expr[y] RB => Expr::Ix(box Expr::Id(x), box y),
    }

    expr: Expr {
        Ident(x) => Expr::Id(x),
        expr_and_not_ty[x] => x,
    }
    */
    derp: () {
        Ident => spanned!()
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
        println!("{:?}", parse(line.into_iter().map(|x| (x, ()))));
    }
}
