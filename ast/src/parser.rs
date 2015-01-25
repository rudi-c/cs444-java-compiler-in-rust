#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate lalrgen;

use ast::*;
use tokenizer::*;
use tokenizer::Token::*;

parser! parse {
    Token;

    stmt: Ast {
        lhs[l] Equals rhs[r] => Ast::Assign(l, r),
        rhs[x] => Ast::Expr(x),
    }

    lhs: Lhs {
        Star rhs[v] => Lhs::Deref(v),
        Identifier[i] => match i {
            Identifier(s) => Lhs::Var(s),
            _ => unreachable!(),
        },
    }

    rhs: Rhs {
        lhs[v] => Rhs::Lhs(box v),
    }
}

pub fn make_cst(tokens: &Vec<(Token, &str)>) {
    let mut tokens_only: Vec<Token> = vec![];
    //let tokens_only = tokens.iter().map(|&t|  t.0);
    println!("{:?}", parse(tokens_only.into_iter()));
}


