#![feature(plugin, box_syntax, advanced_slice_patterns)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;
extern crate term;

use std::cell::RefCell;

pub use ast::*;
use parser::make_ast;
use tokenizer::Tokenizer;
use weed::weed;
use span::Span;
use context::Context;
use error::ErrorReporter;

pub mod span;
pub mod file;
pub mod context;
pub mod error;
pub mod name;
pub mod ast;
pub mod parser;
pub mod tokenizer;
pub mod walker;
pub mod weed;

pub fn create_ast(ctx: &RefCell<Context>, filename: &str) -> Option<CompilationUnit> {
    let file_ix = match ctx.borrow_mut().add_file(Path::new(filename)) {
        Ok(file) => file,
        Err(err) => {
            println!("error opening {} : {}", filename, err.desc);
            return None
        }
    };

    let ctx_borrow = ctx.borrow(); // XXX: this will cause problems later maybe?
    let file = ctx_borrow.file(file_ix);
    let mut tokenizer = Tokenizer::new(&*file.contents, file_ix);
    let ast = match make_ast(tokenizer.by_ref()) {
        Ok(res) => res,
        Err((Some((_, bad_span)), message)) => {
            ctx.borrow().span_error(bad_span, format!("unexpected token; {}", message));
            return None
        }
        Err((None, message)) => {
            ctx.borrow().span_error(Span {
                lo: file.contents.len() as u32,
                hi: file.contents.len() as u32,
                file: file_ix
            }, format!("unexpected EOF; {}", message));
            return None
        }
    };

    if ctx.borrow().verbose {
        for &(ref token, ref text) in tokenizer.tokens.iter() {
            // XXX: the "{:?}" formatter doesn't seem to accept
            // alignment
            println!("{:<25} {:?}", format!("{:?}", token), text);
        }
        println!("{:?}", ast);
    }

    weed(&ast, file.stem(), Span {
        lo: 0,
        hi: file.contents.len() as u32,
        file: file_ix,
    });

    Some(ast)
}

pub fn create_multi_ast(ctx: &RefCell<Context>, filename: &str) -> Vec<CompilationUnit> {
    let file_ix = match ctx.borrow_mut().add_file(Path::new(filename)) {
        Ok(file) => file,
        Err(err) => {
            println!("error opening {} : {}", filename, err.desc);
            return vec![]
        }
    };

    let ctx_borrow = ctx.borrow(); // XXX: this will cause problems later maybe?
    let file = ctx_borrow.file(file_ix);
    let tokens = Tokenizer::new(&*file.contents, file_ix).collect::<Vec<_>>();
    let mut slice = &*tokens;
    let mut ret = vec![];
    while !slice.is_empty() {
        let (cur, next) = slice.split_at(
            slice.iter()
            .skip(1)
            .position(|&(ref x, _)| if let tokenizer::Token::PACKAGE = *x { true } else { false })
            .map(|x| x+1)
            .unwrap_or_else(|| slice.len()));
        slice = next;
        let ast = match make_ast(cur.iter().cloned()) {
            Ok(res) => res,
            Err((Some((_, bad_span)), message)) => {
                ctx.borrow().span_error(bad_span, format!("unexpected token; {}", message));
                continue
            }
            Err((None, message)) => {
                ctx.borrow().span_error(Span {
                    lo: file.contents.len() as u32,
                    hi: file.contents.len() as u32,
                    file: file_ix
                }, format!("unexpected EOF; {}", message));
                continue
            }
        };
        // skip weeding
        ret.push(ast);
    }

    ret
}
