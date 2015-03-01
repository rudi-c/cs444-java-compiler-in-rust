#![feature(plugin, box_syntax, advanced_slice_patterns)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;
extern crate rbtree;
extern crate getopts;
extern crate term;

use getopts::{getopts, optflag};

use std::{io, os};
use std::cell::RefCell;
use std::rt::unwind;

use ast::CompilationUnit;
use parser::make_ast;
use tokenizer::Tokenizer;
use weed::weed;
use span::Span;
use context::{Context, CONTEXT};
use error::{FatalError, ErrorReporter, ERRORS};
use name_resolve::name_resolve;

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
pub mod arena;
pub mod name_resolve;
pub mod name_resolve_structs;
pub mod collect_types;
pub mod collect_members;

fn create_ast(ctx: &RefCell<Context>, filename: &str) -> Option<CompilationUnit> {
    let file_ix = ctx.borrow_mut().add_file(Path::new(filename)).unwrap();
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

fn create_multi_ast(ctx: &RefCell<Context>, filename: &str) -> Vec<CompilationUnit> {
    let file_ix = ctx.borrow_mut().add_file(Path::new(filename)).unwrap();
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

fn driver(ctx: &RefCell<Context>) {
    let opts = &[
        optflag("v", "verbose", "verbose - print parsing debug info"),
        optflag("", "multi", "accept concatenated compilation units"),
    ];

    let matches = match getopts(os::args().tail(), opts) {
        Ok(m) => m,
        Err(f) => {
            writeln!(&mut io::stderr(), "{}", f).unwrap();
            os::set_exit_status(1);
            return
        }
    };

    if matches.opt_present("verbose") {
        ctx.borrow_mut().verbose = true;
    }

    if matches.free.is_empty() {
        // TODO: Should have a print_usage function.
        panic!("No input!");
    };

    let mut asts: Vec<CompilationUnit> = vec![];

    for file in matches.free.iter() {
        println!("Parsing file {}...", file);

        if matches.opt_present("multi") {
            asts.extend(create_multi_ast(ctx, file.as_slice()).into_iter());
        } else if let Some(ast) = create_ast(ctx, file.as_slice()) {
            asts.push(ast);
        } else {
            return;
        }

        if ERRORS.with(|v| v.get()) > 0 { return; }
            /*
            Err(_) => {
                // If the verbose flag was on, we would already
                // have printed the tokens.
                if !context.verbose {
                    for &(ref token, ref text) in tokenizer.tokens.iter() {
                        println!("{:<25} {:?}", format!("{:?}", token), text);
                    }
                }
                println!("{:?}", ast);
                os::set_exit_status(42);
            }
            */
    }

    name_resolve(&arena::Arena::new(), &*asts);
}

fn main() {
    unsafe {
        let error = match unwind::try(|| CONTEXT.with(|ctx| driver(ctx))) {
            Err(res) => {
                if res.is::<FatalError>() {
                    true
                } else {
                    // The compiler had a problem
                    os::set_exit_status(1);
                    false
                }
            },
            Ok(_) => ERRORS.with(|v| v.get()) > 0
        };
        if error {
            os::set_exit_status(42);
        }
    }
}
