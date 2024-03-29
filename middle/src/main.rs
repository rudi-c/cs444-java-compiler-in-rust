#![feature(box_syntax, advanced_slice_patterns, box_patterns)]

#[macro_use] extern crate ast;
extern crate rbtree;
extern crate ivar;
extern crate getopts;

use getopts::{getopts, optflag};

use std::{old_io, env};
use std::cell::RefCell;
use std::rt::unwind;

pub use ast::{name, span, error};
use ast::CompilationUnit;
use ast::parser::make_ast;
use ast::tokenizer::{Token, Tokenizer};
use ast::weed::weed;
use ast::span::Span;
use ast::context::{Context, CONTEXT};
use ast::error::{FatalError, ErrorReporter, ERRORS};
use name_resolve::name_resolve;

use ordering::check_ordering;
use reachability::check_reachability;

macro_rules! matches {
    ($p: pat, $e: expr) => (if let $p = $e { true } else { false });
}

pub mod arena;
pub mod middle;
pub mod lang_items;
pub mod name_resolve;
pub mod collect_types;
pub mod collect_members;
pub mod tycheck;
pub mod typed_walker;
pub mod ordering;
pub mod reachability;
pub mod uses;
pub mod eval;

fn create_ast(ctx: &RefCell<Context>, filename: &str) -> Option<CompilationUnit> {
    let file_ix = match ctx.borrow_mut().add_file(Path::new(filename)) {
        Ok(file) => file,
        Err(err) => {
            println!("error opening {} : {}", filename, err.description());
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

fn create_multi_ast(ctx: &RefCell<Context>, filename: &str) -> Vec<CompilationUnit> {
    let file_ix = match ctx.borrow_mut().add_file(Path::new(filename)) {
        Ok(file) => file,
        Err(err) => {
            println!("error opening {} : {}", filename, err.description());
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
            .position(|&(ref x, _)| if let Token::PACKAGE = *x { true } else { false })
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

    let matches = match getopts(env::args().collect::<Vec<_>>().tail(), opts) {
        Ok(m) => m,
        Err(f) => {
            writeln!(&mut old_io::stderr(), "{}", f).unwrap();
            env::set_exit_status(1);
            return
        }
    };

    if matches.opt_present("verbose") {
        ctx.borrow_mut().verbose = true;
    }

    if matches.free.is_empty() {
        // TODO: Should have a print_usage function.
        println!("No input file specified.");
        return;
    };

    let mut asts: Vec<CompilationUnit> = vec![];

    for file in matches.free.iter() {
        if ctx.borrow().verbose {
            println!("Parsing file {}...", file);
        }

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
                env::set_exit_status(42);
            }
            */
    }

    let arena = arena::Arena::new();
    let universe = name_resolve(&arena, &*asts);

    if ERRORS.with(|v| v.get()) > 0 {
        // Some errors occurred. It's not a good idea to continue, to avoid crashing the compiler.
        return;
    }

    // Static analysis
    check_ordering(&universe);
    check_reachability(&universe);
}

fn main() {
    unsafe {
        let error = match unwind::try(|| CONTEXT.with(|ctx| driver(ctx))) {
            Err(res) => {
                if res.is::<FatalError>() {
                    true
                } else {
                    // The compiler had a problem
                    env::set_exit_status(1);
                    false
                }
            },
            Ok(_) => ERRORS.with(|v| v.get()) > 0
        };
        if error {
            env::set_exit_status(42);
        }
    }
}
