#![feature(plugin, box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, exit_status)]

#[macro_use] extern crate ast;
#[macro_use] extern crate middle;
extern crate getopts;
extern crate rbtree;

use ast::ast::CompilationUnit;
use ast::context::{Context, CONTEXT};
use ast::error::{FatalError, ERRORS};
use ast::{create_ast, create_multi_ast};
use middle::arena::Arena;
use middle::name_resolve::name_resolve;
use middle::ordering::check_ordering;
use middle::reachability::check_reachability;

use getopts::Options;

use emit::emit;

use std::{io, thread, env};
use std::io::Write;
use std::cell::RefCell;

pub mod context;
pub mod mangle;
pub mod code;
pub mod emit;
pub mod descriptors;
pub mod stack;
pub mod method;
pub mod ref_alloc;
pub mod strings;

fn driver(ctx: &RefCell<Context>) {
    let mut opts = Options::new();
    opts.optflag("v", "verbose", "verbose - print parsing debug info");
    opts.optflag("", "multi", "accept concatenated compilation units");

    let matches = match opts.parse(&env::args().collect::<Vec<_>>()[1..]) {
        Ok(m) => m,
        Err(f) => {
            writeln!(&mut io::stderr(), "{}", f).unwrap();
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

    for ref file in matches.free.iter() {
        if ctx.borrow().verbose {
            println!("Parsing file {}...", file);
        }

        if matches.opt_present("multi") {
            asts.extend(create_multi_ast(ctx, &file).into_iter());
        } else if let Some(ast) = create_ast(ctx, &file) {
            asts.push(ast);
        } else {
            return;
        }

        if ERRORS.with(|v| v.get()) > 0 { return; }
    }

    if asts.len() == 0 {
        assert!(ERRORS.with(|v| v.get()) > 0);
        return;
    }

    let arena = Arena::new();
    let universe = name_resolve(&arena, &*asts);

    if ERRORS.with(|v| v.get()) > 0 {
        // Some errors occurred. It's not a good idea to continue, to avoid crashing the compiler.
        return;
    }

    // Static analysis
    check_ordering(&universe);
    check_reachability(&universe);

    if ERRORS.with(|v| v.get()) > 0 {
        return;
    }

    // All checking done. Start emitting code.
    emit(&universe);
}

fn main() {
    let error = match thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(|| {
            CONTEXT.with(|ctx| driver(ctx));
            ERRORS.with(|v| v.get())
        }).unwrap().join() {
        Err(res) => {
            if res.is::<FatalError>() {
                true
            } else {
                // The compiler had a problem
                env::set_exit_status(1);
                false
            }
        },
        Ok(num) => num > 0
    };
    if error {
        env::set_exit_status(42);
    }
}
