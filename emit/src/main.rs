#![feature(plugin, box_syntax, advanced_slice_patterns)]
#![allow(unstable)]

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

use getopts::{getopts, optflag};

use emit::emit;

use std::{io, os, thread};
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
        .scoped(|| CONTEXT.with(|ctx| driver(ctx))).join() {
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
