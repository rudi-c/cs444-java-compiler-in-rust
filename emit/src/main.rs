#![feature(plugin, box_syntax, advanced_slice_patterns)]
#![allow(unstable)]

extern crate ast;
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

use descriptors::emit_descriptor;
use method::emit_method;

use getopts::{getopts, optflag};

use std::{io, os};
use std::cell::RefCell;
use std::rt::unwind;

pub mod context;
pub mod mangle;
pub mod descriptors;
pub mod stack;
pub mod code;
pub mod method;

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
    let emit_ctx = context::Context::create(&universe);

    universe.each_type(|tydef| {
        // Emit type descriptors.
        emit_descriptor(&emit_ctx, tydef);
        for method in tydef.method_impls.iter() {
            emit_method(&emit_ctx, *method);
        }
    });
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
