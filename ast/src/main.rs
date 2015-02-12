#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;
extern crate getopts;
extern crate term;

use getopts::{getopts, optflag};

use std::os;
use std::cell::RefCell;
use std::rt::unwind;

use parser::make_ast;
use tokenizer::Tokenizer;
use weed::weed;
use span::Span;
use context::{Context, CONTEXT};
use error::{FatalError, ErrorReporter, ERRORS};

pub mod span;
pub mod file;
pub mod context;
pub mod error;
pub mod name;
pub mod ast;
pub mod parser;
pub mod tokenizer;
pub mod weed;

fn driver(ctx: &RefCell<Context>) {
    let opts = &[
        optflag("v", "verbose", "verbose - print parsing debug info")
    ];

    let matches = match getopts(os::args().tail(), opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if matches.opt_present("verbose") {
        ctx.borrow_mut().verbose = true;
    }

    let input_arg = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        // TODO: Should have a print_usage function.
        panic!("No input!");
    };

    let file_ix = ctx.borrow_mut().add_file(Path::new(input_arg)).unwrap();
    let ctx_borrow = ctx.borrow(); // XXX: this will cause problems later maybe?
    let file = ctx_borrow.file(file_ix);
    let mut tokenizer = Tokenizer::new(&*file.contents, file_ix);
    let ast = match make_ast(tokenizer.by_ref()) {
        Ok(res) => res,
        Err((Some((_, bad_span)), message)) => {
            ctx.borrow().span_error(bad_span, format!("unexpected token; {}", message));
            return
        }
        Err((None, message)) => {
            ctx.borrow().span_error(Span {
                lo: file.contents.len() as u32,
                hi: file.contents.len() as u32,
                file: file_ix
            }, format!("unexpected EOF; {}", message));
            return
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
