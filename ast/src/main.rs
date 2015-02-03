#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;
extern crate getopts;
use getopts::{getopts, optflag};

use std::os;
use std::io::fs::File;

use parser::make_ast;
use tokenizer::make_tokenizer;
use weed::weed;

mod ast;
mod parser;
mod tokenizer;
#[macro_use]
mod weed;

fn main() {
    let mut verbose = false;
    let opts = &[
        optflag("v", "verbose", "verbose - print parsing debug info")
    ];

    let matches = match getopts(os::args().tail(), opts) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if matches.opt_present("verbose") {
        verbose = true;
    }

    let input_arg = if !matches.free.is_empty() {
        matches.free[0].clone()
    } else {
        // TODO: Should have a print_usage function.
        panic!("No input!");
    };

    let input_path = Path::new(input_arg);
    let mut file = File::open(&input_path).unwrap();
    let input = file.read_to_string().unwrap();

    let mut tokenizer = make_tokenizer(&*input);
    let ast = make_ast(tokenizer.by_ref());

    match ast {
        Ok(ref result) => {
            if verbose {
                for &(ref token, ref text) in tokenizer.tokens.iter() {
                    // XXX: the "{:?}" formatter doesn't seem to accept
                    // alignment
                    println!("{:<25} {:?}", format!("{:?}", token), text);
                }
                println!("{:?}", ast);
            }

            let found_error = weed(result, input_path.filestem_str().unwrap());
            if found_error {
                os::set_exit_status(42);
            }
        },
        Err(_) => {
            // If the verbose flag was on, we would already
            // have printed the tokens.
            if !verbose {
                for &(ref token, ref text) in tokenizer.tokens.iter() {
                    println!("{:<25} {:?}", format!("{:?}", token), text);
                }
            }
            println!("{:?}", ast);
            os::set_exit_status(42);
        }
    }
}
