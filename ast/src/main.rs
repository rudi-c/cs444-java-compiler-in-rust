#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;

use std::os;
use std::io::fs::File;

use parser::make_ast;
use tokenizer::tokenizer;
use weed::weed;

mod ast;
mod parser;
mod tokenizer;
mod weed;

fn main() {
    // TODO: Support symbolic links?
    let ref input_arg = os::args()[1];
    let input_path = Path::new(input_arg);
    let mut file = File::open(&input_path).unwrap();
    let input = file.read_to_string().unwrap();

    let ast = make_ast(tokenizer(&*input));
    println!("{:?}", ast);

    match ast {
        Ok(result) => {
            let found_error = weed(result, input_path.filestem_str().unwrap());
            if found_error {
                os::set_exit_status(42);
            }
        },
        Err(_) => {
            os::set_exit_status(42);
        }
    }
}
