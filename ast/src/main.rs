#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate dfagen;
#[no_link] #[plugin] extern crate lalrgen;

use std::os;

use parser::make_ast;
use tokenizer::{Token, tokenizer};
use weed::weed;

mod ast;
mod parser;
mod tokenizer;
mod weed;

fn main() {
    if let Ok(input) = std::io::stdio::stdin().read_to_string() {
        let ast = make_ast(tokenizer(&*input));
        println!("{:?}", ast);

        if let Ok(result) = ast {
            let found_error = weed(result);
            if found_error {
                os::set_exit_status(42);
            }
        }
    }
}
