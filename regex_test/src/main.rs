#![feature(box_syntax)]
#![allow(unstable)]

extern crate regex_dfa;
extern crate dfagen;
extern crate syntax;

use regex_dfa::{Regex, Dfa};
use regex_dfa::dfa::Normalize;
use regex_dfa::derivatives::Differentiable;

fn main() {
    while let Ok(line) = std::io::stdio::stdin().read_line() {
        match Regex::new(line.trim()) {
            Err(e) => println!("error: {:?}", e),
            Ok(x) => {
                println!("ok: {:?}", x);
                let x = x.normalize();
                println!("{:?}", x);
                println!("{:?}", x.derivative().map(Normalize::normalize));
                let dfa = Dfa::from_derivatives(vec![x, Regex::Null]);
                println!("{:?}", dfa);
                println!("{}", syntax::print::pprust::item_to_string(
                        &*dfagen::dfagen(&dfa.0, syntax::parse::token::str_to_ident("dfa"), syntax::ast::Visibility::Public, syntax::codemap::DUMMY_SP))
                    );
            }
        }
    }
}
