#![feature(box_syntax)]
#![allow(unstable)]

extern crate regex_dfa;
extern crate dfagen;
extern crate syntax;

use regex_dfa::{Regex, Dfa};

fn main() {
    while let Ok(line) = std::io::stdio::stdin().read_line() {
        match Regex::new(line.trim()) {
            Err(e) => println!("error: {:?}", e),
            Ok(x) => {
                println!("ok: {:?}", x);
                let x = x.simplify();
                println!("{:?}", x);
                println!("{:?}", x.derivative().map(regex_dfa::regex::Regex::simplify));
                let dfa = Dfa::from_regex(x);
                println!("{:?}", dfa);
                println!("{}", syntax::print::pprust::item_to_string(
                        &dfagen::dfagen(&dfa, syntax::parse::token::str_to_ident("dfa"), syntax::ast::Visibility::Public, syntax::codemap::DUMMY_SP))
                    );
            }
        }
    }
}
