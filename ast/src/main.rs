#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate lalrgen;

use ast::*;
use ast::List::*;
use parser::make_cst;
use tokenizer::Token;
use tokenizer::Token::*;
use tokenizer::get_tokens;

mod ast;
mod parser;
mod tokenizer;

// See Chapter 18 for full grammar (p.449) (pdf p.475)

parser! parse {
    Token;

    root : CompilationUnit {
        packageDeclarations[pkgs] importDeclarations[imports] typeDeclarations[types] =>
            CompilationUnit { packages: pkgs.toVec(),
                              imports: imports.toVec(),
                              types: types.toVec() },
    }

    packageDeclaration : PackageDeclaration {
        PACKAGE Identifier[path] Semicolon => PackageDeclaration::Path(0),
    }

    importDeclaration : ImportDeclaration {
        IMPORT Identifier[path] Semicolon => ImportDeclaration::Path(1),
    }

    typeDeclaration : TypeDeclaration {
        BREAK Identifier[path] Semicolon => TypeDeclaration::Path(2),
    }

    packageDeclarations : List<PackageDeclaration> {
        packageDeclaration[dcl] => Cons(dcl, box Empty),
        packageDeclarations[dcls] packageDeclaration[dcl] => Cons(dcl, box dcls),
    }

    importDeclarations : List<ImportDeclaration> {
        importDeclaration[dcl] => Cons(dcl, box Empty),
        importDeclarations[dcls] importDeclaration[dcl] => Cons(dcl, box dcls),
    }

    typeDeclarations : List<TypeDeclaration> {
        typeDeclaration[dcl] => Cons(dcl, box Empty),
        typeDeclarations[dcls] typeDeclaration[dcl] => Cons(dcl, box dcls),
    }
}

fn main() {
    if let Ok(input) = std::io::stdio::stdin().read_to_string() {
        let mut to_scan = input.as_slice();
        let tokens = get_tokens(&mut to_scan);
        let ast = parse(tokens.into_iter());

        println!("{:?}", ast);
    }
}
