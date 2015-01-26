#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate lalrgen;

use std::os;

use ast::*;
use ast::List::*;
use parser::make_cst;
use tokenizer::Token;
use tokenizer::Token::*;
use tokenizer::get_tokens;
use weed::weed;

mod ast;
mod parser;
mod tokenizer;
mod weed;

// See Chapter 18 for full grammar (p.449) (pdf p.475)

parser! parse {
    Token;

    // Compilation unit ($7.3)
    root : CompilationUnit {
        packageDeclaration[pkg] importDeclarations[imports] typeDeclarations[types] =>
            CompilationUnit { packages: pkg,
                              imports: imports.toVecReverse(),
                              types: types.toVecReverse() },
    }

    // Package declarations ($7.4)
    packageDeclaration : QualifiedIdentifier {
        PACKAGE qualifiedIdentifier[ident] Semicolon => ident,
    }

    // Import declarations ($7.5)
    importDeclaration : ImportDeclaration {
        IMPORT qualifiedIdentifier[ident] Semicolon => ImportDeclaration::SingleType(ident),
        // TODO: Currently causes a shift-reduce conflict, but I don't think that it should.
        // IMPORT qualifiedIdentifier[ident] Dot Star Semicolon => ident,
    }

    importDeclarations : List<ImportDeclaration> {
        => Empty,
        importDeclarations[dcls] importDeclaration[dcl] => Cons(dcl, box dcls),
    }

    // Top-level type declarations ($7.6)
    // Note that Joos 1W only supports one of these per file, but I leave that to the
    // weeding phase so it's easier to output a clearer error message.
    typeDeclaration : TypeDeclaration {
        classDeclaration[class] => TypeDeclaration::Class(class),
        interfaceDeclaration[interface] => TypeDeclaration::Interface(interface),
    }

    typeDeclarations : List<TypeDeclaration> {
        => Empty,
        typeDeclarations[dcls] typeDeclaration[dcl] => Cons(dcl, box dcls),
    }

    // Identifiers ($6.7)
    qualifiedIdentifier : QualifiedIdentifier {
        qualifiedIdentifierHelper[list] => QualifiedIdentifier { parts: list.toVecReverse() }
    }

    qualifiedIdentifierHelper : List<String> {
        // Helper to avoid have to use .toVecReverse everytime qualified identifier are
        // used, which is quite often.
        Identifier[i] => match i {
            Identifier(ident) => Cons(ident, box Empty),
            _ => unreachable!(),
        },
        qualifiedIdentifierHelper[list] Dot Identifier[i] => match i {
            Identifier(ident) => Cons(ident, box list),
            _ => unreachable!(),
        },
    }

    qualifiedIdentifierList : List<QualifiedIdentifier> {
        qualifiedIdentifier[i] => Cons(i, box Empty),
        qualifiedIdentifierList[list] Comma qualifiedIdentifier[i] => Cons(i, box list),
    }

    // Classes ($8.1)
    classDeclaration : Class {
        CLASS Identifier[name] superType[s] interfaceImplementations[impls] classBody[x] =>
            match name {
                Identifier(ident) => Class { name: ident, extends: s, implements: impls },
                _ => unreachable!(),
            }
    }

    superType : Option<QualifiedIdentifier> {
        => None,
        EXTENDS qualifiedIdentifier[extension] => Some(extension)
    }

    interfaceImplementations : Vec<QualifiedIdentifier> {
        => vec![],
        IMPLEMENTS interfaceImplementationList[impls] => impls.toVecReverse(),
    }

    interfaceImplementationList : List<QualifiedIdentifier> {
        qualifiedIdentifier[i] => Cons(i, box Empty),
        interfaceImplementationList[impls] Comma qualifiedIdentifier[i] => Cons(i, box impls),
    }

    // Class body ($8.1.5)
    classBody : i32 {
        LBrace RBrace => 0
    }

    // Interfaces ($9.1)
    interfaceDeclaration : Interface {
        INTERFACE Identifier[name] interfaceExtensions[exts] interfaceBody[x] =>
            match name {
                Identifier(ident) => Interface { name: ident, extends: exts },
                _ => unreachable!()
            }
    }

    interfaceExtensions : Vec<QualifiedIdentifier> {
        => vec![],
        EXTENDS interfaceExtensionList[impls] => impls.toVecReverse(),
    }

    // TODO: Can we get merge this rule with the one for classes?
    interfaceExtensionList : List<QualifiedIdentifier> {
        qualifiedIdentifier[i] => Cons(i, box Empty),
        interfaceExtensionList[impls] Comma qualifiedIdentifier[i] => Cons(i, box impls),
    }

    // Interface body ($9.1.3)
    interfaceBody : i32 {
        LBrace RBrace => 0
    }
}

fn main() {
    if let Ok(input) = std::io::stdio::stdin().read_to_string() {
        let mut to_scan = input.as_slice();
        let tokens = get_tokens(&mut to_scan);
        let ast = parse(tokens.into_iter());
        println!("{:?}", ast);

        if let Ok(result) = ast {
            let found_error = weed(result);
            if found_error {
                os::set_exit_status(42);
            }
        }
    }
}
