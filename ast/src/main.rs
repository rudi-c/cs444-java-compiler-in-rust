#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[no_link] #[plugin] extern crate lalrgen;

use std::os;

use ast::*;
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
    root: CompilationUnit {
        packageDeclaration[pkg] importDeclarations[imports] typeDeclarations[types] =>
            CompilationUnit { packages: pkg,
                              imports: imports,
                              types: types },
    }

    // Package declarations ($7.4)
    packageDeclaration: QualifiedIdentifier {
        PACKAGE qualifiedIdentifier[ident] Semicolon => ident,
    }

    // Import declarations ($7.5)
    importDeclaration: ImportDeclaration {
        IMPORT qualifiedIdentifier[ident] Semicolon => ImportDeclaration::SingleType(ident),
        // TODO: Currently causes a shift-reduce conflict, but I don't think that it should.
        // IMPORT qualifiedIdentifier[ident] Dot Star Semicolon => ident,
    }

    importDeclarations: Vec<ImportDeclaration> {
        => vec![],
        importDeclarations[mut dcls] importDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Top-level type declarations ($7.6)
    // Note that Joos 1W only supports one of these per file, but I leave that to the
    // weeding phase so it's easier to output a clearer error message.
    typeDeclaration: TypeDeclaration {
        classDeclaration[class] => TypeDeclaration::Class(class),
        interfaceDeclaration[interface] => TypeDeclaration::Interface(interface),
    }

    typeDeclarations: Vec<TypeDeclaration> {
        => vec![],
        typeDeclarations[mut dcls] typeDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Identifiers ($6.7)
    qualifiedIdentifier: QualifiedIdentifier {
        qualifiedIdentifierHelper[list] => QualifiedIdentifier { parts: list }
    }

    qualifiedIdentifierHelper: Vec<String> {
        // Helper to avoid have to use .toVecReverse everytime qualified identifier are
        // used, which is quite often.
        Identifier[i] => match i {
            Identifier(ident) => vec![ident],
            _ => unreachable!(),
        },
        qualifiedIdentifierHelper[mut list] Dot Identifier[i] => match i {
            Identifier(ident) => { list.push(ident); list },
            _ => unreachable!(),
        },
    }

    qualifiedIdentifierList: Vec<QualifiedIdentifier> {
        qualifiedIdentifier[i] => vec![i],
        qualifiedIdentifierList[mut list] Comma qualifiedIdentifier[i] => { list.push(i); list },
    }

    // Classes ($8.1)
    classDeclaration: Class {
        modifierList[mods] CLASS Identifier[name] superType[s]
                interfaceImplementations[impls] classBody[x] =>
            match name {
                Identifier(ident) => Class { name: ident, modifiers: mods,
                                             extends: s, implements: impls },
                _ => unreachable!(),
            }
    }

    superType: Option<QualifiedIdentifier> {
        => None,
        EXTENDS qualifiedIdentifier[extension] => Some(extension)
    }

    interfaceImplementations: Vec<QualifiedIdentifier> {
        => vec![],
        IMPLEMENTS interfaceList[impls] => impls,
    }

    // Class body ($8.1.5)
    classBody: i32 {
        LBrace RBrace => 0
    }

    // Interfaces ($9.1)
    interfaceDeclaration: Interface {
        modifierList[mods] INTERFACE Identifier[name]
                interfaceExtensions[exts] interfaceBody[x] =>
            match name {
                Identifier(ident) => Interface { name: ident,
                                                 modifiers: mods,
                                                 extends: exts },
                _ => unreachable!()
            }
    }

    interfaceExtensions: Vec<QualifiedIdentifier> {
        => vec![],
        EXTENDS interfaceList[impls] => impls,
    }

    // Common to classes and interfaces

    interfaceList: Vec<QualifiedIdentifier> {
        qualifiedIdentifier[i] => vec![i],
        interfaceList[mut impls] Comma qualifiedIdentifier[i] => { impls.push(i); impls }
    }

    modifierList: Vec<Modifier> {
        => vec![],
        modifierList[mut list] modifier[m] => { list.push(m); list }
    }

    modifier: Modifier {
        PUBLIC[_] => Modifier::Public,
        PROTECTED[_] => Modifier::Protected,
        PRIVATE[_] => Modifier::Private,
        ABSTRACT[_] => Modifier::Abstract,
        STATIC[_] => Modifier::Static,
        FINAL[_] => Modifier::Final,
    }

    // Interface body ($9.1.3)
    interfaceBody: i32 {
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
