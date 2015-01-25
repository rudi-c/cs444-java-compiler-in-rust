#![feature(plugin, box_syntax)]
#![allow(unstable)]

#[derive(Show)]
pub enum Ast {
    Assign(Lhs, Rhs),
    Expr(Rhs),
}
#[derive(Show)]
pub enum Lhs {
    Deref(Rhs),
    Var(String),
}
#[derive(Show)]
pub enum Rhs {
    Lhs(Box<Lhs>),
}

#[derive(Show)]
pub enum List<T> {
    Cons(T, Box<List<T>>),
    Empty
}

impl <T> List<T> {
    pub fn toVec(self) -> Vec<T> {
        let mut node = self;
        let mut vec = vec![];
        while let List::Cons(first, rest) = node {
            vec.push(first);
            node = *rest;
        }
        vec
    }

    pub fn toVecReverse(self) -> Vec<T> {
        let mut vec = self.toVec();
        vec.reverse();
        vec
    }
}

#[derive(Show)]
pub enum ImportDeclaration {
    SingleType(QualifiedIdentifier),
    OnDemand(QualifiedIdentifier),
}

#[derive(Show)]
pub enum TypeDeclaration {
    Class(Class),
    Interface(Interface),
}

#[derive(Show)]
pub struct CompilationUnit {
    pub packages: QualifiedIdentifier,
    pub imports : Vec<ImportDeclaration>,
    pub types   : Vec<TypeDeclaration>,
}

#[derive(Show)]
pub struct Class {
    pub name : String,
    pub extends : Option<QualifiedIdentifier>,
    pub implements : Vec<QualifiedIdentifier>,
}

#[derive(Show)]
pub struct Interface {
    pub name : String,
    pub extends : Vec<QualifiedIdentifier>,
}

#[derive(Show)]
pub struct QualifiedIdentifier {
    pub parts : Vec<String>,
}

