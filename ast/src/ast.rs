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
}

#[derive(Show)]
pub enum PackageDeclaration {
    Path(i32)
}

#[derive(Show)]
pub enum ImportDeclaration {
    Path(i32)
}

#[derive(Show)]
pub enum TypeDeclaration {
    Path(i32)
}

#[derive(Show)]
pub struct CompilationUnit {
    pub packages: Vec<PackageDeclaration>,
    pub imports : Vec<ImportDeclaration>,
    pub types   : Vec<TypeDeclaration>,
}

