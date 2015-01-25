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
pub enum CompilationUnit {
    Declarations(List<PackageDeclaration>,
                 List<ImportDeclaration>,
                 List<TypeDeclaration>)
}

