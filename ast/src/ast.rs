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
    pub imports: Vec<ImportDeclaration>,
    pub types: Vec<TypeDeclaration>,
}

#[derive(Show)]
pub struct Class {
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub extends: Option<QualifiedIdentifier>,
    pub implements: Vec<QualifiedIdentifier>,
}

#[derive(Show)]
pub struct Interface {
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub extends: Vec<QualifiedIdentifier>,
}

#[derive(Show)]
pub enum Modifier {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
}

#[derive(Show)]
pub struct QualifiedIdentifier {
    pub parts: Vec<String>,
}

