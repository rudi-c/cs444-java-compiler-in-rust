#![feature(box_syntax, box_patterns, slice_patterns, advanced_slice_patterns, core)]

#[macro_use] extern crate ast;
extern crate rbtree;
extern crate ivar;

// pub use middle::{Universe, Package, PackageRef};
pub use ast::{name, span, error};

macro_rules! matches {
    ($p: pat, $e: expr) => (if let $p = $e { true } else { false });
}

pub mod arena;
pub mod middle;
pub mod lang_items;
pub mod name_resolve;
pub mod collect_types;
pub mod collect_members;
pub mod tycheck;
pub mod typed_walker;
pub mod ordering;
pub mod reachability;
pub mod uses;
pub mod eval;
