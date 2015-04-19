use std::{fmt, mem};
use std::borrow::ToOwned;
use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::AsRef;

use span::{Span, Spanned, IntoSpan};

/// The name of something in the program.
/// However, equal `Symbol`s could refer to different things, depending on context.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Symbol(u32);

thread_local!(static SYMBOL_FROM_NAME: RefCell<HashMap<String, Symbol>> = RefCell::new(HashMap::new()));
thread_local!(static SYMBOL_NAMES: RefCell<Vec<String>> = RefCell::new(Vec::new()));

impl Symbol {
    /// Get the symbol for the given string, or create a new one if it doesn't already exist.
    pub fn from_str(sym: &str) -> Symbol {
        SYMBOL_FROM_NAME.with(|map_cell| {
            let mut map = map_cell.borrow_mut();
            if let Some(r) = map.get(sym) {
                return *r;
            }
            SYMBOL_NAMES.with(move |vec_cell| {
                let mut vec = vec_cell.borrow_mut();
                let ret = Symbol(vec.len() as u32);
                map.insert(sym.to_owned(), ret);
                vec.push(sym.to_owned());
                ret
            })
        })
    }
}

impl AsRef<str> for Symbol {
    fn as_ref<'a>(&'a self) -> &'a str {
        // This is safe because the String is never mutated or deleted.
        SYMBOL_NAMES.with(|cell| unsafe {
            mem::transmute::<&str, &str>(cell.borrow()[self.0 as usize].as_ref())
        })
    }
}
impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.as_ref().fmt(f)
    }
}
impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.as_ref().fmt(f)
    }
}

/// A name mentioned in the program, together with the location where it is mentioned. It always
/// refers to a fragment of source code, not to any references generated inside the compiler.
pub type Ident = Spanned<Symbol>;

impl fmt::Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.node.fmt(f)
    }
}

/// The implementations of `PartialEq` and `Eq` for `Ident` are for convenience. They only compare
/// the identifier's contained `Symbol`s, ignoring their lexical contexts.
impl PartialEq for Ident {
    fn eq(&self, other: &Ident) -> bool { self.node.eq(&other.node) }
    fn ne(&self, other: &Ident) -> bool { self.node.ne(&other.node) }
}
impl Eq for Ident { }

impl AsRef<str> for Ident {
    fn as_ref(&self) -> &str { self.node.as_ref() }
}

/// A dotted identifier, such as `a.b.c`. It is possible for a `QualifiedIdentifier` to actually
/// refer to a field access, e.g. if `a.b` is actually the name of some object and `c` is a field
/// in that object.
#[derive(Debug, Clone)]
pub struct QualifiedIdentifier {
    pub parts: Vec<Ident>,
}

impl QualifiedIdentifier {
    pub fn new(parts: Vec<Ident>) -> QualifiedIdentifier {
        assert!(parts.len() > 0);
        QualifiedIdentifier { parts: parts }
    }
}

impl<'a> IntoSpan for &'a QualifiedIdentifier {
    fn into_span(self) -> Span {
        Span::range(self.parts.first().unwrap(), self.parts.last().unwrap())
    }
}

impl fmt::Display for QualifiedIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        Qualified(self.parts.iter()).fmt(f)
    }
}

// A helper type to print qualified names.
pub struct Qualified<T>(pub T);

impl<'a, T: Iterator + Clone> fmt::Display for Qualified<T> where <T as Iterator>::Item: fmt::Display {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        for (i, part) in self.0.clone().enumerate() {
            if i != 0 {
                try!(f.write_str("."));
            }
            try!(part.fmt(f));
        }
        Ok(())
    }
}

/// The fully-resolved name of something in the program.
/// In particular, equal `Name`s always refer to the same thing.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Name(u32);

thread_local!(static NAMES: RefCell<Vec<String>> = RefCell::new(Vec::new()));

impl Name {
    pub fn fresh(sym: String) -> Name {
        NAMES.with(move |cell| {
            let mut n = cell.borrow_mut();
            let ret = Name(n.len() as u32);
            n.push(sym);
            ret
        })
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        NAMES.with(|cell| cell.borrow()[self.0 as usize].fmt(f))
    }
}
impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        NAMES.with(|cell| cell.borrow()[self.0 as usize].fmt(f))
    }
}
