use std::{cmp, fmt, mem};
use std::cell::RefCell;
use std::collections::{hash_map, HashMap};

use span::{Span, Spanned, spanned};

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
                map.insert(sym.to_string(), ret);
                vec.push(sym.to_string());
                ret
            })
        })
    }
}

impl Str for Symbol {
    fn as_slice(&self) -> &str {
        // This is safe because the String is never mutated or deleted.
        SYMBOL_NAMES.with(|cell| unsafe {
            mem::transmute::<&str, &str>(cell.borrow()[self.0 as usize].as_slice())
        })
    }
}
impl fmt::Show for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.as_slice().fmt(f)
    }
}
impl fmt::String for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.as_slice().fmt(f)
    }
}

/// A name mentioned in the program, together with the location where it is mentioned. It always
/// refers to a fragment of source code, not to any references generated inside the compiler.
pub type Ident = Spanned<Symbol>;

impl fmt::String for Ident {
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

impl Str for Ident {
    fn as_slice(&self) -> &str { self.node.as_slice() }
}

#[derive(Show, Clone)]
pub struct QualifiedIdentifier_ {
    pub parts: Vec<Ident>,
}
/// A dotted identifier, such as `a.b.c`. It is possible for a `QualifiedIdentifier` to actually
/// refer to a field access, e.g. if `a.b` is actually the name of some object and `c` is a field
/// in that object.
pub type QualifiedIdentifier = Spanned<QualifiedIdentifier_>;

impl QualifiedIdentifier {
    pub fn new(parts: Vec<Ident>) -> QualifiedIdentifier {
        assert!(parts.len() > 0);
        spanned(Span::range(parts.first().unwrap().span, parts.last().unwrap().span),
                QualifiedIdentifier_ { parts: parts })
    }

    pub fn as_symbol(&self) -> Symbol {
        let mut string_repr = String::new();
        for i in range(0, self.node.parts.len()) {
            if i != 0 {
                string_repr.push('.');
            }
            string_repr.push_str(self.node.parts[i].as_slice());
        }
        return Symbol::from_str(string_repr.as_slice());
    }

    // Returns a new QualifiedIdentifier with an identifier appended.
    pub fn append_ident(&self, identifier: &Ident) -> QualifiedIdentifier {
        let mut new_identifier = self.clone();
        new_identifier.node.parts.push(identifier.clone());
        new_identifier
    }
}

/// The fully-resolved name of something in the program.
/// In particular, equal `Name`s always refer to the same thing.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Copy, Clone)]
pub struct Name(u32);

thread_local!(static NAMES: RefCell<Vec<Symbol>> = RefCell::new(Vec::new()));

impl Name {
    pub fn fresh(sym: Symbol) -> Name {
        NAMES.with(move |cell| {
            let mut n = cell.borrow_mut();
            let ret = Name(n.len() as u32);
            n.push(sym);
            ret
        })
    }
}

impl fmt::Show for Name {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        NAMES.with(|cell| cell.borrow()[self.0 as usize].fmt(f))
    }
}
