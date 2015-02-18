#![macro_use]
use std::fmt::{Show, Formatter, Error};

pub type Location = u32;
pub type FileId = usize;

#[derive(Show, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Span {
    pub lo: Location,
    pub hi: Location,
    pub file: FileId,
}

impl Span {
    pub fn range(a: Span, b: Span) -> Span {
        assert_eq!(a.file, b.file);
        Span { lo: a.lo, hi: b.hi, file: a.file }
    }
}

#[derive(Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

pub fn spanned<T>(sp: Span, n: T) -> Spanned<T> {
    Spanned { span: sp, node: n }
}

#[macro_export]
macro_rules! node {
    ($x: pat) => (Spanned { span: _, node: $x });
}

impl<T: Show> Show for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.node.fmt(f)
    }
}
