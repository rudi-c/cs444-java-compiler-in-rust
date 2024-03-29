#![macro_use]
use std::fmt::{Debug, Formatter, Error};
use std::ops;

pub type Location = u32;
pub type FileId = usize;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct Span {
    pub lo: Location,
    pub hi: Location,
    pub file: FileId,
}

pub trait IntoSpan {
    fn into_span(self) -> Span;
}

impl IntoSpan for Span {
    fn into_span(self) -> Span { self }
}

impl Span {
    pub fn range<T: IntoSpan, U: IntoSpan>(a: T, b: U) -> Span {
        let a = a.into_span();
        let b = b.into_span();
        assert_eq!(a.file, b.file);
        Span { lo: a.lo, hi: b.hi, file: a.file }
    }
}

#[derive(Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.node
    }
}

impl<'a, T> IntoSpan for &'a Spanned<T> {
    fn into_span(self) -> Span { self.span }
}

pub fn spanned<T: IntoSpan, U>(sp: T, n: U) -> Spanned<U> {
    Spanned { span: sp.into_span(), node: n }
}

#[macro_export]
macro_rules! node {
    ($x: pat) => ($crate::span::Spanned { span: _, node: $x });
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        self.node.fmt(f)
    }
}
