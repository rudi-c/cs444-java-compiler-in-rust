#![macro_use]
use std::{fmt, iter};
use std::cell::Cell;
use span::Span;
use context::{Context, CONTEXT};
use term;

// A dummy struct to give `panic!()`,
// which will tell the `try` in `main` to exit with status 42.
pub struct FatalError;

pub trait ErrorReporter {
    fn span_msg<S: fmt::String>(&self, span: Span, err_type: &str, colour: term::color::Color, err: S);

    fn span_note<S: fmt::String>(&self, span: Span, err: S) {
        self.span_msg(span, "note: ", term::color::BRIGHT_GREEN, err);
    }

    fn span_warning<S: fmt::String>(&self, span: Span, err: S) {
        self.span_msg(span, "warning: ", term::color::BRIGHT_YELLOW, err);
    }

    /// Report an error that will stop compilation.
    fn span_error<S: fmt::String>(&self, span: Span, err: S) {
        self.span_msg(span, "error: ", term::color::BRIGHT_RED, err);

        ERRORS.with(|v| v.set(v.get()+1));
    }

    /// Report `err` and exit immediately.
    fn span_fatal<S: fmt::String>(&self, span: Span, err: S) -> ! {
        self.span_error(span, err);
        fatal()
    }
}

/// Exit in a controlled manner.
pub fn fatal() -> ! {
    panic!(FatalError)
}

/// The number of errors reported.
thread_local!(pub static ERRORS: Cell<u32> = Cell::new(0));

impl ErrorReporter for Context {
    fn span_msg<S: fmt::String>(&self, span: Span, err_type: &str, colour: term::color::Color, err: S) {
        let file = self.file(span.file);
        let lo = file.row_col(span.lo as usize);
        let hi = file.row_col(span.hi as usize);
        let path = file.path.as_str().unwrap();
        let mut stderr = term::stderr().unwrap();
        stderr.reset().unwrap();
        write!(&mut stderr, "{}:{}:{}: ", path, lo.0+1, lo.1+1).unwrap();
        if span.lo + 1 < span.hi {
            // `hi.1`, not `hi.1+1`, since the range is half-open but humans
            // expect closed intervals
            write!(&mut stderr, "{}:{}: ", hi.0+1, hi.1).unwrap();
        }
        stderr.fg(colour).unwrap();
        write!(&mut stderr, "{}", err_type).unwrap();
        stderr.reset().unwrap();
        stderr.attr(term::attr::Bold).unwrap();
        writeln!(&mut stderr, "{}", err).unwrap();
        stderr.reset().unwrap();
        if lo.0 == hi.0 {
            // one line error; print the contents of the file
            let line = file.line(lo.0);
            let prefix = format!("{}:{} ", path, lo.0+1);
            writeln!(&mut stderr, "{}{}", prefix, line);
            // for the second, just print spaces
            let prefix_spaces: String = iter::repeat(' ').take(prefix.chars().count()).collect();
            // ... and a nice indicator
            let line_indicator: String = line.chars().enumerate().map(|(i, c)|
                if c == '\t' {
                    // XXX: If a tab occurs in the middle of a span, this puts a gap in it
                    '\t'
                } else if i == lo.1 {
                    '^'
                } else if i >= lo.1 && i < hi.1 {
                    '~'
                } else {
                    ' '
                }).collect();
            stderr.fg(term::color::GREEN).unwrap();
            writeln!(&mut stderr, "{}{}", prefix_spaces, line_indicator).unwrap();
            stderr.reset().unwrap();
        }
    }
}

// These functions work on the global context.
pub fn span_note<S: fmt::String>(span: Span, err: S) {
    CONTEXT.with(move |ctx| ctx.borrow().span_note(span, err));
}
pub fn span_warning<S: fmt::String>(span: Span, err: S) {
    CONTEXT.with(move |ctx| ctx.borrow().span_warning(span, err));
}
pub fn span_error<S: fmt::String>(span: Span, err: S) {
    CONTEXT.with(move |ctx| ctx.borrow().span_error(span, err));
}
pub fn span_fatal<S: fmt::String>(span: Span, err: S) -> ! {
    CONTEXT.with(move |ctx| ctx.borrow().span_fatal(span, err));
    // rustc doesn't understand that the above must panic
    fatal()
}

#[macro_export]
macro_rules! span_note {
    ($span: expr, $($args: expr),+) => (::error::span_note($span, format!($($args),+)));
}
#[macro_export]
macro_rules! span_warning {
    ($span: expr, $($args: expr),+) => (::error::span_warning($span, format!($($args),+)));
}
#[macro_export]
macro_rules! span_error {
    ($span: expr, $($args: expr),+) => (::error::span_error($span, format!($($args),+)));
}
#[macro_export]
macro_rules! span_fatal {
    ($span: expr, $($args: expr),+) => (::error::span_fatal($span, format!($($args),+)));
}
