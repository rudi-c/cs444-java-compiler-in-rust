#![macro_use]

use ast;

use file::File;
use std::cell::RefCell;
use std::io::IoResult;

pub struct Context {
    // All the files being compiled.
    // Indices in this vector are stable.
    files: Vec<File>,
    pub verbose: bool,
}

impl Context {
    pub fn new() -> Context {
        Context {
            files: vec![],
            verbose: false,
        }
    }

    pub fn add_file<'a>(&'a mut self, path: Path) -> IoResult<usize> {
        let ix = self.files.len();
        self.files.push(try!(File::new(path)));
        Ok(ix)
    }

    pub fn file<'a>(&'a self, i: usize) -> &'a File {
        &self.files[i]
    }
}

/// The global context.
thread_local!(pub static CONTEXT: RefCell<Context> = RefCell::new(Context::new()));
