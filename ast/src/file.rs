use std::fs;
use std::io;
use std::io::Read;
use std::path::PathBuf;

pub struct File {
    pub path: PathBuf,
    pub contents: String,
    pub lines: Vec<usize>,
}

impl File {
    pub fn new(path: PathBuf) -> io::Result<File> {
        let mut contents = String::new();
        try!(try!(fs::File::open(&path)).read_to_string(&mut contents));
        let mut lines = vec![0];
        lines.extend(contents.match_indices("\n").map(|(_, ix)| ix));
        Ok(File {
            path: path,
            contents: contents,
            lines: lines,
        })
    }

    /// Returns the stem of the file's name.
    pub fn stem(&self) -> &str {
        self.path.file_stem().expect("file must have extension").to_str().unwrap()
    }

    /// Returns the row and column of the provided character.
    /// Both index from zero.
    pub fn row_col(&self, ix: usize) -> (usize, usize) {
        let mut lo = 0;
        let mut hi = self.lines.len();
        while lo + 1 < hi {
            let mid = (lo + hi) / 2;
            if self.lines[mid] > ix {
                hi = mid;
            } else {
                lo = mid;
            }
        }
        (lo, self.contents[self.lines[lo]..ix].chars().count())
    }

    /// Returns the given line of the input.
    pub fn line(&self, ix: usize) -> &str {
        let lo = self.lines[ix];
        let hi = match self.lines.get(ix+1) {
            Some(&x) => x-1,
            None => self.contents.len(),
        };
        &self.contents[lo..hi]
    }
}
