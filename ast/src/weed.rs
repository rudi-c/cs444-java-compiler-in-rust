use ast::*;

// http://stackoverflow.com/questions/27588416/how-to-send-output-to-stderr
macro_rules! println_err(
    ($($arg:tt)*) => (
        match writeln!(&mut ::std::io::stdio::stderr(), $($arg)* ) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    )
);

// Return whether an error was found.
pub fn weed(ast: CompilationUnit) -> bool {
    if ast.types.len() > 1 {
        println_err!("Too many types (class/interface) in this file - Joos only supports 1.");
        return true;
    }

    return false;
}
