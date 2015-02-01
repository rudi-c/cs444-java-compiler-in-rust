use ast::*;
use std::collections::HashSet;

// http://stackoverflow.com/questions/27588416/how-to-send-output-to-stderr
macro_rules! println_err(
    ($($arg:tt)*) => (
        match writeln!(&mut ::std::io::stdio::stderr(), $($arg)* ) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    )
);

// Make sure that we only have allowed modifiers among the modifiers,
// and that each modifier appears at most once.
pub fn ensure_valid_modifiers(allowed_modifiers: &HashSet<Modifier>,
        modifiers: &Vec<Modifier>, modifier_target: &str) -> bool {
    let mut error = false;

    let mut modifier_set = HashSet::new();
    for modifier in modifiers.iter() {
        if modifier_set.contains(modifier) {
            println_err!("Modifier `{:?}` occurs more than once for {}.",
                         modifier, modifier_target);
            error = true;
        }
        if !allowed_modifiers.contains(modifier) {
            println_err!("`{:?}` not a valid modifier for {}.",
                         modifier, modifier_target);
            error = true;
        }

        modifier_set.insert(modifier);
    }

    return error;
}

pub fn weed_class_field(field: &Field) -> bool {
    let mut error = false;

    return error;
}

pub fn weed_class_method(method: &Method) -> bool {
    let mut error = false;

    return error;
}

pub fn weed_class(class: &Class) -> bool {
    let mut error = false;

    // ($8.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier> =
        vec![Modifier::Public, Modifier::Abstract, Modifier::Static, Modifier::Final]
        .into_iter().collect();

    error |= ensure_valid_modifiers(&allowed_modifiers, &class.modifiers,
                                    format!("class `{}`", class.name).as_slice());

    // Assignment specs: "A class cannot be both abstract and final."
    if class.modifiers.contains(&Modifier::Abstract) &&
       class.modifiers.contains(&Modifier::Final) {
        println_err!("Class `{}` cannot be both Abstract and Final.", class.name);
        error = true;
    }

    let mut has_constructor = false;
    for body_declaration in class.body.iter() {
        match body_declaration {
            &ClassBodyDeclaration::FieldDeclaration(ref field) =>
                error |= weed_class_field(field),
            &ClassBodyDeclaration::MethodDeclaration(ref method) =>
                error |= weed_class_method(method),
            &ClassBodyDeclaration::ConstructorDeclaration(ref constructor) => {
                if constructor.name == class.name {
                    has_constructor = true;
                } else {
                    println_err!("`{}` not a valid constructor for class `{}`",
                                 constructor.name, class.name);
                    error = true;
                }
            },
        }
    }

    // Assignment specs: "Every class must contain at least one explicit constructor."
    if !has_constructor {
        println_err!("Class `{}` has no explicit constructor.", class.name);
        error = true;
    }

    return error;
}

pub fn weed_interface(interface: &Interface) -> bool {
    let mut error = false;

    return error;
}

// Return whether an error was found.
pub fn weed(ast: CompilationUnit) -> bool {
    let mut error = false;

    if ast.types.len() > 1 {
        println_err!("Too many types (class/interface) in this file - Joos only supports 1.");
        error = true;
    }

    if ast.types.len() == 1 {
        match ast.types[0] {
            TypeDeclaration::Class(ref class) => error |= weed_class(class),
            TypeDeclaration::Interface(ref interface) => error |= weed_interface(interface),
        }
    }

    return error;
}
