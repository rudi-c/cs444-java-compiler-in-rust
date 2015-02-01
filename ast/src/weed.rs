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

    // This should universally hold for anything that needs modifiers.
    if modifiers.contains(&Modifier::Public) && modifiers.contains(&Modifier::Protected) {
        println_err!("Both modifiers Public and Protected appear for {}.", modifier_target);
        error = true;
    }

    return error;
}

pub fn weed_block(block: &Block) -> bool {
    let mut error = false;

    return error;
}

pub fn weed_class_field(field: &Field) -> bool {
    let mut error = false;

    // Assignment specs: "No field can be final."
    let allowed_modifiers: HashSet<Modifier> =
        vec![Modifier::Public, Modifier::Protected, Modifier::Static]
        .into_iter().collect();

    error |= ensure_valid_modifiers(&allowed_modifiers, &field.modifiers,
                                    format!("field `{}`", field.name).as_slice());

    return error;
}

pub fn weed_class_method(method: &Method) -> bool {
    let mut error = false;

    let allowed_modifiers: HashSet<Modifier> =
        vec![Modifier::Public, Modifier::Protected, Modifier::Abstract,
             Modifier::Static, Modifier::Final, Modifier::Native]
        .into_iter().collect();

    error |= ensure_valid_modifiers(&allowed_modifiers, &method.modifiers,
                                    format!("method `{}`", method.name).as_slice());

    // Assignment specs: "An abstract method cannot be static or final"
    if method.modifiers.contains(&Modifier::Abstract) {
        if method.modifiers.contains(&Modifier::Static) ||
           method.modifiers.contains(&Modifier::Final) {
            println_err!("Abstract method `{}` cannot also be static/final.", method.name);
            error = true;
        }
    }

    // Assignment specs: "A static method cannot be final."
    if method.modifiers.contains(&Modifier::Static) &&
       method.modifiers.contains(&Modifier::Final) {
        println_err!("Static method `{}` cannot also be final.", method.name);
        error = true;
    }

    // Assignment specs: "A native method must be static."
    if method.modifiers.contains(&Modifier::Native) &&
       !method.modifiers.contains(&Modifier::Static) {
        println_err!("Native method `{}` must also be static.", method.name);
        error = true;
    }

    // Methods must have an access modifier. I'm not sure where this is specified,
    // but the test case Je_1_Methods_MissingAccessModifier.java seems to require it.
    if !(method.modifiers.contains(&Modifier::Public) ^
         method.modifiers.contains(&Modifier::Protected)) {
        println_err!("Method `{}` must have exactly one access modifier.", method.name);
        error = true;
    }

    // Assignment specs: "A method has a body if and only if it is neither
    // abstract nor native".
    let abstract_or_native = method.modifiers.contains(&Modifier::Abstract) ||
                             method.modifiers.contains(&Modifier::Native);
    if let Some(ref body) = method.body {
        if abstract_or_native {
            println_err!("Abstract/native method `{}` should not have a body.", method.name);
            error = true;
        } else {
            error |= weed_block(body);
        }
    } else if !abstract_or_native {
        println_err!("Non-abstract/native method `{}` should have a body.", method.name);
        error = true;
    }

    return error;
}

pub fn weed_interface_method(method: &Method) -> bool {
    let mut error = false;

    // Assignment specs: "An interface method cannot be static, final or native.
    let allowed_modifiers: HashSet<Modifier> =
        vec![Modifier::Public, Modifier::Protected, Modifier::Abstract]
        .into_iter().collect();

    error |= ensure_valid_modifiers(&allowed_modifiers, &method.modifiers,
                                    format!("interface method `{}`", method.name).as_slice());

    // This should have been taken care of during parsing.
    assert!(method.body.is_none());

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

    // ($9.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier> =
        vec![Modifier::Public, Modifier::Abstract, Modifier::Static]
        .into_iter().collect();

    error |= ensure_valid_modifiers(&allowed_modifiers, &interface.modifiers,
                                    format!("interface `{}`", interface.name).as_slice());

    for method in interface.body.iter() {
        error |= weed_interface_method(method);
    }

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
