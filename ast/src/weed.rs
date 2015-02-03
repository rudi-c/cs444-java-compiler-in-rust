use ast::*;
use std::collections::HashSet;

// http://stackoverflow.com/questions/27588416/how-to-send-output-to-stderr
macro_rules! println_err(
    ($($arg:tt)*) => (
        match writeln!(&mut ::std::io::stdio::stderr(), "ERROR: {}",
                       format!($($arg)*) ) {
            Ok(_) => {},
            Err(x) => panic!("Unable to write to stderr: {}", x),
        }
    )
);

// Make sure that we only have allowed modifiers among the modifiers,
// and that each modifier appears at most once.
// FIXME(errors): Swap out the `error` parameter.
pub fn ensure_valid_modifiers(allowed_modifiers: &HashSet<Modifier_>,
        modifiers: &Vec<Modifier>, modifier_target: &str, error: &mut bool) -> HashSet<Modifier_> {
    let mut modifier_set = HashSet::new();
    for modifier in modifiers.iter() {
        if modifier_set.contains(&modifier.node) {
            println_err!("Modifier `{:?}` occurs more than once for {}.",
                         modifier.node, modifier_target);
            *error = true;
        }
        if !allowed_modifiers.contains(&modifier.node) {
            println_err!("`{:?}` not a valid modifier for {}.",
                         modifier.node, modifier_target);
            *error = true;
        }

        modifier_set.insert(modifier.node);
    }

    // This should universally hold for anything that needs modifiers.
    if modifier_set.contains(&Modifier_::Public) && modifier_set.contains(&Modifier_::Protected) {
        println_err!("Both modifiers Public and Protected appear for {}.", modifier_target);
        *error = true;
    }
    if !modifier_set.contains(&Modifier_::Public) && !modifier_set.contains(&Modifier_::Protected) {
        println_err!("Access modifier required for {} - package private not supported.",
                     modifier_target);
        *error = true;
    }

    return modifier_set;
}

pub fn weed_expression(expression: &Expression) -> bool {
    let mut error = false;

    match expression.node {
        // ($3.10.1) Special rule for integers that are operands of unary minus.
        // Yup, all this work just to weed that one edge case.

        Expression_::Literal(Literal::Integer(i)) => {
            if i > 2147483647 {
                println_err!("Integer {} exceeds maximum value of integer literal.", i);
                error = true;
            } else if i < -2147483648 {
                println_err!("Integer {} exceeds minimum value of integer literal.", i);
                error = true;
            }
        },

        Expression_::Literal(_) => {},
        Expression_::This => {},
        Expression_::QualifiedThis(_) => {},

        Expression_::NewDynamicClass(box ref expr, _, ref exprs)
        | Expression_::MethodInvocation(Some(box ref expr), _, ref exprs) => {
            error |= weed_expression(expr);
            error |= weed_expressions(exprs);
        },

        Expression_::NewStaticClass(_, ref exprs)
        | Expression_::MethodInvocation(_, _, ref exprs) => {
            error |= weed_expressions(exprs);
        },

        Expression_::NewArray(_, box ref expr)
        | Expression_::FieldAccess(box ref expr, _)
        | Expression_::Prefix(_, box ref expr)
        | Expression_::Cast(_, box ref expr) => {
            error |= weed_expression(expr);
        },

        Expression_::ArrayAccess(box ref expr1, box ref expr2)
        | Expression_::Assignment(box ref expr1, box ref expr2)
        | Expression_::Infix(_, box ref expr1, box ref expr2) => {
            error |= weed_expression(expr1);
            error |= weed_expression(expr2);
        }

        Expression_::Name(_) => {},

        Expression_::InstanceOf(box ref expr, _) => {
            error |= weed_expression(expr);
        },
    }

    return error;
}

pub fn weed_expressions(expressions: &Vec<Expression>) -> bool {
    let mut error = false;

    for expr in expressions.iter() {
        error |= weed_expression(expr);
    }

    return error;
}

pub fn weed_opt_expression(expression: &Option<Expression>) -> bool {
    if let &Some(ref expr) = expression {
        return weed_expression(expr);
    } else {
        return false;
    }
}


pub fn weed_statement(statement: &Statement) -> bool {
    let mut error = false;

    match statement.node {
        Statement_::Expression(ref expr) => error |= weed_expression(expr),
        Statement_::If(ref expr, box ref stmt1, None) => {
            error |= weed_expression(expr);
            error |= weed_statement(stmt1);
        },
        Statement_::If(ref expr, box ref stmt1, Some(box ref stmt2)) => {
            error |= weed_expression(expr);
            error |= weed_statement(stmt1);
            error |= weed_statement(stmt2);
        },
        Statement_::While(ref expr, box ref stmt) => {
            error |= weed_expression(expr);
            error |= weed_statement(stmt);
        },
        Statement_::For(ref expr1, ref expr2, ref expr3, box ref stmt) => {
            error |= weed_opt_expression(expr1);
            error |= weed_opt_expression(expr2);
            error |= weed_opt_expression(expr3);
            error |= weed_statement(stmt);
        }
        Statement_::ForDecl(_, ref expr1, ref expr2, box ref stmt) => {
            error |= weed_opt_expression(expr1);
            error |= weed_opt_expression(expr2);
            error |= weed_statement(stmt);
        }
        Statement_::Return(ref expr) => error |= weed_expression(expr),
        Statement_::Block(ref block) => error |= weed_block(block),
        Statement_::Empty => {}
    }

    return error;
}

pub fn weed_block(block: &Block) -> bool {
    let mut error = false;

    for stmt in block.node.stmts.iter() {
        match stmt.node {
            BlockStatement_::Statement(ref statement) =>
                error |= weed_statement(statement),
            BlockStatement_::LocalVariable(ref local) =>
                error |= weed_expression(&local.node.initializer),
            BlockStatement_::LocalClass(_) => {},
        }
    }

    return error;
}

pub fn weed_class_field(field: &Field) -> bool {
    let mut error = false;

    // Assignment specs: "No field can be final."
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Static]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &field.node.modifiers,
                           format!("field `{}`", field.node.name.node).as_slice(),
                           &mut error);

    if let Some(ref expr) = field.node.initializer {
        error |= weed_expression(expr);
    }

    return error;
}

pub fn weed_class_method(method: &Method) -> bool {
    let mut error = false;

    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract,
             Modifier_::Static, Modifier_::Final, Modifier_::Native]
        .into_iter().collect();

    let modifiers = ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                                           format!("method `{}`", method.node.name).as_slice(),
                                           &mut error);

    // Assignment specs: "An abstract method cannot be static or final"
    if modifiers.contains(&Modifier_::Abstract) {
        if modifiers.contains(&Modifier_::Static) ||
           modifiers.contains(&Modifier_::Final) {
            println_err!("Abstract method `{}` cannot also be static/final.", method.node.name);
            error = true;
        }
    }

    // Assignment specs: "A static method cannot be final."
    if modifiers.contains(&Modifier_::Static) &&
       modifiers.contains(&Modifier_::Final) {
        println_err!("Static method `{}` cannot also be final.", method.node.name);
        error = true;
    }

    // Assignment specs: "A native method must be static."
    if modifiers.contains(&Modifier_::Native) &&
       !modifiers.contains(&Modifier_::Static) {
        println_err!("Native method `{}` must also be static.", method.node.name);
        error = true;
    }

    // Assignment specs: "A method has a body if and only if it is neither
    // abstract nor native".
    let abstract_or_native = modifiers.contains(&Modifier_::Abstract) ||
                             modifiers.contains(&Modifier_::Native);
    if let Some(ref body) = method.node.body {
        if abstract_or_native {
            println_err!("Abstract/native method `{}` should not have a body.", method.node.name);
            error = true;
        } else {
            error |= weed_block(body);
        }
    } else if !abstract_or_native {
        println_err!("Non-abstract/native method `{}` should have a body.", method.node.name);
        error = true;
    }

    return error;
}

pub fn weed_interface_method(method: &Method) -> bool {
    let mut error = false;

    // Assignment specs: "An interface method cannot be static, final or native.
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                           format!("interface method `{}`", method.node.name).as_slice(),
                           &mut error);

    // This should have been taken care of during parsing.
    assert!(method.node.body.is_none());

    return error;
}

pub fn weed_class(class: &Class) -> bool {
    let mut error = false;

    // ($8.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static, Modifier_::Final]
        .into_iter().collect();

    let modifiers = ensure_valid_modifiers(&allowed_modifiers, &class.node.modifiers,
                                           format!("class `{}`", class.node.name).as_slice(),
                                           &mut error);

    // Assignment specs: "A class cannot be both abstract and final."
    if modifiers.contains(&Modifier_::Abstract) &&
       modifiers.contains(&Modifier_::Final) {
        println_err!("Class `{}` cannot be both Abstract and Final.", class.node.name);
        error = true;
    }

    let mut has_constructor = false;
    for body_declaration in class.node.body.iter() {
        match body_declaration.node {
            ClassBodyDeclaration_::FieldDeclaration(ref field) =>
                error |= weed_class_field(field),
            ClassBodyDeclaration_::MethodDeclaration(ref method) =>
                error |= weed_class_method(method),
            ClassBodyDeclaration_::ConstructorDeclaration(ref constructor) => {
                if constructor.node.name == class.node.name {
                    has_constructor = true;
                    error |= weed_block(&constructor.node.body);
                } else {
                    println_err!("`{}` not a valid constructor for class `{}`",
                                 constructor.node.name, class.node.name);
                    error = true;
                }
            },
        }
    }

    // Assignment specs: "Every class must contain at least one explicit constructor."
    if !has_constructor {
        println_err!("Class `{}` has no explicit constructor.", class.node.name);
        error = true;
    }

    return error;
}

pub fn weed_interface(interface: &Interface) -> bool {
    let mut error = false;

    // ($9.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &interface.node.modifiers,
                           format!("interface `{}`", interface.node.name).as_slice(),
                           &mut error);

    for method in interface.node.body.iter() {
        error |= weed_interface_method(method);
    }

    return error;
}

pub fn weed_filename(typename: &str, filestem: &str) -> bool {
    if typename != filestem {
        println_err!("File {} must contain a class or interface with the same name.", filestem);
        return true;
    }
    return false;
}

// Return whether an error was found.
pub fn weed(ast: &CompilationUnit, filestem: &str) -> bool {
    let mut error = false;

    if ast.types.len() > 1 {
        println_err!("Too many types (class/interface) in this file - Joos only supports 1.");
        error = true;
    }

    if ast.types.len() == 1 {
        match ast.types[0].node {
            TypeDeclaration_::Class(ref class) => {
                error |= weed_filename(class.node.name.as_slice(), filestem);
                error |= weed_class(class);
            },
            TypeDeclaration_::Interface(ref interface) => {
                error |= weed_filename(interface.node.name.as_slice(), filestem);
                error |= weed_interface(interface);
            },
        }
    }

    return error;
}
