use ast::*;
use span::Span;
use std::collections::HashSet;

// Make sure that we only have allowed modifiers among the modifiers,
// and that each modifier appears at most once.
// FIXME: I pass the span of the ident of the modified object to this fn,
// but ideally something better would be used
pub fn ensure_valid_modifiers(allowed_modifiers: &HashSet<Modifier_>,
        modifiers: &Vec<Modifier>, span: Span, modifier_target: &str) -> HashSet<Modifier_> {
    let mut modifier_set = HashSet::new();
    for modifier in modifiers.iter() {
        if modifier_set.contains(&modifier.node) {
            span_error!(modifier.span, "modifier `{:?}` occurs more than once for {}",
                        modifier.node, modifier_target);
        }
        if !allowed_modifiers.contains(&modifier.node) {
            span_error!(modifier.span, "`{:?}` is not a valid modifier for {}",
                        modifier.node, modifier_target);
        }

        modifier_set.insert(modifier.node);
    }

    // This should universally hold for anything that needs modifiers.
    if modifier_set.contains(&Modifier_::Public) && modifier_set.contains(&Modifier_::Protected) {
        span_error!(span, "{} cannot be both `public` and `protected`",
                    modifier_target);
    }
    if !modifier_set.contains(&Modifier_::Public) && !modifier_set.contains(&Modifier_::Protected) {
        span_error!(span, "access modifier required for {} (private not supported)",
                    modifier_target);
    }

    return modifier_set;
}

pub fn weed_expression(expression: &Expression) {
    match expression.node {
        // ($3.10.1) Special rule for integers that are operands of unary minus.
        // Yup, all this work just to weed that one edge case.

        Expression_::Literal(Literal::Integer(i)) => {
            if i > 2147483647 {
                span_error!(expression.span, "integer {} exceeds maximum value of integer literal", i);
            } else if i < -2147483648 {
                span_error!(expression.span, "integer {} exceeds minimum value of integer literal", i);
            }
        },

        Expression_::Literal(_) => {},
        Expression_::This => {},
        Expression_::QualifiedThis(_) => {},

        Expression_::NewDynamicClass(box ref expr, _, ref exprs)
        | Expression_::MethodInvocation(Some(box ref expr), _, ref exprs) => {
            weed_expression(expr);
            weed_expressions(exprs);
        },

        Expression_::NewStaticClass(_, ref exprs)
        | Expression_::MethodInvocation(_, _, ref exprs) => {
            weed_expressions(exprs);
        },

        Expression_::NewArray(_, box ref expr)
        | Expression_::FieldAccess(box ref expr, _)
        | Expression_::Prefix(_, box ref expr)
        | Expression_::Cast(_, box ref expr) => {
            weed_expression(expr);
        },

        Expression_::ArrayAccess(box ref expr1, box ref expr2)
        | Expression_::Assignment(box ref expr1, box ref expr2)
        | Expression_::Infix(_, box ref expr1, box ref expr2) => {
            weed_expression(expr1);
            weed_expression(expr2);
        }

        Expression_::Name(_) => {},

        Expression_::InstanceOf(box ref expr, _) => {
            weed_expression(expr);
        },
    }
}

pub fn weed_expressions(expressions: &Vec<Expression>) {
    for expr in expressions.iter() {
        weed_expression(expr);
    }
}

pub fn weed_opt_expression(expression: &Option<Expression>) {
    if let &Some(ref expr) = expression {
        weed_expression(expr);
    }
}

pub fn weed_statement(statement: &Statement) {
    match statement.node {
        Statement_::Expression(ref expr) => weed_expression(expr),
        Statement_::If(ref expr, box ref stmt1, None) => {
            weed_expression(expr);
            weed_statement(stmt1);
        },
        Statement_::If(ref expr, box ref stmt1, Some(box ref stmt2)) => {
            weed_expression(expr);
            weed_statement(stmt1);
            weed_statement(stmt2);
        },
        Statement_::While(ref expr, box ref stmt) => {
            weed_expression(expr);
            weed_statement(stmt);
        },
        Statement_::For(ref expr1, ref expr2, ref expr3, box ref stmt) => {
            weed_opt_expression(expr1);
            weed_opt_expression(expr2);
            weed_opt_expression(expr3);
            weed_statement(stmt);
        }
        Statement_::ForDecl(_, ref expr1, ref expr2, box ref stmt) => {
            weed_opt_expression(expr1);
            weed_opt_expression(expr2);
            weed_statement(stmt);
        }
        Statement_::Return(ref expr) => weed_expression(expr),
        Statement_::Block(ref block) => weed_block(block),
        Statement_::Empty => {}
    }
}

pub fn weed_block(block: &Block) {
    for stmt in block.node.stmts.iter() {
        match stmt.node {
            BlockStatement_::Statement(ref statement) =>
                weed_statement(statement),
            BlockStatement_::LocalVariable(ref local) =>
                weed_expression(&local.node.initializer),
            BlockStatement_::LocalClass(_) => {},
        }
    }
}

pub fn weed_class_field(field: &Field) {
    // Assignment specs: "No field can be final."
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Static]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &field.node.modifiers,
                           field.node.name.span,
                           format!("field `{}`", field.node.name.node).as_slice());

    if let Some(ref expr) = field.node.initializer {
        weed_expression(expr);
    }
}

pub fn weed_class_method(method: &Method) {
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract,
             Modifier_::Static, Modifier_::Final, Modifier_::Native]
        .into_iter().collect();

    let modifiers = ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                                           method.node.name.span,
                                           format!("method `{}`", method.node.name).as_slice());

    // Assignment specs: "An abstract method cannot be static or final"
    if modifiers.contains(&Modifier_::Abstract) {
        if modifiers.contains(&Modifier_::Static) {
            span_error!(method.node.name.span, "abstract method `{}` cannot also be static", method.node.name);
        }
        if modifiers.contains(&Modifier_::Final) {
            span_error!(method.node.name.span, "abstract method `{}` cannot also be final", method.node.name);
        }
    }

    // Assignment specs: "A static method cannot be final."
    if modifiers.contains(&Modifier_::Static) &&
       modifiers.contains(&Modifier_::Final) {
        span_error!(method.node.name.span, "static method `{}` cannot also be final", method.node.name);
    }

    // Assignment specs: "A native method must be static."
    if modifiers.contains(&Modifier_::Native) &&
       !modifiers.contains(&Modifier_::Static) {
        span_error!(method.node.name.span, "native method `{}` must also be static", method.node.name);
    }

    // Assignment specs: "A method has a body if and only if it is neither
    // abstract nor native".
    let abstract_or_native = modifiers.contains(&Modifier_::Abstract) ||
                             modifiers.contains(&Modifier_::Native);
    if let Some(ref body) = method.node.body {
        if abstract_or_native {
            span_error!(method.node.name.span, "abstract/native method `{}` should not have a body", method.node.name);
        } else {
            weed_block(body);
        }
    } else if !abstract_or_native {
        span_error!(method.node.name.span, "non-abstract/native method `{}` should have a body", method.node.name);
    }
}

pub fn weed_interface_method(method: &Method) {
    // Assignment specs: "An interface method cannot be static, final or native.
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                           method.node.name.span,
                           format!("interface method `{}`", method.node.name).as_slice());

    // This should have been taken care of during parsing.
    assert!(method.node.body.is_none());
}

pub fn weed_class(class: &Class) {
    // ($8.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static, Modifier_::Final]
        .into_iter().collect();

    let modifiers = ensure_valid_modifiers(&allowed_modifiers, &class.node.modifiers,
                                           class.node.name.span,
                                           format!("class `{}`", class.node.name).as_slice());

    // Assignment specs: "A class cannot be both abstract and final."
    if modifiers.contains(&Modifier_::Abstract) &&
       modifiers.contains(&Modifier_::Final) {
        span_error!(class.node.name.span,
                    "class `{}` cannot be both abstract and final", class.node.name);
    }

    let mut has_constructor = false;
    for body_declaration in class.node.body.iter() {
        match body_declaration.node {
            ClassBodyDeclaration_::FieldDeclaration(ref field) =>
                weed_class_field(field),
            ClassBodyDeclaration_::MethodDeclaration(ref method) =>
                weed_class_method(method),
            ClassBodyDeclaration_::ConstructorDeclaration(ref constructor) => {
                if constructor.node.name == class.node.name {
                    has_constructor = true;
                    weed_block(&constructor.node.body);
                } else {
                    span_error!(constructor.node.name.span,
                                "`{}` not a valid constructor for class `{}`",
                                constructor.node.name, class.node.name);
                }
            },
        }
    }

    // Assignment specs: "Every class must contain at least one explicit constructor."
    if !has_constructor {
        span_error!(class.node.name.span,
                    "class `{}` has no explicit constructor", class.node.name);
    }
}

pub fn weed_interface(interface: &Interface) {
    // ($9.1.1) "The access modifiers protected and private pertain only to member
    // classes within a directly enclosing class declaration"
    let allowed_modifiers: HashSet<Modifier_> =
        vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static]
        .into_iter().collect();

    ensure_valid_modifiers(&allowed_modifiers, &interface.node.modifiers,
                           interface.node.name.span,
                           format!("interface `{}`", interface.node.name).as_slice());

    for method in interface.node.body.iter() {
        weed_interface_method(method);
    }
}

pub fn weed_filename(typename: &Ident, filestem: &str, kind: &str) {
    if typename.as_slice() != filestem {
        span_error!(typename.span, "{} name must match filename `{}`", kind, filestem);
    }
}

// Return whether an error was found.
pub fn weed(ast: &CompilationUnit, filestem: &str, file_span: Span) {
    if ast.types.len() > 1 {
        span_error!(file_span, "too many types (class/interface) in this file - Joos only supports 1");
    }

    for ty_decl in ast.types.iter() {
        match ty_decl.node {
            TypeDeclaration_::Class(ref class) => {
                weed_filename(&class.node.name, filestem, "class");
                weed_class(class);
            },
            TypeDeclaration_::Interface(ref interface) => {
                weed_filename(&interface.node.name, filestem, "interface");
                weed_interface(interface);
            },
        }
    }
}
