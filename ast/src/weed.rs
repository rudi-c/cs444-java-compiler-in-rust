use ast::*;
use name::Ident;
use span::Span;
use std::collections::HashSet;
use walker::*;

struct Weeder<'a, 'b> {
    filestem: &'a str,
    has_constructor: bool,
    current_class: Option<&'b Class>,
}

// Make sure that we only have allowed modifiers among the modifiers,
// and that each modifier appears at most once.
// FIXME: I pass the span of the ident of the modified object to this fn,
// but ideally something better would be used
fn ensure_valid_modifiers(allowed_modifiers: &HashSet<Modifier_>,
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

impl<'a, 'b> Walker<'b> for Weeder<'a, 'b> {
    fn walk_expression(&mut self, expression: &'b Expression) {
        match expression.node {
            // ($3.10.1) Special rule for integers that are operands of unary minus.

            Expression_::Literal(Literal::Integer(i)) => {
                if i > 2147483647 {
                    span_error!(expression.span,
                                "integer {} exceeds maximum value of integer literal", i);
                } else if i < -2147483648 {
                    span_error!(expression.span,
                                "integer {} exceeds minimum value of integer literal", i);
                }
            },
            _ => ()
        }
        default_walk_expression(self, expression);
    }

    fn walk_class_field(&mut self, field: &'b Field) {
        // Assignment specs: "No field can be final."
        let allowed_modifiers: HashSet<Modifier_> =
            vec![Modifier_::Public, Modifier_::Protected, Modifier_::Static]
            .into_iter().collect();

        ensure_valid_modifiers(&allowed_modifiers, &field.node.modifiers,
                               field.node.name.span,
                               &format!("field `{}`", field.node.name.node));

        default_walk_class_field(self, field);
    }

    fn walk_class_method(&mut self, method: &'b Method) {
        let allowed_modifiers: HashSet<Modifier_> =
            vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract,
                 Modifier_::Static, Modifier_::Final, Modifier_::Native]
            .into_iter().collect();

        let modifiers = ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                                               method.node.name.span,
                                               &format!("method `{}`", method.node.name));

        // Assignment specs: "An abstract method cannot be static or final"
        if modifiers.contains(&Modifier_::Abstract) {
            if modifiers.contains(&Modifier_::Static) {
                span_error!(method.node.name.span,
                            "abstract method `{}` cannot also be static",
                            method.node.name);
            }
            if modifiers.contains(&Modifier_::Final) {
                span_error!(method.node.name.span,
                            "abstract method `{}` cannot also be final",
                            method.node.name);
            }
        }

        // Assignment specs: "A static method cannot be final."
        if modifiers.contains(&Modifier_::Static) &&
           modifiers.contains(&Modifier_::Final) {
            span_error!(method.node.name.span,
                        "static method `{}` cannot also be final",
                        method.node.name);
        }

        // Assignment specs: "A native method must be static."
        if modifiers.contains(&Modifier_::Native) &&
           !modifiers.contains(&Modifier_::Static) {
            span_error!(method.node.name.span,
                        "native method `{}` must also be static",
                        method.node.name);
        }

        // Assignment specs: "A method has a body if and only if it is neither
        // abstract nor native".
        let abstract_or_native = modifiers.contains(&Modifier_::Abstract) ||
                                 modifiers.contains(&Modifier_::Native);
        if method.node.body.is_some() {
            if abstract_or_native {
                span_error!(method.node.name.span,
                            "abstract/native method `{}` should not have a body",
                            method.node.name);
            }
        } else if !abstract_or_native {
            span_error!(method.node.name.span,
                        "non-abstract/native method `{}` should have a body",
                        method.node.name);
        }

        default_walk_class_method(self, method);
    }

    fn walk_interface_method(&mut self, method: &'b Method) {
        // Assignment specs: "An interface method cannot be static, final or native.
        let allowed_modifiers: HashSet<Modifier_> =
            vec![Modifier_::Public, Modifier_::Protected, Modifier_::Abstract]
            .into_iter().collect();

        ensure_valid_modifiers(&allowed_modifiers, &method.node.modifiers,
                               method.node.name.span,
                               &format!("interface method `{}`", method.node.name));

        // This should have been taken care of during parsing.
        assert!(method.node.body.is_none());

        default_walk_interface_method(self, method);
    }

    fn walk_class(&mut self, class: &'b Class) {
        // No nested classes.
        assert!(self.current_class.is_none());
        self.current_class = Some(class);

        weed_filename(&class.node.name, self.filestem, "class");

        // ($8.1.1) "The access modifiers protected and private pertain only to member
        // classes within a directly enclosing class declaration"
        let allowed_modifiers: HashSet<Modifier_> =
            vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static, Modifier_::Final]
            .into_iter().collect();

        let modifiers = ensure_valid_modifiers(&allowed_modifiers, &class.node.modifiers,
                                               class.node.name.span,
                                               &format!("class `{}`", class.node.name));

        // Assignment specs: "A class cannot be both abstract and final."
        if modifiers.contains(&Modifier_::Abstract) &&
           modifiers.contains(&Modifier_::Final) {
            span_error!(class.node.name.span,
                        "class `{}` cannot be both abstract and final", class.node.name);
        }

        // The walker for a constructor sets `self.has_constructor` to true
        default_walk_class(self, class);

        // Assignment specs: "Every class must contain at least one explicit constructor."
        if !self.has_constructor {
            span_error!(class.node.name.span,
                        "class `{}` has no explicit constructor", class.node.name);
        }
        self.has_constructor = false;
        self.current_class = None;
    }

    fn walk_constructor(&mut self, ctor: &'b Constructor) {
        if let Some(class) = self.current_class {
            if class.node.name == ctor.node.name {
                self.has_constructor = true;
            } else {
                span_error!(ctor.node.name.span,
                            "`{}` not a valid constructor for class `{}`",
                            ctor.node.name, class.node.name);
            }
        } else {
            panic!("constructor found outside of a class?");
        }

        default_walk_constructor(self, ctor);
    }

    fn walk_interface(&mut self, interface: &'b Interface) {
        weed_filename(&interface.node.name, self.filestem, "interface");

        // ($9.1.1) "The access modifiers protected and private pertain only to member
        // classes within a directly enclosing class declaration"
        let allowed_modifiers: HashSet<Modifier_> =
            vec![Modifier_::Public, Modifier_::Abstract, Modifier_::Static]
            .into_iter().collect();

        ensure_valid_modifiers(&allowed_modifiers, &interface.node.modifiers,
                               interface.node.name.span,
                               &format!("interface `{}`", interface.node.name));

        default_walk_interface(self, interface);
    }
}

fn weed_filename(typename: &Ident, filestem: &str, kind: &str) {
    if typename.as_ref() != filestem {
        span_error!(typename.span, "{} name must match filename `{}`", kind, filestem);
    }
}

pub fn weed(ast: &CompilationUnit, filestem: &str, file_span: Span) {
    if ast.types.len() > 1 {
        span_error!(file_span, "too many types (class/interface) in this file - Joos only supports 1");
    }

    Weeder {
        filestem: filestem,
        has_constructor: false,
        current_class: None,
    }.walk_compilation_unit(ast);
}
