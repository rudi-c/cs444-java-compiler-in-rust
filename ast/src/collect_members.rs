use ast;
use walker::*;
use arena::Arena;
use name::*;

use middle::*;
use name_resolve::{Environment};
use lang_items::LangItems;

struct Collector<'env, 'a: 'env, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    env: &'env Environment<'a, 'ast>,
    type_definition: TypeDefinitionRef<'a, 'ast>,
}

fn insert_field<'a, 'ast>(tydef: TypeDefinitionRef<'a, 'ast>,
                          field_name: Symbol,
                          field: FieldRef<'a, 'ast>) {
    if let Some(old) = tydef.fields.borrow_mut().insert(field_name, field) {
        // something with the same name was already there!
        if old.origin == tydef {
            assert_eq!(field.origin, tydef);
            // duplicate definition
            span_error!(field.ast.span, "field `{}` already exists in `{}`",
                        field_name, tydef.fq_name);
        } else {
            // hides parent field, no problem.
        }
    }
}

fn insert_method<'a, 'ast>(tydef: TypeDefinitionRef<'a, 'ast>,
                           signature: MethodSignature<'a, 'ast>,
                           method: MethodRef<'a, 'ast>) {
    if let Some(old) = tydef.methods.borrow_mut().insert(signature.clone(), method) {
        // override
        if old.fq_name == method.fq_name {
            // this is okay
        } else if old.origin == tydef {
            assert_eq!(method.origin, tydef);
            // not really an override
            span_error!(method.ast.span, "method `{}` already defined in `{}`",
                        signature, tydef.fq_name);
        } else if old.ret_ty != method.ret_ty {
            // dOvs well-formedness constraint 6
            span_error!(method.ast.span, "overrided method `{}` has the wrong return type (expected `{}`, found `{}`)",
                        signature,
                        old.ret_ty, method.ret_ty);
        } else if method.has_modifier(ast::Modifier_::Static) != old.has_modifier(ast::Modifier_::Static) {
            // dOvs well-formedness constraint 5
            // XXX: deduplicate code
            span_error!(method.ast.span,
                        "{} method `{}` in `{}` cannot override {} method in `{}`",
                        if method.has_modifier(ast::Modifier_::Static) { "static" } else { "instance" },
                        signature,
                        method.origin.fq_name,
                        if old.has_modifier(ast::Modifier_::Static) { "static" } else { "instance" },
                        old.origin.fq_name);
        } else if method.has_modifier(ast::Modifier_::Protected) &&
            old.has_modifier(ast::Modifier_::Public) {
            // dOvs well-formedness constraint 7
            span_error!(method.ast.span,
                        "protected method `{}` in `{}` cannot override public method in `{}`",
                        signature,
                        method.origin.fq_name,
                        old.origin.fq_name);
        } else if old.has_modifier(ast::Modifier_::Final) {
            // dOvs well-formedness constraint 9
            span_error!(method.ast.span,
                        "method `{}` in `{}` cannot override final method in `{}`",
                        signature,
                        method.origin.fq_name,
                        old.origin.fq_name);
        }
    }
}

impl<'env, 'a, 'ast> Walker<'ast> for Collector<'env, 'a, 'ast> {
    fn walk_class_field(&mut self, field: &'ast ast::Field) {
        let tydef = self.type_definition;
        let field_name = field.node.name.node;
        let fq_name = format!("{}.{}", tydef.fq_name, field_name);
        let ty = self.env.resolve_type(&field.node.ty);
        insert_field(tydef, field_name, self.arena.alloc(Field::new(fq_name, tydef, ty, field)));

        // FIXME: Add the field to env?

        // no need to walk deeper
    }
    fn walk_class_method(&mut self, method_ast: &'ast ast::Method) {
        // Note: Implementation is shared with interface methods

        let tydef = self.type_definition;
        let method_name = method_ast.node.name.node;
        let signature = MethodSignature {
            name: method_name,
            args: method_ast.node.params.iter().map(|arg| {
                self.env.resolve_type(&arg.node.ty)
            }).collect(),
        };
        let ret_ty = method_ast.node.return_type.as_ref()
            .map(|ty| self.env.resolve_type(ty));
        let fq_name = format!("{}.{}", tydef.fq_name, signature);

        let impled = match tydef.kind {
            TypeKind::Class => if method_ast.node.has_modifier(ast::Modifier_::Abstract) {
                Abstract
            } else {
                Concrete
            },
            TypeKind::Interface => Abstract,
        };

        insert_method(tydef, signature, self.arena.alloc(
                Method::new(fq_name, tydef, ret_ty, impled, method_ast)));

        // no need to walk deeper
    }
    fn walk_interface_method(&mut self, method: &'ast ast::Method) {
        // same as a class method
        self.walk_class_method(method);
    }
    fn walk_constructor(&mut self, ast: &'ast ast::Constructor) {
        let tydef = self.type_definition;
        let args: Vec<_> = ast.params.iter().map(|arg| self.env.resolve_type(&arg.ty)).collect();
        // XXX
        let fq_name = format!("{}.{}", tydef.fq_name, MethodSignature { name: Symbol::from_str("<ctor>"), args: args.clone() });
        let ctor = self.arena.alloc(Constructor::new(fq_name, ast));
        if let Some(old) = tydef.constructors.borrow_mut().insert(args.clone(), ctor) {
            span_error!(ast.span, "constructor is already defined in `{}`", tydef.fq_name);
            span_note!(old.ast.span, "the old definition is here");
        }
    }
}

pub fn collect_members<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                 env: &Environment<'a, 'ast>,
                                 tydef: TypeDefinitionRef<'a, 'ast>,
                                 lang_items: &LangItems<'a, 'ast>) {
    // Bring in parent members
    for &parent in Some(&lang_items.object).into_iter()
        .chain(tydef.implements.borrow().iter())
        .chain(tydef.extends.borrow().iter()) {
        for (signature, &method) in parent.methods.borrow().iter() {
            insert_method(tydef, signature.clone(), method);
        }
        for (&name, &field) in parent.fields.borrow().iter() {
            insert_field(tydef, name, field);
        }
    }
    Collector {
        arena: arena,
        env: env,
        type_definition: tydef,
    }.walk_type_declaration(tydef.ast);
    // TODO: This would be a good place to check that all methods have
    // been implemented (no method with no body that isn't abstract).
}

