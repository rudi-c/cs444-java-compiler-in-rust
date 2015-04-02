use ast;
use ast::walker::*;
use arena::Arena;
use name::*;
use error::have_error;

use middle::*;
use name_resolve::{Environment, ToPopulate};
use lang_items::LangItems;

use std::collections::HashMap;
use std::mem;

struct Collector<'env, 'a: 'env, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    env: &'env Environment<'a, 'ast>,
    inherited_methods: HashMap<MethodSignature<'a, 'ast>, Vec<(TypeDefinitionRef<'a, 'ast>, MethodRef<'a, 'ast>)>>,
    type_definition: TypeDefinitionRef<'a, 'ast>,

    ordered_fields: Vec<Symbol>,
    fields: HashMap<Symbol, FieldRef<'a, 'ast>>,
    methods: HashMap<MethodSignature<'a, 'ast>, MethodRef<'a, 'ast>>,
    method_impls: Vec<MethodImplRef<'a, 'ast>>,
    constructors: HashMap<Arguments<'a, 'ast>, ConstructorRef<'a, 'ast>>,
    to_populate: Vec<ToPopulate<'a, 'ast>>,
}

impl<'env, 'a, 'ast> Collector<'env, 'a, 'ast> {
    fn insert_field(&mut self,
                    field_name: Symbol,
                    field: FieldRef<'a, 'ast>) {
        if let Some(old) = self.fields.insert(field_name, field) {
            // something with the same name was already there!
            if old.origin == self.type_definition {
                assert_eq!(field.origin, self.type_definition);
                // duplicate definition
                span_error!(field.ast.span, "field `{}` already exists in `{}`",
                            field_name, self.type_definition.fq_name);
            } else {
                // hides parent field, no problem.
            }
        } else {
            self.ordered_fields.push(field_name);
        }
    }

    // If `defined` is Some(Method { ... }), then it MUST have an attached AST node.
    fn create_method(&mut self,
                     signature: MethodSignature<'a, 'ast>,
                     inherited: &[(TypeDefinitionRef<'a, 'ast>, MethodRef<'a, 'ast>)],
                     defined: Option<MethodRef<'a, 'ast>>) {
        let tydef = self.type_definition;

        let inherited_final = inherited.iter().find(|&&(_, ref method)| method.is_final)
            .map(|&(origin, _)| origin);

        let method = if let Some(method) = defined {
            if let Some(old_origin) = inherited_final {
                // dOvs well-formedness constraint 9
                span_error!(method.ast.unwrap().span,
                            "method `{}` in `{}` cannot override final method in `{}`",
                            signature,
                            tydef.fq_name,
                            old_origin.fq_name);
            }
            method
        } else {
            // Synthesize a method.
            assert!(inherited.len() > 0);
            let concrete = {
                let mut it = inherited.iter()
                .filter_map(|&(origin, method)| {
                    match method.impled {
                        // don't inherit stuff from interfaces
                        Concrete(method_impl) if origin.kind == TypeKind::Class =>
                            Some((origin, method, method_impl)),
                        _ => None
                    }
                }).fuse();
                let r = it.next();
                if it.count() > 0 {
                    assert!(have_error());
                }
                r
            };
            let impled = if let Some((_, _, method_impl)) = concrete {
                Concrete(method_impl)
            } else {
                Abstract
            };
            let accessibility = if let Some(&(origin, method)) = inherited.iter()
                .find(|&&(_, method)| matches!(Public, method.accessibility)) {
                if let Some((concrete_origin, concrete_method, _)) = concrete {
                    if matches!(Protected(..), concrete_method.accessibility) {
                        span_error!(tydef.ast.span,
                                    "cannot implement public method `{}` of `{}` with protected method from `{}`",
                                    method.fq_name, origin.fq_name, concrete_origin.fq_name);
                    }
                }
                Public
            } else {
                inherited.last().unwrap().1.accessibility.clone()
            };
            self.arena.alloc(Method::new(inherited[0].1.fq_name.to_string(),
                                         inherited[0].1.ret_ty.clone(),
                                         impled,
                                         inherited_final.is_some(),
                                         inherited[0].1.is_static,
                                         accessibility,
                                         None))
        };

        let span = if let Some(ast) = method.ast { ast.span } else { tydef.ast.span };

        for &(parent_ty, parent_method) in inherited.iter() {
            // dOvs well-formedness constraint 6: check return types equal
            if parent_method.ret_ty != method.ret_ty {
                if let Some(ast) = method.ast {
                    span_error!(ast.span,
                                "overrided method `{}` has the wrong return type (expected `{}`, found `{}`)",
                                signature, parent_method.ret_ty, method.ret_ty);
                } else {
                    span_error!(tydef.ast.span,
                                "bad inheritance: incompatible return types for method `{}` (expected `{}`, found `{}`)",
                                signature, parent_method.ret_ty, method.ret_ty);
                    span_note!(tydef.ast.span,
                               "conflicting methods inherited from types `{}`, `{}`",
                               parent_ty.fq_name, inherited[0].0.fq_name);
                }
                break;
            }

            // dOvs well-formedness constraint 5: check static-ness equal
            if parent_method.is_static != method.is_static {
                span_error!(span,
                            "{} method `{}` in `{}` cannot override {} method in `{}`",
                            if method.is_static { "static" } else { "instance" },
                            signature,
                            tydef.fq_name,
                            if parent_method.is_static { "static" } else { "instance" },
                            parent_ty.fq_name);
            }

            // dOvs well-formedness constraint 7: check visibility
            if matches!(Public, parent_method.accessibility) && matches!(Protected(..), method.accessibility) {
                span_error!(span,
                            "protected method `{}` in `{}` cannot override public method in `{}`",
                            signature,
                            tydef.fq_name,
                            parent_ty.fq_name);
            }
        }

        if let Some(old) = self.methods.insert(signature.clone(), method) {
            span_error!(method.ast.unwrap().span,
                        "method `{}` already defined in `{}`",
                        signature,
                        tydef.fq_name);
            span_note!(old.ast.unwrap().span,
                       "the old definition is here");
        }
    }

    fn finalize(mut self) -> Vec<ToPopulate<'a, 'ast>> {
        for (signature, v) in mem::replace(&mut self.inherited_methods, HashMap::new()).into_iter() {
            self.create_method(signature, &*v, None);
        }

        self.type_definition.ordered_fields.set(self.ordered_fields);
        self.type_definition.fields.set(self.fields);
        self.type_definition.methods.set(self.methods);
        self.type_definition.method_impls.set(self.method_impls);
        self.type_definition.constructors.set(self.constructors);
        self.to_populate
    }
}

impl<'env, 'a, 'ast> Walker<'ast> for Collector<'env, 'a, 'ast> {
    fn walk_class_field(&mut self, field: &'ast ast::Field) {
        let tydef = self.type_definition;
        let field_name = field.node.name.node;
        let fq_name = format!("{}.{}", tydef.fq_name, field_name);
        let ty = self.env.resolve_type(&field.node.ty);
        let field = self.arena.alloc(Field::new(fq_name, tydef, ty, field));
        self.to_populate.push(ToPopulate::Field(field));
        self.insert_field(field_name, field);

        // no need to walk deeper
    }
    fn walk_method(&mut self, method_ast: &'ast ast::Method) {
        let tydef = self.type_definition;
        let method_name = method_ast.node.name.node;
        let args: Vec<_> = method_ast.node.params.iter().map(|arg| {
            self.env.resolve_type(&arg.node.ty)
        }).collect();

        let signature = MethodSignature {
            name: method_name,
            args: args.clone(),
        };
        let ret_ty = self.env.resolve_type(&method_ast.node.return_type);
        let fq_name = format!("{}.{}", tydef.fq_name, signature);
        let is_final = method_ast.node.has_modifier(ast::Modifier_::Final);
        let is_static = method_ast.node.has_modifier(ast::Modifier_::Static);
        let accessibility = if method_ast.node.has_modifier(ast::Modifier_::Protected) {
            Protected(tydef)
        } else {
            Public
        };

        let method_impl = self.arena.alloc(
            MethodImpl::new(format!("{}.{}", tydef.fq_name, method_name), tydef, args, ret_ty.clone(), is_static, method_ast));
        self.method_impls.push(method_impl);
        self.to_populate.push(ToPopulate::Method(method_impl));
        let impled = match tydef.kind {
            TypeKind::Class => if method_ast.node.has_modifier(ast::Modifier_::Abstract) {
                // Still create a MethodImpl, just for the parameters.
                Abstract
            } else {
                Concrete(method_impl)
            },
            TypeKind::Interface => Abstract,
        };

        let method = self.arena.alloc(
            Method::new(fq_name, ret_ty, impled, is_final, is_static, accessibility, Some(method_ast)));

        let inherited = self.inherited_methods.remove(&signature).unwrap_or_default();
        self.create_method(signature, &*inherited, Some(method));

        // no need to walk deeper
    }
    fn walk_constructor(&mut self, ast: &'ast ast::Constructor) {
        let tydef = self.type_definition;
        let args: Vec<_> = ast.params.iter().map(|arg| self.env.resolve_type(&arg.ty)).collect();
        // XXX
        let fq_name = format!("{}.{}", tydef.fq_name, MethodSignature { name: Symbol::from_str("<ctor>"), args: args.clone() });
        let ctor = self.arena.alloc(Constructor::new(fq_name, args.clone(), ast));
        self.to_populate.push(ToPopulate::Constructor(ctor));
        if let Some(old) = self.constructors.insert(args.clone(), ctor) {
            span_error!(ast.span, "constructor is already defined in `{}`", tydef.fq_name);
            span_note!(old.ast.span, "the old definition is here");
        }
    }
}

pub fn collect_members<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                 env: &Environment<'a, 'ast>,
                                 tydef: TypeDefinitionRef<'a, 'ast>,
                                 lang_items: &LangItems<'a, 'ast>)
-> Vec<ToPopulate<'a, 'ast>> {
    let mut collector = Collector {
        arena: arena,
        env: env,
        inherited_methods: HashMap::new(),
        type_definition: tydef,
        ordered_fields: vec![],
        fields: HashMap::new(),
        methods: HashMap::new(),
        method_impls: vec![],
        constructors: HashMap::new(),
        to_populate: vec![],
    };
    // Bring in parent members
    for &parent in {
        // Inherit from `Object`, unless we explicitly extend some class (from which we indirectly
        // inherit from Object)...
        if (tydef.kind == TypeKind::Interface || tydef.extends.len() == 0)
        // as long as this isn't `Object` itself.
            && (tydef != lang_items.object) {
            Some(&lang_items.object)
        } else {
            None
        }
    }.into_iter()
    .chain(tydef.implements.iter())
    .chain(tydef.extends.iter()) {
        for (signature, &method) in parent.methods.iter() {
            collector.inherited_methods.entry(signature.clone()).get()
                .unwrap_or_else(|v| v.insert(vec![]))
                .push((tydef, method));
        }
        for (&name, &field) in parent.fields.iter() {
            collector.insert_field(name, field);
        }
    }
    collector.walk_type_declaration(tydef.ast);
    collector.finalize()
    // TODO: This would be a good place to check that all methods have
    // been implemented (no method with no body that isn't abstract).
}

