use ast;
use name::*;
use walker::*;
use arena::Arena;

use std::borrow::ToOwned;
use std::collections::hash_map;

use name_resolve_structs::*;

struct Collector<'a, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    package: PackageRef<'a, 'ast>,
    scope: Vec<Symbol>,
    type_definition: Option<TypeDefinitionRef<'a, 'ast>>,
    all_tydefs: Vec<TypeDefinitionRef<'a, 'ast>>,
}

impl<'a, 'ast> Walker<'ast> for Collector<'a, 'ast> {
    fn walk_class_field(&mut self, field: &'ast ast::Field) {
        let field_name = format!("{}.{}", Qualified(self.scope.iter()), field.node.name);
        let tydef = self.type_definition.as_ref().unwrap();
        if let Some(_) = tydef.fields.borrow_mut().insert(field.node.name.node, self.arena.alloc(Field::new(field_name, field))) {
            // something with the same name was already there!
            span_error!(field.span, "field `{}` already exists in `{}`",
                        field.node.name, Qualified(self.scope.iter()));
        }

        // no need to walk deeper
    }
    fn walk_class_method(&mut self, method_ast: &'ast ast::Method) {
        // Note: Implementation is shared with interface methods

        let tydef = self.type_definition.as_ref().unwrap();
        let mut methods = tydef.methods.borrow_mut();
        let overloads = methods.entry(method_ast.node.name.node).get().unwrap_or_else(|v| {
            // First time seeing this method name.
            v.insert(vec![])
        });

        let method_name = format!("{}.{}", Qualified(self.scope.iter()), method_ast.node.name);
        overloads.push(self.arena.alloc(Method::new(method_name, method_ast)));

        // no need to walk deeper
    }
    fn walk_interface_method(&mut self, method: &'ast ast::Method) {
        // same as a class method
        self.walk_class_method(method);
    }

    fn walk_type_declaration(&mut self, ty_decl: &'ast ast::TypeDeclaration) {
        assert!(self.type_definition.is_none());

        let name = ty_decl.name();
        let kind = match ty_decl.node {
            ast::TypeDeclaration_::Class(..) => TypeKind::Class,
            ast::TypeDeclaration_::Interface(..) => TypeKind::Interface,
        };
        self.scope.push(name.node);
        let fq_type = Qualified(self.scope.iter()).to_string();
        let tydef = self.arena.alloc(TypeDefinition::new(fq_type, kind, ty_decl));

        // Insert `tydef` into the package
        match self.package.contents.borrow_mut().entry(name.node) {
            hash_map::Entry::Occupied(v) => {
                match *v.get() {
                    PackageItem::Package(..) => {
                        type_package_conflict(&*tydef);
                    }
                    PackageItem::TypeDefinition(..) => {
                        span_error!(name.span,
                                    "type `{}` already exists in package `{}`",
                                    name, self.package.fq_name);
                    }
                }
                return
            }
            hash_map::Entry::Vacant(v) => {
                v.insert(PackageItem::TypeDefinition(tydef));
            }
        }

        self.type_definition = Some(tydef);
        self.all_tydefs.push(tydef);

        default_walk_type_declaration(self, ty_decl);

        self.type_definition = None;
        self.scope.pop();
    }
}

fn type_package_conflict(tydef: &TypeDefinition) {
    span_error!(tydef.ast.name().span,
                // Technically this is the name of the type, not the package,
                // but the point is that they're the same...
                "type name conflicts with package `{}`",
                tydef.fq_name);
}

// Looks up a package by qualified identifier, creating it if necessary.
// If a name conflict occurs, this will create a dummy package.
fn resolve_create_package<'a, 'ast>(arena: &'a Arena<'a, 'ast>, toplevel: PackageRef<'a, 'ast>, id: &[Ident]) -> PackageRef<'a, 'ast> {
    id.iter().enumerate().fold(toplevel, |package, (ix, ident)| {
        let new = |:| arena.alloc(Package::new(Qualified(id[0..ix+1].iter()).to_string()));
        match package.contents.borrow_mut().entry(ident.node) {
            hash_map::Entry::Occupied(mut v) => {
                let slot = v.get_mut();
                match *slot {
                    PackageItem::Package(it) => return it, // Found it
                    PackageItem::TypeDefinition(ref tydef) => {
                        // There was a type instead!
                        type_package_conflict(&**tydef);
                    }
                }
                // Kick out the type and put a package instead.
                // This prevents a spray of errors in case one type conflicts with a package with
                // many compilation units
                let next = new();
                *slot = PackageItem::Package(next);
                next
            }
            hash_map::Entry::Vacant(v) => {
                let next = new();
                v.insert(PackageItem::Package(next));
                next
            }
        }
    })
}

// Phase 1.
pub fn collect_types<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                               toplevel: PackageRef<'a, 'ast>,
                               asts: &'ast [ast::CompilationUnit])
-> Vec<(PackageRef<'a, 'ast>, &'ast ast::CompilationUnit, Vec<TypeDefinitionRef<'a, 'ast>>)> {
    let default_package: PackageRef<'a, 'ast> = arena.alloc(Package::new("default package".to_owned()));
    asts.iter().map(|ast| {
        let (package, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_create_package(arena, toplevel, &*package_identifier.node.parts),
             package_identifier.node.parts.iter().map(|x| x.node).collect())
        } else {
            (default_package, vec![])
        };

        let mut collector = Collector {
            arena: arena,
            package: package,
            scope: scope,
            type_definition: None,
            all_tydefs: vec![],
        };
        collector.walk_compilation_unit(ast);

        (package, ast, collector.all_tydefs)
    }).collect()
}

