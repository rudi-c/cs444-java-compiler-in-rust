use ast;
use name::*;
use ast::walker::*;
use arena::Arena;

use std::collections::hash_map;

use middle::*;

struct Collector<'a, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    package: PackageRef<'a, 'ast>,
    scope: Vec<Symbol>,
    tydef: Option<TypeDefinitionRef<'a, 'ast>>,
}

impl<'a, 'ast> Walker<'ast> for Collector<'a, 'ast> {
    fn walk_type_declaration(&mut self, ty_decl: &'ast ast::TypeDeclaration) {
        let name = ty_decl.name();
        let kind = match ty_decl.node {
            ast::TypeDeclaration_::Class(..) => TypeKind::Class,
            ast::TypeDeclaration_::Interface(..) => TypeKind::Interface,
        };
        self.scope.push(name.node);
        let fq_type = Qualified(self.scope.iter()).to_string();
        let tydef = self.arena.alloc(TypeDefinition::new(fq_type, kind, self.package, ty_decl));

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

        self.tydef = Some(tydef);
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
        let new = || arena.alloc(Package::new(Qualified(id[0..ix+1].iter()).to_string()));
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
                               default_package: PackageRef<'a, 'ast>,
                               asts: &'ast [ast::CompilationUnit])
-> Vec<(PackageRef<'a, 'ast>, &'ast ast::CompilationUnit, TypeDefinitionRef<'a, 'ast>)> {
    let mut r = vec![];
    for ast in asts.iter() {
        let (package, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_create_package(arena, toplevel, &*package_identifier.parts),
             package_identifier.parts.iter().map(|x| x.node).collect())
        } else {
            (default_package, vec![Symbol::from_str("")])
        };

        let mut collector = Collector {
            arena: arena,
            package: package,
            scope: scope,
            tydef: None,
        };
        collector.walk_compilation_unit(ast);
        if let Some(tydef) = collector.tydef {
            r.push((package, ast, tydef));
        }
    }
    r
}

