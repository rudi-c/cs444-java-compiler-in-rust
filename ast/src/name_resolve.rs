use ast;
use name::*;
use walker::*;

use std::borrow::ToOwned;
use std::collections::{hash_map, HashMap};
use std::cell::RefCell;
use arena::TypedArena;

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

// TODO: Store AST bits.

pub struct Arenas<'a, 'ast: 'a> {
    package: TypedArena<RefCell<Package<'a, 'ast> > >,
    type_definition: TypedArena<RefCell<TypeDefinition<'a, 'ast> > >,
    field: TypedArena<RefCell<Field<'a, 'ast> > >,
    method: TypedArena<RefCell<Method<'a, 'ast> > >,
}
impl<'a, 'ast> Arenas<'a, 'ast> {
    pub fn new() -> Arenas<'a, 'ast> {
        Arenas {
            package: TypedArena::new(),
            type_definition: TypedArena::new(),
            field: TypedArena::new(),
            method: TypedArena::new(),
        }
    }
    pub fn new_package(&'a self, name: String) -> PackageRef<'a, 'ast> { self.package.alloc(RefCell::new(Package::new(name))) }
    pub fn new_type_definition(&'a self, name: String, kind: TypeKind) -> TypeDefinitionRef<'a, 'ast> { self.type_definition.alloc(RefCell::new(TypeDefinition::new(name, kind))) }
    pub fn new_field(&'a self, name: String, ast: &'ast ast::Field) -> FieldRef<'a, 'ast> { self.field.alloc(RefCell::new(Field::new(name, ast))) }
    pub fn new_method(&'a self, name: String) -> MethodRef<'a, 'ast> { self.method.alloc(RefCell::new(Method::new(name))) }
}

#[derive(Show)]
pub enum PackageItem<'a, 'ast: 'a> {
    Package(PackageRef<'a, 'ast>),
    TypeDefinition(TypeDefinitionRef<'a, 'ast>),
}

#[derive(Show, Copy)]
pub enum TypeKind {
    Class,
    Interface,
}

#[derive(Show)]
pub struct Package<'a, 'ast: 'a> {
    fq_name: String,
    contents: HashMap<Symbol, PackageItem<'a, 'ast>>
}
pub type PackageRef<'a, 'ast> = &'a RefCell<Package<'a, 'ast>>;

impl<'a, 'ast> Package<'a, 'ast> {
    pub fn new(name: String) -> Package<'a, 'ast> {
        Package {
            fq_name: name,
            contents: HashMap::new(),
        }
    }
}

#[derive(Show)]
pub struct Field<'a, 'ast: 'a> {
    fq_name: String,
    ast: &'ast ast::Field,
}
pub type FieldRef<'a, 'ast> = &'a RefCell<Field<'a, 'ast>>;

impl<'a, 'ast> Field<'a, 'ast> {
    pub fn new(name: String, ast: &'ast ast::Field) -> Field<'a, 'ast> {
        Field {
            fq_name: name,
            ast: ast,
        }
    }
}

#[derive(Show)]
pub struct Method<'a, 'ast: 'a> {
    fq_name: String,
    // TODO: Promote overloads to their own struct, maybe?
    overloads: Vec<&'ast ast::Method>,
}
pub type MethodRef<'a, 'ast> = &'a RefCell<Method<'a, 'ast>>;

impl<'a, 'ast> Method<'a, 'ast> {
    pub fn new(name: String) -> Method<'a, 'ast> {
        Method {
            fq_name: name,
            overloads: vec![],
        }
    }
}

#[derive(Show)]
pub struct TypeDefinition<'a, 'ast: 'a> {
    fq_name: String,
    kind: TypeKind,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    fields: HashMap<Symbol, FieldRef<'a, 'ast>>,
    methods: HashMap<Symbol, MethodRef<'a, 'ast>>,
}
pub type TypeDefinitionRef<'a, 'ast> = &'a RefCell<TypeDefinition<'a, 'ast>>;

impl<'a, 'ast> TypeDefinition<'a, 'ast> {
    pub fn new(name: String, kind: TypeKind) -> TypeDefinition<'a, 'ast> {
        TypeDefinition {
            fq_name: name,
            kind: kind,
            fields: HashMap::new(),
            methods: HashMap::new(),
        }
    }
}

struct Collector<'all, 'ast: 'all> {
    arena: &'all Arenas<'all, 'ast>,
    package: PackageRef<'all, 'ast>,
    scope: Vec<Symbol>,
    type_definition: Option<TypeDefinitionRef<'all, 'ast>>,
}

impl<'all, 'ast> Collector<'all, 'ast> {
    fn walk_type<F: FnOnce(&mut Self)>(&mut self, name: &Ident, kind: TypeKind, f: F) {
        assert!(self.type_definition.is_none());

        self.scope.push(name.node);
        let fq_type = Qualified(self.scope.iter()).to_string();

        let tydef = match self.package.borrow_mut().contents.entry(name.node) {
            hash_map::Entry::Occupied(_v) => {
                span_error!(name.span,
                            "type `{}` already exists in package `{:?}`",
                            name, self.package);
                return
            }
            hash_map::Entry::Vacant(v) => {
                let def = self.arena.new_type_definition(fq_type, kind);
                v.insert(PackageItem::TypeDefinition(def));
                def
            }
        };

        self.type_definition = Some(tydef);

        f(self);

        self.type_definition = None;
        self.scope.pop();
    }
}

impl<'all, 'ast> Walker<'ast> for Collector<'all, 'ast> {
    fn walk_class_field(&mut self, field: &'ast ast::Field) {
        let field_name = format!("{}.{}", Qualified(self.scope.iter()), field.node.name);
        let tydef = self.type_definition.as_ref().unwrap();
        if let Some(_) = tydef.borrow_mut().fields.insert(field.node.name.node, self.arena.new_field(field_name, field)) {
            // something with the same name was already there!
            span_error!(field.span, "field `{}` already exists in `{}`",
                        field.node.name, Qualified(self.scope.iter()));
        }

        // no need to walk deeper
    }
    fn walk_class_method(&mut self, method_ast: &'ast ast::Method) {
        // Note: Implementation is shared with interfaace methods

        let mut tydef = self.type_definition.as_ref().unwrap().borrow_mut();
        let method = tydef.methods.entry(method_ast.node.name.node).get().unwrap_or_else(|v| {
            // First time seeing this method.
            let method_name = format!("{}.{}", Qualified(self.scope.iter()), method_ast.node.name);
            v.insert(self.arena.new_method(method_name))
        });
        method.borrow_mut().overloads.push(method_ast);

        // no need to walk deeper
    }
    fn walk_interface_method(&mut self, method: &'ast ast::Method) {
        // same as a class method
        self.walk_class_method(method);
    }

    fn walk_class(&mut self, class: &'ast ast::Class) {
        self.walk_type(&class.node.name, TypeKind::Class, |me| default_walk_class(me, class));
    }

    fn walk_interface(&mut self, interface: &'ast ast::Interface) {
        self.walk_type(&interface.node.name, TypeKind::Interface, |me| default_walk_interface(me, interface));
    }
}

// Looks up a package by qualified identifier, creating it if necessary.
// If a name conflict occurs, this will create a dummy package.
fn resolve_package<'all, 'ast>(arena: &'all Arenas<'all, 'ast>, toplevel: PackageRef<'all, 'ast>, id: &QualifiedIdentifier) -> PackageRef<'all, 'ast> {
    let mut package = toplevel;
    for (ix, ident) in id.node.parts.iter().enumerate() {
        match package.borrow_mut().contents.entry(ident.node) {
            hash_map::Entry::Occupied(v) => {
                if let PackageItem::Package(it) = *v.get() {
                    // Found it
                    package = it;
                } else {
                    span_error!(ident.span,
                                // FIXME: better error message
                                "package name conflict");
                    // create a new name for now...
                    // XXX: One bad class processed early could cause a ton of package name conflicts
                    package = arena.new_package(Qualified(id.node.parts[0..ix+1].iter()).to_string());
                }
            }
            hash_map::Entry::Vacant(v) => {
                package = arena.new_package(Qualified(id.node.parts[0..ix+1].iter()).to_string());
                v.insert(PackageItem::Package(package));
            }
        }
    }
    package
}

pub fn fully_qualify_names<'all, 'ast>(arena: &'all Arenas<'all, 'ast>, toplevel: PackageRef<'all, 'ast>, asts: &'ast [ast::CompilationUnit]) {
    for ast in asts.iter() {
        let (package, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_package(arena, toplevel, package_identifier),
             package_identifier.node.parts.iter().map(|x| x.node).collect())
        } else {
            (toplevel, vec![])
        };

        Collector {
            arena: arena,
            package: package,
            scope: scope,
            type_definition: None,
        }.walk_compilation_unit(ast);
    }
}

pub fn name_resolve(asts: &[ast::CompilationUnit]) {
    let arena = Arenas::new();
    let toplevel = arena.new_package("top level".to_owned());
    fully_qualify_names(&arena, toplevel, asts);

    // For testing - remove when name resolution is finished.
    // XXX: This may lead to an infinite loop later
    println!("{:?}", toplevel);
}
