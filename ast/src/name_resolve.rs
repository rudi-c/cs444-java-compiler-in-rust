use ast;
use name::*;
use walker::*;

use rbtree::RbMap;

use std::borrow::ToOwned;
use std::collections::{hash_map, HashMap};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

fn rc_cell<T>(x: T) -> Rc<RefCell<T>> { Rc::new(RefCell::new(x)) }

// The idea here is to have an object graph represented using Rc pointers.
// RefCell allows the graph to be mutated by later passes.
// To avoid cycles, Rc pointers should be used for objects that are conceptually "children" (e.g.
// types in a package, methods in a type); weak pointers elsewhere.
// Every object has a unique `Name`, which can be used as an key for external maps. There is
// intentionally no way to go from a `Name` back to the object: if you need it, just store the
// pointer! `Name`s are also generally associated with fully-qualified identifiers, suitable for
// printing in error messages.
// Many objects also hold references to their associated AST nodes, hence the 'ast lifetime
// everywhere.

#[derive(Show)]
pub enum PackageItem<'ast> {
    Package(PackageRef<'ast>),
    TypeDefinition(TypeDefinitionRef<'ast>),
}

#[derive(Show)]
pub struct Package<'ast> {
    fq_name: Name,
    contents: HashMap<Symbol, PackageItem<'ast>>
}
pub type PackageRef<'ast> = Rc<RefCell<Package<'ast>>>;
pub type PackageWeak<'ast> = Weak<RefCell<Package<'ast>>>;

impl<'ast> Package<'ast> {
    pub fn new(name: String) -> PackageRef<'ast> {
        rc_cell(Package {
            fq_name: Name::fresh(name),
            contents: HashMap::new(),
        })
    }
}

#[derive(Show)]
pub struct Field<'ast> {
    fq_name: Name,
    ast: &'ast ast::Field,
}
pub type FieldRef<'ast> = Rc<RefCell<Field<'ast>>>;
pub type FieldWeak<'ast> = Weak<RefCell<Field<'ast>>>;

impl<'ast> Field<'ast> {
    pub fn new(name: String, ast: &'ast ast::Field) -> FieldRef<'ast> {
        rc_cell(Field {
            fq_name: Name::fresh(name),
            ast: ast,
        })
    }
}

#[derive(Show)]
pub struct Method<'ast> {
    fq_name: Name,
    // TODO: Promote overloads to their own struct, maybe?
    overloads: Vec<&'ast ast::Method>,
}
pub type MethodRef<'ast> = Rc<RefCell<Method<'ast>>>;
pub type MethodWeak<'ast> = Weak<RefCell<Method<'ast>>>;

impl<'ast> Method<'ast> {
    pub fn new(name: String) -> MethodRef<'ast> {
        rc_cell(Method {
            fq_name: Name::fresh(name),
            overloads: vec![],
        })
    }
}

#[derive(Show, Copy)]
pub enum TypeKind {
    Class,
    Interface,
}

#[derive(Show)]
pub struct TypeDefinition<'ast> {
    fq_name: Name,
    kind: TypeKind,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    fields: HashMap<Symbol, FieldRef<'ast>>,
    methods: HashMap<Symbol, MethodRef<'ast>>,

    ast: &'ast ast::TypeDeclaration,
}
pub type TypeDefinitionRef<'ast> = Rc<RefCell<TypeDefinition<'ast>>>;
pub type TypeDefinitionWeak<'ast> = Weak<RefCell<TypeDefinition<'ast>>>;

impl<'ast> TypeDefinition<'ast> {
    pub fn new(name: String, kind: TypeKind, ast: &'ast ast::TypeDeclaration) -> TypeDefinitionRef<'ast> {
        rc_cell(TypeDefinition {
            fq_name: Name::fresh(name),
            kind: kind,
            fields: HashMap::new(),
            methods: HashMap::new(),
            ast: ast,
        })
    }
}

struct Collector<'ast> {
    package: PackageRef<'ast>,
    scope: Vec<Symbol>,
    type_definition: Option<TypeDefinitionRef<'ast>>,
}

impl<'ast> Walker<'ast> for Collector<'ast> {
    fn walk_class_field(&mut self, field: &'ast ast::Field) {
        let field_name = format!("{}.{}", Qualified(self.scope.iter()), field.node.name);
        let tydef = self.type_definition.as_ref().unwrap();
        if let Some(_) = tydef.borrow_mut().fields.insert(field.node.name.node, Field::new(field_name, field)) {
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
            v.insert(Method::new(method_name))
        });
        method.borrow_mut().overloads.push(method_ast);

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
        let tydef = TypeDefinition::new(fq_type, kind, ty_decl);

        // Insert `tydef` into the package
        {
            let mut package = self.package.borrow_mut();
            // XXX: Need to go through some contortions here to make rustc recognize the mutable
            // deref
            let &mut Package { ref fq_name, ref mut contents } = &mut *package;
            match contents.entry(name.node) {
                hash_map::Entry::Occupied(v) => {
                    match *v.get() {
                        PackageItem::Package(..) => {
                            type_package_conflict(&*tydef.borrow());
                        }
                        PackageItem::TypeDefinition(..) => {
                            span_error!(name.span,
                                        "type `{}` already exists in package `{}`",
                                        name, fq_name);
                        }
                    }
                    return
                }
                hash_map::Entry::Vacant(v) => {
                    v.insert(PackageItem::TypeDefinition(tydef.clone()));
                }
            }
        }

        self.type_definition = Some(tydef);

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
fn resolve_package<'ast>(toplevel: PackageRef<'ast>, id: &QualifiedIdentifier) -> PackageRef<'ast> {
    id.node.parts.iter().enumerate().fold(toplevel, |package, (ix, ident)| {
        let new = |:| Package::new(Qualified(id.node.parts[0..ix+1].iter()).to_string());
        match package.borrow_mut().contents.entry(ident.node) {
            hash_map::Entry::Occupied(mut v) => {
                let mut slot = v.get_mut();
                match *slot {
                    PackageItem::Package(ref it) => return it.clone(), // Found it
                    PackageItem::TypeDefinition(ref tydef) => {
                        // There was a type instead!
                        type_package_conflict(&*tydef.borrow());
                    }
                }
                // Kick out the type and put a package instead.
                // This prevents a spray of errors in case one type conflicts with a package with
                // many compilation units
                let next = new();
                *slot = PackageItem::Package(next.clone());
                next
            }
            hash_map::Entry::Vacant(v) => {
                let next = new();
                v.insert(PackageItem::Package(next.clone()));
                next
            }
        }
    })
}

pub fn fully_qualify_names<'ast>(toplevel: PackageRef<'ast>, asts: &'ast [ast::CompilationUnit]) {
    for ast in asts.iter() {
        let (package, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_package(toplevel.clone(), package_identifier),
             package_identifier.node.parts.iter().map(|x| x.node).collect())
        } else {
            (toplevel.clone(), vec![])
        };

        Collector {
            package: package,
            scope: scope,
            type_definition: None,
        }.walk_compilation_unit(ast);
    }
}

pub fn name_resolve(asts: &[ast::CompilationUnit]) {
    let toplevel = Package::new("top level".to_owned());
    fully_qualify_names(toplevel.clone(), asts);

    // For testing - remove when name resolution is finished.
    // XXX: This may lead to an infinite loop later
    println!("{:?}", toplevel);
}
