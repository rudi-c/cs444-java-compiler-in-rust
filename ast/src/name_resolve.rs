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

#[derive(Show, Clone)]
pub enum PackageItem<'ast> {
    Package(PackageRef<'ast>),
    TypeDefinition(TypeDefinitionRef<'ast>),
}

impl<'ast> PackageItem<'ast> {
    pub fn fq_name(&self) -> Name {
        match self {
            &PackageItem::Package(ref package) => package.borrow().fq_name,
            &PackageItem::TypeDefinition(ref typedef) => typedef.borrow().fq_name,
        }
    }
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

pub fn fully_qualify_names<'ast>(toplevel: PackageRef<'ast>,
                                 asts: &'ast [ast::CompilationUnit]) {
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

enum SymTableItem<'ast> {
    LocalVariable,
    Field(Field<'ast>),
}

type TypesEnvironment<'ast> = RbMap<Name, PackageItem<'ast>>;
type NonTypesEnvironment<'ast> = RbMap<Name, SymTableItem<'ast>>;

struct EnvironmentStack<'ast> {
    types: TypesEnvironment<'ast>,
    nonTypes: Vec<NonTypesEnvironment<'ast>>
}

impl<'ast> Walker<'ast> for EnvironmentStack<'ast> {

}

// Returns the PackageItem corresponding to a path of identifiers a.b.c
pub fn resolve_import<'ast>(package: PackageRef<'ast>, path: &[Ident])
        -> Option<PackageItem<'ast>> {
    match path.first() {
        // Base case
        None => Some(PackageItem::Package(package)),
        Some(first) => match package.borrow().contents.get(&first.node) {
            Some(&PackageItem::Package(ref found_package)) => {
                resolve_import(found_package.clone(), path.tail())
            },
            Some(&PackageItem::TypeDefinition(ref typedef)) => {
                if path.len() == 1 {
                    Some(PackageItem::TypeDefinition(typedef.clone()))
                } else {
                    // No nested classes, so shouldn't traverse past a typedef.
                    None
                }
            },
            None => None,
        },
    }
}

fn insert_check_present<'ast>(symbol: &Symbol,
                              package_item: &PackageItem<'ast>,
                              imported: &QualifiedIdentifier,
                              current_env: TypesEnvironment<'ast>)
        -> TypesEnvironment<'ast> {
    let name = Name::fresh(symbol.to_string());
    let (new_env, previous_opt) = current_env.insert(name, package_item.clone());
    if let Some(previous) = previous_opt {
        if previous.1.fq_name() != package_item.fq_name() {
            span_error!(imported.span,
                        "importing `{}` from `{}` conflicts with previous import",
                        symbol,
                        imported);
        }
    }
    new_env
}

fn import_single_type<'ast>(imported: &QualifiedIdentifier,
                            toplevel: PackageRef<'ast>,
                            current_env: TypesEnvironment<'ast>)
        -> TypesEnvironment<'ast> {
    let resolved_import = resolve_import(toplevel,
                                         imported.node.parts.as_slice());
    if let Some(package_item) = resolved_import {
        insert_check_present(&imported.node.parts.last().unwrap().node,
                             &package_item,
                             imported,
                             current_env)
    } else {
        span_error!(imported.span,
                    "cannot find package `{}` to import '*'",
                    imported);
        current_env
    }
}

fn import_on_demand<'ast>(imported: &QualifiedIdentifier,
                          toplevel: PackageRef<'ast>,
                          current_env: TypesEnvironment<'ast>)
        -> TypesEnvironment<'ast> {
    let resolved_import = resolve_import(toplevel,
                                          imported.all_but_last());
    match resolved_import {
        Some(PackageItem::Package(ref package)) => {
            // Add every type and package to the environment.
            package.borrow()
                   .contents
                   .iter()
                   .fold(current_env, |env, (symbol, package_item)| {
                insert_check_present(symbol,
                                     package_item,
                                     imported,
                                     env)
            })
        },
        Some(PackageItem::TypeDefinition(_)) |
        None => {
            span_error!(imported.span,
                        "cannot find package `{}` to import",
                        imported);
            current_env
        }
    }
}

fn build_environments<'ast>(toplevel: PackageRef<'ast>,
                            asts: &'ast [ast::CompilationUnit]) {
    for ast in asts.iter() {
        let mut types_env: TypesEnvironment<'ast> = RbMap::new();

        // Add all imports to initial environment for this compilation unit.
        for import in ast.imports.iter() {
            match import.node {
                ast::ImportDeclaration_::SingleType(ref qident) => {
                    types_env = import_single_type(qident,
                                                   toplevel.clone(),
                                                   types_env);
                },
                ast::ImportDeclaration_::OnDemand(ref qident) => {
                    types_env = import_on_demand(qident,
                                                 toplevel.clone(),
                                                 types_env);
                },
            }
        }

        EnvironmentStack {
            types: types_env,
            nonTypes: vec![RbMap::new()],
        }.walk_compilation_unit(ast);
    }
}

pub fn name_resolve(asts: &[ast::CompilationUnit]) {
    let toplevel = Package::new("top level".to_owned());
    fully_qualify_names(toplevel.clone(), asts);
    build_environments(toplevel.clone(), asts);

    // For testing - remove when name resolution is finished.
    // XXX: This may lead to an infinite loop later
    println!("{:?}", toplevel);
}
