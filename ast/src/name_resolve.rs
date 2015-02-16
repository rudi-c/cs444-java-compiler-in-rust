use ast;
use name::*;
use walker::*;

use std::borrow::ToOwned;
use std::collections::{hash_map, HashMap};
use std::cell::RefCell;
use std::rc::{Rc, Weak};

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

fn rc_cell<T>(x: T) -> Rc<RefCell<T>> { Rc::new(RefCell::new(x)) }

// TODO: Store AST bits.

#[derive(Show)]
pub enum PackageItem<'ast> {
    Package(PackageRef<'ast>),
    TypeDefinition(TypeDefinitionRef<'ast>),
}

#[derive(Show, Copy)]
pub enum TypeKind {
    Class,
    Interface,
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

#[derive(Show)]
pub struct TypeDefinition<'ast> {
    fq_name: Name,
    kind: TypeKind,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    fields: HashMap<Symbol, FieldRef<'ast>>,
    methods: HashMap<Symbol, MethodRef<'ast>>,
}
pub type TypeDefinitionRef<'ast> = Rc<RefCell<TypeDefinition<'ast>>>;
pub type TypeDefinitionWeak<'ast> = Weak<RefCell<TypeDefinition<'ast>>>;

impl<'ast> TypeDefinition<'ast> {
    pub fn new(name: String, kind: TypeKind) -> TypeDefinitionRef<'ast> {
        rc_cell(TypeDefinition {
            fq_name: Name::fresh(name),
            kind: kind,
            fields: HashMap::new(),
            methods: HashMap::new(),
        })
    }
}

struct Collector<'ast> {
    package: PackageRef<'ast>,
    scope: Vec<Symbol>,
    type_definition: Option<TypeDefinitionRef<'ast>>,
}

impl<'ast> Collector<'ast> {
    fn walk_type<F: FnOnce(&mut Self)>(&mut self, name: &Ident, kind: TypeKind, f: F) {
        assert!(self.type_definition.is_none());

        self.scope.push(name.node);

        let tydef = match self.package.borrow_mut().contents.entry(name.node) {
            hash_map::Entry::Occupied(_v) => {
                span_error!(name.span,
                            "type `{}` already exists in package `{:?}`",
                            name, self.package);
                return
            }
            hash_map::Entry::Vacant(v) => {
                let fq_type = Qualified(self.scope.iter()).to_string();
                let def = TypeDefinition::new(fq_type, kind);
                v.insert(PackageItem::TypeDefinition(def.clone()));
                def
            }
        };

        self.type_definition = Some(tydef);

        f(self);

        self.type_definition = None;
        self.scope.pop();
    }
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

    fn walk_class(&mut self, class: &'ast ast::Class) {
        self.walk_type(&class.node.name, TypeKind::Class, |me| default_walk_class(me, class));
    }

    fn walk_interface(&mut self, interface: &'ast ast::Interface) {
        self.walk_type(&interface.node.name, TypeKind::Interface, |me| default_walk_interface(me, interface));
    }
}

// Looks up a package by qualified identifier, creating it if necessary.
// If a name conflict occurs, this will create a dummy package.
fn resolve_package<'ast>(toplevel: PackageRef<'ast>, id: &QualifiedIdentifier) -> PackageRef<'ast> {
    id.node.parts.iter().enumerate().fold(toplevel, |package, (ix, ident)| {
        match package.borrow_mut().contents.entry(ident.node) {
            hash_map::Entry::Occupied(v) => {
                if let PackageItem::Package(ref it) = *v.get() {
                    // Found it
                    it.clone()
                } else {
                    span_error!(ident.span,
                                // FIXME: better error message
                                "package name conflict");
                    // create a new name for now...
                    // XXX: One bad class processed early could cause a ton of package name conflicts
                    Package::new(Qualified(id.node.parts[0..ix+1].iter()).to_string())
                }
            }
            hash_map::Entry::Vacant(v) => {
                let next = Package::new(Qualified(id.node.parts[0..ix+1].iter()).to_string());
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
