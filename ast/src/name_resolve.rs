use ast::*;
use name::*;
use walker::*;

use std::borrow::ToOwned;
use std::collections::{hash_map, HashMap};

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

#[derive(Show)]
pub enum NamedItem {
    Package(Package),
    TypeDefinition(TypeDefinition),
    // TODO: Store AST bits.
    Field,
    Method,
}

impl NamedItem {
    pub fn package(&self) -> &Package {
        match *self {
            NamedItem::Package(ref p) => p,
            _ => panic!("expected a package"),
        }
    }
    pub fn package_mut(&mut self) -> &mut Package {
        match *self {
            NamedItem::Package(ref mut p) => p,
            _ => panic!("expected a package"),
        }
    }
}

#[derive(Show)]
pub enum TypeKind {
    Class,
    Interface,
}

#[derive(Show)]
pub struct Package {
    contents: HashMap<Symbol, Name>
}

impl Package {
    pub fn new() -> Package {
        Package {
            contents: HashMap::new(),
        }
    }
}

#[derive(Show)]
pub struct TypeDefinition {
    kind: TypeKind,

    // Note that members and methods can have the same name, therefore
    // need to be be in separate namespaces.
    members: HashMap<Symbol, Name>,
    methods: HashMap<Symbol, Name>,
}

impl TypeDefinition {
    pub fn new(kind: TypeKind) -> TypeDefinition {
        TypeDefinition {
            kind: kind,
            members: HashMap::new(),
            methods: HashMap::new(),
        }
    }
}

struct Collector<'all, 'ast> {
    all: &'all mut HashMap<Name, NamedItem>,
    scope: Vec<Symbol>,
    package: Name,
    type_definition: Option<TypeDefinition>,
}

impl<'all, 'ast> Walker<'ast> for Collector<'all, 'ast> {
    fn walk_class_field(&mut self, field: &Field) {
        let field_name = Name::fresh(format!("{}.{}", Qualified(self.scope.iter()), field.node.name));

        debug_assert!(!self.all.contains_key(&field_name));
        self.all.insert(field_name, NamedItem::Field); // TODO: Maybe record some information for the field
        if let Some(_) = self.type_definition.as_mut().unwrap().members.insert(field.node.name.node, field_name) {
            // something with the same name was already there!
            span_error!(field.span, "field `{}` already exists in `{}`",
                        field.node.name, Qualified(self.scope.iter()));
        }

        // no need to walk deeper
    }
    fn walk_class_method(&mut self, method: &Method) {
        // Note: Implementation is shared with interfaace methods

        let Collector { ref mut all, ref mut type_definition, .. } = *self;
        let mut ty_methods = &mut type_definition.as_mut().unwrap().methods;
        match ty_methods.entry(method.node.name.node) {
            hash_map::Entry::Occupied(_v) => {
                // Overloaded method.
                // TODO: Record information about this overload (?)
            }
            hash_map::Entry::Vacant(v) => {
                // First time seeing this method.
                let method_name = Name::fresh(format!("{}.{}", Qualified(self.scope.iter()), method.node.name));

                debug_assert!(!all.contains_key(&method_name));
                all.insert(method_name, NamedItem::Method);
                v.insert(method_name);
            }
        }

        // no need to walk deeper
    }
    fn walk_interface_method(&mut self, method: &Method) {
        // same as a class method
        self.walk_class_method(method);
    }

    fn walk_class(&mut self, class: &Class) {
        assert!(self.type_definition.is_none());

        self.scope.push(class.node.name.node);
        let fq_type = Name::fresh(format!("{}", Qualified(self.scope.iter())));
        match self.all.get_mut(&self.package).unwrap().package_mut().contents.entry(class.node.name.node) {
            hash_map::Entry::Occupied(_v) => {
                span_error!(class.node.name.span,
                            "type `{}` already exists in package `{:?}`",
                            class.node.name, self.package);
            }
            hash_map::Entry::Vacant(v) => {
                v.insert(fq_type);
            }
        }

        self.type_definition = Some(TypeDefinition::new(TypeKind::Class));
        default_walk_class(self, class);
        self.all.insert(fq_type, NamedItem::TypeDefinition(self.type_definition.take().unwrap()));
        self.scope.pop();
    }

    fn walk_interface(&mut self, interface: &Interface) {
        // XXX: This is almost identical to the above, factor it out pls
        assert!(self.type_definition.is_none());

        self.scope.push(interface.node.name.node);
        let fq_type = Name::fresh(format!("{}", Qualified(self.scope.iter())));
        match self.all.get_mut(&self.package).unwrap().package_mut().contents.entry(interface.node.name.node) {
            hash_map::Entry::Occupied(_v) => {
                span_error!(interface.node.name.span,
                            "type `{}` already exists in package `{:?}`",
                            interface.node.name, self.package);
            }
            hash_map::Entry::Vacant(v) => {
                v.insert(fq_type);
            }
        }

        self.type_definition = Some(TypeDefinition::new(TypeKind::Interface));
        default_walk_interface(self, interface);
        self.all.insert(fq_type, NamedItem::TypeDefinition(self.type_definition.take().unwrap()));
        self.scope.pop();
    }
}

// Looks up a package by qualified identifier, creating it if necessary.
fn resolve_package(all: &mut HashMap<Name, NamedItem>, toplevel: Name, id: &QualifiedIdentifier) -> Name {
    let mut name = toplevel;
    for (ix, ident) in id.node.parts.iter().enumerate() {
        if let NamedItem::Package(ref mut package) = *all.get_mut(&name).unwrap() {
            match package.contents.entry(ident.node) {
                hash_map::Entry::Occupied(v) => {
                    // Found it
                    name = *v.get();
                }
                hash_map::Entry::Vacant(v) => {
                    name = Name::fresh(format!("{}", Qualified(id.node.parts[0..ix+1].iter())));
                    v.insert(name);
                }
            }
        } else {
            span_error!(ident.span,
                        // FIXME: better error message
                        "package name conflict");
            // create a new name for now...
            // XXX: One bad class processed early could cause a ton of package name conflicts
            name = Name::fresh(Qualified(id.node.parts[0..ix+1].iter()).to_string());
        }
        // make sure a package exists with this name
        if let hash_map::Entry::Vacant(v) = all.entry(name) {
            v.insert(NamedItem::Package(Package::new()));
        }
    }
    name
}

pub fn fully_qualify_names(asts: &Vec<CompilationUnit>) -> HashMap<Name, NamedItem> {
    let mut all: HashMap<Name, NamedItem> = HashMap::new();

    let toplevel = Name::fresh("top level".to_owned());
    all.insert(toplevel, NamedItem::Package(Package::new()));

    for ast in asts.iter() {
        let (package_name, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_package(&mut all, toplevel, package_identifier),
             package_identifier.node.parts.iter().map(|x| x.node).collect())
        } else {
            (toplevel, vec![])
        };

        Collector {
            all: &mut all,
            scope: scope,
            package: package_name,
            type_definition: None,
        }.walk_compilation_unit(ast);
    }

    all
}

pub fn name_resolve(asts: &Vec<CompilationUnit>) {
    let mut all: HashMap<Name, NamedItem> = fully_qualify_names(asts);

    // For testing - remove when name resolution is finished.
    for (name, named_item) in all.iter() {
        println!("{:?}: {:?}", name, named_item);
    }
}
