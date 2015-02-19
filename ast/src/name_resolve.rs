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

    pub fn print_light(&self) {
        println!("{}", self.fq_name());
        if let &PackageItem::Package(ref package) = self {
            for package_item in package.borrow().contents.values() {
                package_item.print_light();
            }
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

    extends: Vec<TypeDefinitionRef<'ast>>,
    implements: Vec<TypeDefinitionRef<'ast>>,

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
            extends: vec![],
            implements: vec![],
            ast: ast,
        })
    }
}

#[derive(Show, Clone)]
pub enum Type<'ast> {
    SimpleType(SimpleType<'ast>),
    ArrayType(SimpleType<'ast>),

    // Placeholder when name resolution fails.
    Unknown,
}

#[derive(Show, Clone)]
pub enum SimpleType<'ast> {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(TypeDefinitionRef<'ast>),
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

#[derive(Show, Clone)]
enum SymTableItem<'ast> {
    Type(TypeDefinitionRef<'ast>),
    LocalVariable,
    Field(FieldRef<'ast>, Type<'ast>),
    Method(MethodRef<'ast>),
}


type TypesEnvironment<'ast> = RbMap<Symbol, TypeDefinitionRef<'ast>>;
type NonTypesEnvironment<'ast> = RbMap<Symbol, SymTableItem<'ast>>;

struct EnvironmentStack<'ast> {
    // Since there are no nested classes and only one type per file,
    // this is not a stack/vector and we just use mutation to add the
    // single class/interface in the file to the initial environment
    // containing imported types.
    types: TypesEnvironment<'ast>,
    non_types: Vec<NonTypesEnvironment<'ast>>,
    toplevel: PackageRef<'ast>,
    package: Option<QualifiedIdentifier>,
}

impl<'ast> Walker<'ast> for EnvironmentStack<'ast> {
    fn walk_class(&mut self, class: &'ast ast::Class) {
        let ref class_name = class.node.name;
        let typedef = self.get_type_declaration(class_name).unwrap();

        let mut non_types_env = self.non_types.last().unwrap().clone();

        // Process the class' inheritance.
        if let Some(ref extension) = class.node.extends {
            self.resolve_extensions(typedef.clone(),
                                    &non_types_env,
                                    &[extension.clone()]);
        }
        self.resolve_implements(typedef.clone(),
                                &non_types_env,
                                class.node.implements.as_slice());

        // Add the class itself to the environment.
        self.types = insert_declared_type(&self.types, class_name, typedef.clone());

        // Process class body.
        // Is there a neater way to do this filter?
        let fields: Vec<&ast::Field> =
            class.node.body
                 .iter()
                 .filter_map(|dcl| {
                   if let ast::ClassBodyDeclaration_::FieldDeclaration(ref field) = dcl.node {
                       Some(field)
                   } else {
                       None
                   }})
                 .collect();
        non_types_env = self.collect_fields(non_types_env, fields.as_slice(), typedef);
    }

    fn walk_interface(&mut self, interface: &'ast ast::Interface) {
        let ref interface_name = interface.node.name;
        let typedef = self.get_type_declaration(interface_name).unwrap();

        let mut non_types_env = self.non_types.last().unwrap();

        // Process the interface's inheritance.
        self.resolve_extensions(typedef.clone(),
                                non_types_env,
                                interface.node.extends.as_slice());

        // Add the interface itself to the environment.
        self.types = insert_declared_type(&self.types, interface_name, typedef);
    }
}

impl<'ast> EnvironmentStack<'ast> {
    fn get_type_declaration(&self, ident: &Ident) -> Option<TypeDefinitionRef<'ast>> {
        if let Some(ref package) = self.package {
            let fq_parts = package.append_ident(ident).node.parts;
            let resolved_import = resolve_import(self.toplevel.clone(),
                                                 fq_parts.as_slice());

            if let Some(PackageItem::TypeDefinition(typedef)) = resolved_import {
                Some(typedef)
            } else {
                None
            }
        } else {
            // Top level.
            let resolved_import = resolve_import(self.toplevel.clone(),
                                                 &[ident.clone()]);
            if let Some(PackageItem::TypeDefinition(typedef)) = resolved_import {
                Some(typedef)
            } else {
                None
            }
        }
    }

    fn resolve_extensions(&self,
                          typedef: TypeDefinitionRef<'ast>,
                          non_types_env: &NonTypesEnvironment<'ast>,
                          extensions: &[QualifiedIdentifier]) {
        for extension in extensions.iter() {
            match self.resolve_object_type(extension, non_types_env) {
                Some(extended_type) => {
                    typedef.borrow_mut().extends.push(extended_type.clone());
                },
                None => {
                    span_error!(extension.span,
                                "class `{}` cannot extend non-type `{}`",
                                typedef.borrow().fq_name,
                                extension);
                },
            }
        }
    }

    fn resolve_implements(&self,
                          typedef: TypeDefinitionRef<'ast>,
                          non_types_env: &NonTypesEnvironment<'ast>,
                          implements: &[QualifiedIdentifier]) {
        for implement in implements.iter() {
            match self.resolve_object_type(implement, non_types_env) {
                Some(implemented_type) => {
                    typedef.borrow_mut().implements.push(implemented_type.clone());
                },
                None => {
                    span_error!(implement.span,
                                "{:?} `{}` cannot implement non-type `{}`",
                                typedef.borrow().kind,
                                typedef.borrow().fq_name,
                                implement);
                },
            }
        }
    }

    fn resolve_type(&self,
                    ty: &ast::Type,
                    non_types_env: &NonTypesEnvironment<'ast>) -> Type<'ast> {
        match ty.node {
            ast::Type_::SimpleType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type, non_types_env) {
                    Type::SimpleType(ty)
                } else {
                    Type::Unknown
                },
            ast::Type_::ArrayType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type, non_types_env) {
                    Type::ArrayType(ty)
                } else {
                    Type::Unknown
                },
        }
    }

    fn resolve_simple_type(&self,
                           ty: &ast::SimpleType,
                           non_types_env: &NonTypesEnvironment<'ast>)
            -> Option<SimpleType<'ast>> {
        match ty.node {
            ast::SimpleType_::Boolean => Some(SimpleType::Boolean),
            ast::SimpleType_::Int => Some(SimpleType::Int),
            ast::SimpleType_::Short => Some(SimpleType::Short),
            ast::SimpleType_::Char => Some(SimpleType::Char),
            ast::SimpleType_::Byte => Some(SimpleType::Byte),
            ast::SimpleType_::Other(ref qident) =>
                self.resolve_object_type(qident, non_types_env)
                    .and_then(|ty| Some(SimpleType::Other(ty))),
        }
    }

    fn resolve_object_type(&self,
                           qident: &QualifiedIdentifier,
                           non_types_env: &NonTypesEnvironment<'ast>)
            -> Option<TypeDefinitionRef<'ast>> {
        match self.resolve_identifier(qident, non_types_env) {
            Some(SymTableItem::Type(ty)) => {
                Some(ty)
            },
            Some(_) => {
                span_error!(qident.span, "`{}` is not a type", qident);
                None
            },
            None => None,
        }
    }

    fn collect_fields(&self,
                      non_types_env: NonTypesEnvironment<'ast>,
                      fields: &[&ast::Field],
                      typedef: TypeDefinitionRef<'ast>) -> NonTypesEnvironment<'ast> {
        fields.iter().fold(non_types_env, |env, field| {
            let field_ref = typedef.borrow().fields.get(&field.node.name.node).unwrap().clone();
            let field_type = self.resolve_type(&field.node.ty, &env);

            let sym_table_item = SymTableItem::Field(field_ref.clone(), field_type);
            let (new_env, existing) = env.insert(field.node.name.node,
                                                 sym_table_item);

            if let Some(_) = existing {
                // There should only be fields in the symbol table at the moment.
                span_error!(field.node.name.span,
                            "field {} already exists",
                            field.node.name);

                // TODO: Add note about where the previous declaration is?
            }

            new_env
        })
    }

    // Tries to resolve a non-fully qualified identifier to a symbol table
    // item. Spans an error upon failure.
    fn resolve_identifier(&self, qident: &QualifiedIdentifier,
                          non_types_env: &NonTypesEnvironment<'ast>)
            -> Option<SymTableItem<'ast>> {
        // For qident = a.b.c.d...
        let first = qident.node.parts.first().unwrap();
        let rest = qident.node.parts.tail();

        if let Some(item) = non_types_env.get(&first.node) {
            // a is local variable or parameter or static import
            self.resolve_expression_identifier(rest, item)
        } else if let Some(item) = self.types.get(&first.node) {
            // a is a type
            self.resolve_type_identifier(rest, item.clone())
        } else if let Some(item) = self.toplevel.borrow().contents.get(&first.node) {
            // a is a package
            if let &PackageItem::Package(ref package) = item {
                self.resolve_package_identifier(rest, package.clone())
            } else {
                span_error!(first.span,
                            "resolving unimported type `{}`",
                            first.node);
                None
            }
        } else {
            span_error!(first.span,
                        "unable to resolve identifier `{}` in `{}`",
                        first.node,
                        qident);
            None
        }
    }

    fn resolve_expression_identifier(&self, qident: &[Ident],
                                     expression: &SymTableItem<'ast>)
            -> Option<SymTableItem<'ast>> {
        match qident {
            // Just this expression
            [] => Some(expression.clone()),
            [ref first, rest..] => {
                // TODO
                Some(expression.clone())
            }
        }
    }

    fn resolve_type_identifier(&self, qident: &[Ident],
                               typedef: TypeDefinitionRef<'ast>)
            -> Option<SymTableItem<'ast>> {
        match qident {
            // Just this type
            [] => Some(SymTableItem::Type(typedef.clone())),
            // Look in the type's members
            [ref first, rest..] => {
                if let Some(field) = typedef.borrow().fields.get(&first.node) {
                    // TODO: What should the type be here?
                    let item = SymTableItem::Field(field.clone(), Type::Unknown);
                    self.resolve_expression_identifier(rest, &item)
                } else if let Some(method) = typedef.borrow().methods.get(&first.node) {
                    let item = SymTableItem::Method(method.clone());
                    self.resolve_expression_identifier(rest, &item)
                } else {
                    span_error!(first.span,
                                "member `{}` not found on type `{}`",
                                first,
                                typedef.borrow().fq_name);
                    None
                }
            }
        }
    }

    fn resolve_package_identifier(&self, qident: &[Ident],
                                  package: PackageRef<'ast>)
            -> Option<SymTableItem<'ast>> {
        // This function should not be called on an empty qident, because
        // it would imply that we are returning a package, which is not
        // a valid name resolution. Caller functions should catch this case.
        let first = qident.first().unwrap();

        match package.borrow().contents.get(&first.node) {
            Some(&PackageItem::Package(ref found_package)) => {
                if qident.len() == 1 {
                    // Can't resolve to a package.
                    span_error!(first.span,
                                "package {} is not a valid name resolution",
                                found_package.borrow().fq_name);
                    None
                } else {
                    self.resolve_package_identifier(qident.tail(),
                                                    found_package.clone())
                }
            },
            Some(&PackageItem::TypeDefinition(ref typedef)) => {
                self.resolve_type_identifier(qident.tail(), typedef.clone())
            },
            None => {
                span_error!(first.span,
                            "type or package `{}` not found in package `{}`",
                            first,
                            package.borrow().fq_name);
                None
            }
        }
    }
}

fn insert_declared_type<'ast>(env: &TypesEnvironment<'ast>,
                              ident: &Ident,
                              typedef: TypeDefinitionRef<'ast>) -> TypesEnvironment<'ast> {

    let (new_env, previous) = env.insert(ident.node, typedef);
    if let Some(&(_, ref previous_item)) = previous {
        // TODO: Shouldn't continue after this error - how to do that?
        span_error!(ident.span,
                    "type `{}` declared in this file conflicts with import `{}`",
                    ident,
                    previous_item.borrow().fq_name);
    }
    new_env
}

// Returns the PackageItem corresponding to a path of identifiers a.b.c
pub fn resolve_import<'ast>(package: PackageRef<'ast>, path: &[Ident])
        -> Option<PackageItem<'ast>> {
    match path {
        // Base case
        [] => Some(PackageItem::Package(package)),
        [ref first, rest..] => match package.borrow().contents.get(&first.node) {
            Some(&PackageItem::Package(ref found_package)) => {
                resolve_import(found_package.clone(), rest)
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

fn insert_type_import<'ast>(symbol: &Symbol,
                            typedef: &TypeDefinitionRef<'ast>,
                            imported: &QualifiedIdentifier,
                            current_env: TypesEnvironment<'ast>)
        -> TypesEnvironment<'ast> {
    let (new_env, previous_opt) = current_env.insert(symbol.clone(),
                                                     typedef.clone());
    if let Some(previous) = previous_opt {
        if previous.1.borrow().fq_name != typedef.borrow().fq_name {
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
    if let Some(PackageItem::TypeDefinition(typedef)) = resolved_import {
        insert_type_import(&imported.node.parts.last().unwrap().node,
                           &typedef,
                           imported,
                           current_env)
    } else if let Some(PackageItem::Package(_)) = resolved_import {
        // $(7.5.1) : Note that an import statement cannot import a subpackage,
        //            only a type.
        span_error!(imported.span,
                    "cannot import subpackages (trying to import `{}` here)",
                    imported);
        current_env
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
                // ($7.5.2) : A type-import-on-demand declaration allows all
                //            accessible types declared in a type or package
                //            to be imported as needed.
                if let &PackageItem::TypeDefinition(ref typedef) = package_item {
                    insert_type_import(symbol,
                                       typedef,
                                       imported,
                                       env)
                } else {
                    env
                }
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

        // TODO: For testing - remove later.
        println!("{} types environment: {:?}", ast.types[0].name(), types_env);

        EnvironmentStack {
            types: types_env,
            non_types: vec![RbMap::new()],
            toplevel: toplevel.clone(),
            package: ast.package.clone(),
        }.walk_compilation_unit(ast);
    }
}

pub fn name_resolve(asts: &[ast::CompilationUnit]) {
    let toplevel = Package::new("top level".to_owned());
    fully_qualify_names(toplevel.clone(), asts);
    build_environments(toplevel.clone(), asts);

    // TODO: For testing - remove when name resolution is finished.
    PackageItem::Package(toplevel).print_light();
}
