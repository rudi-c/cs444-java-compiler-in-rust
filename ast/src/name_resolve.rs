use ast;
use name::*;
use walker::*;
use span::Span;

use rbtree::RbMap;

use std::borrow::ToOwned;
use std::collections::{hash_map, HashMap, HashSet};
use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
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
    ast: &'ast ast::Method,
}
pub type MethodRef<'ast> = Rc<RefCell<Method<'ast>>>;
pub type MethodWeak<'ast> = Weak<RefCell<Method<'ast>>>;

impl<'ast> Method<'ast> {
    pub fn new(name: String, ast: &'ast ast::Method) -> MethodRef<'ast> {
        rc_cell(Method {
            fq_name: Name::fresh(name),
            ast: ast,
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

    // Method overloads can have the same name.
    methods: HashMap<Symbol, Vec<MethodRef<'ast>>>,

    extends: Vec<TypeDefinitionWeak<'ast>>,
    implements: Vec<TypeDefinitionWeak<'ast>>,

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

impl<'ast> PartialEq for TypeDefinition<'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.fq_name.eq(&other.fq_name)
    }
}

impl<'ast> Eq for TypeDefinition<'ast> {}

impl<'ast> PartialOrd for TypeDefinition<'ast> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.fq_name.partial_cmp(&other.fq_name)
    }
}

impl<'ast> Ord for TypeDefinition<'ast> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.fq_name.cmp(&other.fq_name)
    }
}

#[derive(Show, Clone, PartialEq, Eq, PartialOrd, Ord)]
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
    Other(TypeDefinitionWeak<'ast>),
}

impl<'ast> PartialEq for SimpleType<'ast> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&SimpleType::Boolean, &SimpleType::Boolean) => true,
            (&SimpleType::Int, &SimpleType::Int) => true,
            (&SimpleType::Short, &SimpleType::Short) => true,
            (&SimpleType::Char, &SimpleType::Char) => true,
            (&SimpleType::Byte, &SimpleType::Byte) => true,
            (&SimpleType::Other(ref typedef1), &SimpleType::Other(ref typedef2)) =>
                typedef1.upgrade().unwrap().borrow().eq(
                    &*typedef2.upgrade().unwrap().borrow()),
            _ => false,
        }
    }
}

impl<'ast> Eq for SimpleType<'ast> {}

impl<'ast> PartialOrd for SimpleType<'ast> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        fn int_val<'ast>(ty: &SimpleType<'ast>) -> i32 {
            match ty {
                &SimpleType::Boolean => 0,
                &SimpleType::Int => 1,
                &SimpleType::Short => 2,
                &SimpleType::Char => 3,
                &SimpleType::Byte => 4,
                _ => panic!("should not be here"),
            }
        }
        match (self, other) {
            (&SimpleType::Other(ref typedef1), &SimpleType::Other(ref typedef2)) =>
                typedef1.upgrade().unwrap().borrow().partial_cmp(
                    &*typedef2.upgrade().unwrap().borrow()),
            (&SimpleType::Other(_), _) => Some(Ordering::Greater),
            (_, &SimpleType::Other(_)) => Some(Ordering::Less),
           (type1, type2) => int_val(type1).partial_cmp(&int_val(type2)),
        }
    }
}

impl<'ast> Ord for SimpleType<'ast> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
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
        // Note: Implementation is shared with interface methods

        let mut tydef = self.type_definition.as_ref().unwrap().borrow_mut();
        let overloads = tydef.methods.entry(method_ast.node.name.node).get().unwrap_or_else(|v| {
            // First time seeing this method name.
            v.insert(vec![])
        });

        let method_name = format!("{}.{}", Qualified(self.scope.iter()), method_ast.node.name);
        overloads.push(Method::new(method_name, method_ast));

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
fn resolve_create_package<'ast>(toplevel: PackageRef<'ast>, id: &[Ident]) -> PackageRef<'ast> {
    id.iter().enumerate().fold(toplevel, |package, (ix, ident)| {
        let new = |:| Package::new(Qualified(id[0..ix+1].iter()).to_string());
        match package.borrow_mut().contents.entry(ident.node) {
            hash_map::Entry::Occupied(mut v) => {
                let slot = v.get_mut();
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

// Look up a package by its fully-qualified name.
fn resolve_package<'ast>(toplevel: PackageRef<'ast>, id: &[Ident]) -> Option<PackageRef<'ast>> {
    let mut package = toplevel;
    for (ix, ident) in id.iter().enumerate() {
        package = match package.borrow().contents.get(&ident.node) {
            Some(&PackageItem::Package(ref it)) => {
                it.clone() // Found it
            }
            Some(&PackageItem::TypeDefinition(..)) => {
                // There was a type instead!
                span_error!(Span::range(&id[0], &id[ix]),
                            "no such package `{}`; found a type instead",
                            Qualified(id[0..ix+1].iter()));
                return None
            }
            None => {
                span_error!(Span::range(&id[0], &id[ix]),
                            "no such package `{}`",
                            Qualified(id[0..ix+1].iter()));
                return None
            }
        };
    }
    Some(package)
}

// Phase 1.
fn collect_types<'ast>(toplevel: PackageRef<'ast>,
                       asts: &'ast [ast::CompilationUnit]) {
    for ast in asts.iter() {
        let (package, scope) = if let Some(ref package_identifier) = ast.package {
            (resolve_create_package(toplevel.clone(), &*package_identifier.node.parts),
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
enum Referent<'ast> {
    Type(TypeDefinitionRef<'ast>),
    Variable(Variable<'ast>),
    Method(MethodRef<'ast>),
}

#[derive(Show, Clone)]
enum Variable<'ast> {
    LocalVariable,
    Field(FieldRef<'ast>, Type<'ast>),
}


type TypesEnvironment<'ast> = RbMap<Symbol, TypeDefinitionRef<'ast>>;
type VariablesEnvironment<'ast> = RbMap<Symbol, Variable<'ast>>;
type MethodsEnvironment<'ast> = RbMap<(Symbol, Vec<Type<'ast>>),
                                      (MethodRef<'ast>, Option<Type<'ast>>)>;

#[derive(Show, Clone)]
struct EnvironmentStack<'ast> {
    // Since there are no nested classes and only one type per file,
    // this is not a stack/vector and we just use mutation to add the
    // single class/interface in the file to the initial environment
    // containing imported types.
    types: TypesEnvironment<'ast>,

    // Similarly, no stack for methods.
    methods: MethodsEnvironment<'ast>,

    variables: Vec<VariablesEnvironment<'ast>>,
    toplevel: PackageRef<'ast>,
    package: Option<QualifiedIdentifier>,

    // Search here for more types.
    on_demand_packages: Vec<PackageRef<'ast>>
}

type TypeEnvironmentPair<'ast> = (TypeDefinitionRef<'ast>, EnvironmentStack<'ast>);

impl<'ast> Walker<'ast> for EnvironmentStack<'ast> {
    fn walk_class(&mut self, class: &'ast ast::Class) {
        let ref class_name = class.node.name;
        let typedef = self.get_type_declaration(class_name).unwrap();
        let mut vars_env = self.variables.last().unwrap().clone();

        // Add the class itself to the environment.
        self.types = insert_declared_type(&self.types, class_name, typedef.clone());

        // Process class body.
        vars_env = self.collect_fields(vars_env, typedef.clone());
        self.methods = self.collect_methods(typedef.clone());
    }

    fn walk_interface(&mut self, interface: &'ast ast::Interface) {
        let ref interface_name = interface.node.name;
        let typedef = self.get_type_declaration(interface_name).unwrap();
        let mut vars_env = self.variables.last().unwrap().clone();

        // Add the interface itself to the environment.
        self.types = insert_declared_type(&self.types, interface_name, typedef.clone());

        // Process interface body.
        self.methods = self.collect_methods(typedef.clone());
    }
}

impl<'ast> EnvironmentStack<'ast> {
    fn get_type_declaration(&self, ident: &Ident) -> Option<TypeDefinitionRef<'ast>> {
        let p = if let Some(ref package) = self.package {
            self.resolve_package_name(&*package.node.parts).unwrap()
        } else {
            // Top level.
            self.toplevel.clone()
        };
        // FIXME: This is kinda bad
        if let Some(&PackageItem::TypeDefinition(ref typedef)) =
                p.borrow().contents.get(&ident.node) {
            Some(typedef.clone())
        } else {
            None
        }
    }

    // Resolve extends and implements. This is done separately from walking
    // the AST because we need to process the compilations in topological
    // order (with respect to inheritance).
    //
    // Returns the TypeDefinition that has just been resolved.
    fn resolve_inheritance(&self, typedcl: &ast::TypeDeclaration)
            -> TypeDefinitionRef<'ast> {
        match typedcl.node {
            ast::TypeDeclaration_::Class(ref class) => {
                let ref class_name = class.node.name;
                let typedef = self.get_type_declaration(class_name).unwrap();

                if let Some(ref extension) = class.node.extends {
                    self.resolve_extensions(typedef.clone(),
                                            &[extension.clone()]);
                }
                self.resolve_implements(typedef.clone(),
                                        class.node.implements.as_slice());
                typedef.clone()
            },
            ast::TypeDeclaration_::Interface(ref interface) => {
                let ref interface_name = interface.node.name;
                let typedef = self.get_type_declaration(interface_name).unwrap();

                self.resolve_extensions(typedef.clone(),
                                        interface.node.extends.as_slice());
                typedef.clone()
            },
        }
    }

    fn resolve_extensions(&self,
                          typedef: TypeDefinitionRef<'ast>,
                          extensions: &[QualifiedIdentifier]) {
        for extension in extensions.iter() {
            match self.resolve_type_name(&*extension.node.parts) {
                Some(extended_type) => {
                    typedef.borrow_mut().extends.push(extended_type.downgrade());
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    fn resolve_implements(&self,
                          typedef: TypeDefinitionRef<'ast>,
                          implements: &[QualifiedIdentifier]) {
        for implement in implements.iter() {
            match self.resolve_type_name(&*implement.node.parts) {
                Some(implemented_type) => {
                    typedef.borrow_mut().implements.push(implemented_type.downgrade());
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    fn resolve_type(&self, ty: &ast::Type) -> Type<'ast> {
        match ty.node {
            ast::Type_::SimpleType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type) {
                    Type::SimpleType(ty)
                } else {
                    Type::Unknown
                },
            ast::Type_::ArrayType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type) {
                    Type::ArrayType(ty)
                } else {
                    Type::Unknown
                },
        }
    }

    fn resolve_simple_type(&self,
                           ty: &ast::SimpleType)
            -> Option<SimpleType<'ast>> {
        match ty.node {
            ast::SimpleType_::Boolean => Some(SimpleType::Boolean),
            ast::SimpleType_::Int => Some(SimpleType::Int),
            ast::SimpleType_::Short => Some(SimpleType::Short),
            ast::SimpleType_::Char => Some(SimpleType::Char),
            ast::SimpleType_::Byte => Some(SimpleType::Byte),
            ast::SimpleType_::Other(ref qident) =>
                self.resolve_type_name(&*qident.node.parts)
                    .map(|ty| SimpleType::Other(ty.downgrade())),
        }
    }

    fn collect_fields(&self,
                      vars_env: VariablesEnvironment<'ast>,
                      typedef: TypeDefinitionRef<'ast>) -> VariablesEnvironment<'ast> {
        typedef.borrow()
               .fields
               .values()
               .fold(vars_env, |env, field| {
            let ref field_ast = field.borrow().ast.node;
            let field_ref = typedef.borrow().fields.get(&field_ast.name.node).unwrap().clone();
            let field_type = self.resolve_type(&field_ast.ty);

            let variable = Variable::Field(field_ref.clone(), field_type);
            let (new_env, existing) = env.insert(field_ast.name.node,
                                                 variable);

            if let Some(_) = existing {
                // There should only be fields in the symbol table at the moment.
                span_error!(field_ast.name.span,
                            "field {} already exists",
                            field_ast.name);

                // TODO: Add note about where the previous declaration is?
            }

            new_env
        })
    }

    // Add the methods of the type to the environment.
    fn collect_methods(&mut self, typedef: TypeDefinitionRef<'ast>)
            -> MethodsEnvironment<'ast> {
        let mut methods_env = self.methods.clone();

        for (name, overloads) in typedef.borrow().methods.iter() {
            for method in overloads.iter() {
                let ref method_ast = method.borrow().ast.node;
                let return_type = method_ast.return_type.as_ref()
                                            .map(|ty| self.resolve_type(ty));
                let argument_types: Vec<Type> =
                    method_ast.params.iter()
                              .map(|param| self.resolve_type(&param.node.ty))
                              .collect();

                let (new_env, existing) =
                    self.methods.insert((name.clone(), argument_types),
                                        (method.clone(), return_type));
                methods_env = new_env;
            }
        }

        methods_env
    }

    // Look up a package by its fully qualified name.
    // Emits an error on failure.
    fn resolve_package_name(&self, id: &[Ident]) -> Option<PackageRef<'ast>> {
        assert!(!id.is_empty());
        resolve_package(self.toplevel.clone(), id)
    }

    // Look up a (user-defined) type by either a qualified or simple name.
    // Emits an error on failure.
    fn resolve_type_name(&self, id: &[Ident]) -> Option<TypeDefinitionRef<'ast>> {
        match id {
            [] => panic!("bug: tried to resolve an empty type name"),
            // simple name
            [ref ident] => match self.find_type(ident) {
                Some(tydef) => Some(tydef.clone()),
                None => {
                    span_error!(ident.span, "unresolved type name");
                    None
                }
            },
            // fully-qualified name
            // in Joos, `init` must refer to a package
            [init.., ref last] => self.resolve_package_name(init).and_then(|package| {
                match package.borrow().contents.get(&last.node) {
                    Some(&PackageItem::TypeDefinition(ref tydef)) => Some(tydef.clone()),
                    _ => {
                        span_error!(Span::range(&init[0], last),
                                    "no such type `{}` in package `{}`",
                                    last, Qualified(init.iter()));
                        None
                    }
                }
            })
        }
    }

    /*
    // Tries to resolve a non-fully qualified identifier to a symbol table
    // item. Spans an error upon failure.
    fn resolve_identifier(&self, qident: &QualifiedIdentifier,
                          vars_env: &VariablesEnvironment<'ast>)
            -> Option<Referent<'ast>> {
        // For qident = a.b.c.d...
        let first = qident.node.parts.first().unwrap();
        let rest = qident.node.parts.tail();

        if let Some(item) = vars_env.get(&first.node) {
            // a is local variable or parameter or static import
            self.resolve_expression_identifier(rest, &Referent::Variable(item.clone()))
        } else if let Some(item) = self.find_type(first) {
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
                                     expression: &Referent<'ast>)
            -> Option<Referent<'ast>> {
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
            -> Option<Referent<'ast>> {
        match qident {
            // Just this type
            [] => Some(Referent::Type(typedef.clone())),
            // Look in the type's members
            [ref first, rest..] => {
                if let Some(field) = typedef.borrow().fields.get(&first.node) {
                    // TODO: What should the type be here?
                    let item = Referent::Variable(Variable::Field(field.clone(), Type::Unknown));
                    self.resolve_expression_identifier(rest, &item)
                } else if let Some(method) = typedef.borrow().methods.get(&first.node) {
                    let item = Referent::Method(method.clone());
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
            -> Option<Referent<'ast>> {
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
    */

    // Look up a type by simple name, using the current environment.
    // TODO: Clean up the error story here (don't want to emit multiple errors)
    fn find_type(&self, ty: &Ident) -> Option<TypeDefinitionRef<'ast>> {
        self.types.get(&ty.node)
                  .cloned()
                  .or_else(|| {
            // If the type is not in the current environment, we can look in the
            // on-demand import packages.
            // It's necessary to look through every package to check for
            // ambiguities.
            println!("here for {}", ty);
            let mut found_type: Option<TypeDefinitionRef<'ast>> = None;
            for package in self.on_demand_packages.iter() {
                println!("try {}", package.borrow().fq_name);
                match package.borrow().contents.get(&ty.node) {
                    Some(&PackageItem::TypeDefinition(ref typedef)) => {
                        println!("found {}", typedef.borrow().fq_name);
                        // If we already have a type, then there's an ambiguity.
                        if let Some(existing) = found_type {
                            span_error!(ty.span,
                                        "ambiguous type name `{}`: could refer to `{}` or `{}`",
                                        ty,
                                        typedef.borrow().fq_name,
                                        existing.borrow().fq_name);
                            found_type = None;
                            break;
                        } else {
                            found_type = Some(typedef.clone());
                        }
                    },
                    // Ignore subpackages.
                    Some(_) => {},
                    None => {},
                }
            }
            found_type
        })
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
    match &*imported.node.parts {
        [] => panic!("impossible: imported empty type"),
        [ref id] => {
            span_error!(id.span,
                        "imported type name must fully qualified");
            current_env
        },
        // FIXME: Deduplicate this code with `resolve_type_name`
        // (factor into `resole_fq_type_name` or something)
        [init.., ref last] => match resolve_package(toplevel.clone(), init) {
            None => current_env,
            Some(package) => match package.borrow().contents.get(&last.node) {
                Some(&PackageItem::TypeDefinition(ref tydef)) => {
                    insert_type_import(&last.node, tydef, imported, current_env)
                }
                _ => {
                    span_error!(Span::range(&init[0], last),
                                "no such type `{}` in package `{}`",
                                last, Qualified(init.iter()));
                    current_env
                }
            }
        },
    }
}

fn import_on_demand<'ast>(imported: &QualifiedIdentifier,
                          toplevel: PackageRef<'ast>,
                          on_demand_packages: &mut Vec<PackageRef<'ast>>) {
    if let Some(package) = resolve_package(toplevel, &*imported.node.parts) {
        on_demand_packages.push(package);
    }
}

fn inheritance_topological_sort_search<'ast>(typedef: TypeDefinitionRef<'ast>,
                                             seen: &mut HashSet<Name>,
                                             visited: &mut HashSet<Name>,
                                             stack: &mut Vec<Name>,
                                             sorted: &mut Vec<Name>)
        -> Result<(), ()> {
    let borrow = typedef.borrow();
    let mut parents = borrow.extends.iter().chain(borrow.implements.iter());

    stack.push(borrow.fq_name.clone());

    if !seen.insert(borrow.fq_name.clone()) {
        span_error!(borrow.ast.span,
                    "found an inheritance cycle: {:?}",
                    stack);
        return Err(());
    }

    for parent in parents {
        if !visited.contains(&parent.upgrade().unwrap().borrow().fq_name) {
            try!(inheritance_topological_sort_search(parent.upgrade().unwrap(), seen,
                                                     visited, stack, sorted));
        }
    }

    sorted.push(borrow.fq_name.clone());
    visited.insert(borrow.fq_name.clone());
    stack.pop();
    Ok(())
}

fn inheritance_topological_sort<'ast>(preprocessed_types: &[TypeEnvironmentPair<'ast>])
        -> Option<Vec<TypeEnvironmentPair<'ast>>> {

    // To find items in processed_types by fully-qualified names.
    let mut lookup = HashMap::new();
    for &(ref typedef, ref env) in preprocessed_types.iter() {
        lookup.insert(typedef.borrow().fq_name.clone(), (typedef.clone(), env.clone()));
    }

    let mut sorted: Vec<Name> = vec![];

    {
        let mut seen: HashSet<Name> = HashSet::new();
        let mut visited: HashSet<Name> = HashSet::new();

        // Keep track of the depth-first search stack for error message
        // purposes (it shows the user where the cycle is).
        let mut stack: Vec<Name> = vec![];

        for &(ref typedef, _) in preprocessed_types.iter() {
            if !visited.contains(&typedef.borrow().fq_name) {
                let result = inheritance_topological_sort_search(
                    typedef.clone(), &mut seen, &mut visited,
                    &mut stack, &mut sorted);
                if let Err(_) = result {
                    return None;
                }
            }
        }
    }

    Some(sorted.iter().map(|name| lookup.get(name).unwrap().clone()).collect())
    // Some(sorted.iter().map(|name| match lookup.get(name).unwrap() {
    //     &(&typedef, &env) => (typedef.clone(), env.clone())
    // }).collect())
}

fn build_environments<'ast>(toplevel: PackageRef<'ast>,
                            asts: &'ast [ast::CompilationUnit]) {
    let mut preprocessed_types = vec![];

    for ast in asts.iter() {
        let mut types_env: TypesEnvironment<'ast> = RbMap::new();

        let mut on_demand_packages = vec![];

        // Add all imports to initial environment for this compilation unit.
        for import in ast.imports.iter() {
            match import.node {
                ast::ImportDeclaration_::SingleType(ref qident) => {
                    types_env = import_single_type(qident,
                                                   toplevel.clone(),
                                                   types_env);
                },
                ast::ImportDeclaration_::OnDemand(ref qident) => {
                    import_on_demand(qident,
                                     toplevel.clone(),
                                     &mut on_demand_packages);
                },
            }
        }

        // TODO: For testing - remove later.
        println!("{} types environment: {:?}", ast.types[0].name(), types_env);

        let env_stack = EnvironmentStack {
            types: types_env,
            methods: RbMap::new(),
            variables: vec![RbMap::new()],
            toplevel: toplevel.clone(),
            package: ast.package.clone(),
            on_demand_packages: on_demand_packages,
        };

        let typedef = env_stack.resolve_inheritance(&ast.types[0]);
        preprocessed_types.push((typedef, env_stack));
    }

    if let Some(sorted) = inheritance_topological_sort(preprocessed_types.as_slice()) {
        preprocessed_types = sorted;
    } else {
        return;
    }

    for &(ref typedef, ref env) in preprocessed_types.iter() {
        env.clone().walk_type_declaration(typedef.borrow().ast);
    }
}

#[derive(Show)]
pub struct VariableDefinition<'ast> {
    fq_name: Name,
    ty: Type<'ast>,
    ast: &'ast ast::VariableDeclaration,
}
pub type VariableDefinitionRef<'ast> = Rc<RefCell<VariableDefinition<'ast>>>;
pub type VariableDefinitionWeak<'ast> = Weak<RefCell<VariableDefinition<'ast>>>;

pub fn name_resolve<'ast>(asts: &'ast [ast::CompilationUnit]) -> PackageRef<'ast> {
    let toplevel = Package::new("top level".to_owned());
    collect_types(toplevel.clone(), asts);
    build_environments(toplevel.clone(), asts);

    // TODO: For testing - remove when name resolution is finished.
    PackageItem::Package(toplevel.clone()).print_light();
    println!("{:?}", toplevel);
    toplevel
}
