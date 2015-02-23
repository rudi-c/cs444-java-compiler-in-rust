use ast;
use name::*;
use walker::*;
use span::Span;

use name_resolve_structs::*;
use collect_types::collect_types;

use rbtree::RbMap;

use std::borrow::ToOwned;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::{Rc, Weak};

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
                p.contents.borrow().get(&ident.node) {
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
                    typedef.extends.borrow_mut().push(extended_type.downgrade());
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
                    typedef.implements.borrow_mut().push(implemented_type.downgrade());
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
        typedef.fields
               .borrow()
               .values()
               .fold(vars_env, |env, field| {
            let ref field_ast = field.ast.node;
            let field_ref = typedef.fields.borrow().get(&field_ast.name.node).unwrap().clone();
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

        for (name, overloads) in typedef.methods.borrow().iter() {
            for method in overloads.iter() {
                let ref method_ast = method.ast.node;
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
                match package.contents.borrow().get(&last.node) {
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
        } else if let Some(item) = self.toplevel.contents.borrow().get(&first.node) {
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
                if let Some(field) = typedef.fields.borrow().get(&first.node) {
                    // TODO: What should the type be here?
                    let item = Referent::Variable(Variable::Field(field.clone(), Type::Unknown));
                    self.resolve_expression_identifier(rest, &item)
                } else if let Some(method) = typedef.methods.borrow().get(&first.node) {
                    let item = Referent::Method(method.clone());
                    self.resolve_expression_identifier(rest, &item)
                } else {
                    span_error!(first.span,
                                "member `{}` not found on type `{}`",
                                first,
                                typedef.fq_name);
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

        match package.contents.borrow().get(&first.node) {
            Some(&PackageItem::Package(ref found_package)) => {
                if qident.len() == 1 {
                    // Can't resolve to a package.
                    span_error!(first.span,
                                "package {} is not a valid name resolution",
                                found_package.fq_name);
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
                            package.fq_name);
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
                println!("try {}", package.fq_name);
                match package.contents.borrow().get(&ty.node) {
                    Some(&PackageItem::TypeDefinition(ref typedef)) => {
                        println!("found {}", typedef.fq_name);
                        // If we already have a type, then there's an ambiguity.
                        if let Some(existing) = found_type {
                            span_error!(ty.span,
                                        "ambiguous type name `{}`: could refer to `{}` or `{}`",
                                        ty,
                                        typedef.fq_name,
                                        existing.fq_name);
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

// Look up a package by its fully-qualified name.
fn resolve_package<'ast>(toplevel: PackageRef<'ast>, id: &[Ident]) -> Option<PackageRef<'ast>> {
    let mut package = toplevel;
    for (ix, ident) in id.iter().enumerate() {
        package = match package.contents.borrow().get(&ident.node) {
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

fn insert_declared_type<'ast>(env: &TypesEnvironment<'ast>,
                              ident: &Ident,
                              typedef: TypeDefinitionRef<'ast>) -> TypesEnvironment<'ast> {

    let (new_env, previous) = env.insert(ident.node, typedef);
    if let Some(&(_, ref previous_item)) = previous {
        // TODO: Shouldn't continue after this error - how to do that?
        span_error!(ident.span,
                    "type `{}` declared in this file conflicts with import `{}`",
                    ident,
                    previous_item.fq_name);
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
        if previous.1.fq_name != typedef.fq_name {
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
            Some(package) => match package.contents.borrow().get(&last.node) {
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
    let extends_borrow = typedef.extends.borrow();
    let implements_borrow = typedef.implements.borrow();
    let mut parents = extends_borrow.iter().chain(implements_borrow.iter());

    stack.push(typedef.fq_name);

    if !seen.insert(typedef.fq_name) {
        span_error!(typedef.ast.span,
                    "found an inheritance cycle: {:?}",
                    stack);
        return Err(());
    }

    for parent in parents {
        if !visited.contains(&parent.upgrade().unwrap().fq_name) {
            try!(inheritance_topological_sort_search(parent.upgrade().unwrap(), seen,
                                                     visited, stack, sorted));
        }
    }

    sorted.push(typedef.fq_name);
    visited.insert(typedef.fq_name);
    stack.pop();
    Ok(())
}

fn inheritance_topological_sort<'ast>(preprocessed_types: &[TypeEnvironmentPair<'ast>])
        -> Option<Vec<TypeEnvironmentPair<'ast>>> {

    // To find items in processed_types by fully-qualified names.
    let mut lookup = HashMap::new();
    for &(ref typedef, ref env) in preprocessed_types.iter() {
        lookup.insert(typedef.fq_name.clone(), (typedef.clone(), env.clone()));
    }

    let mut sorted: Vec<Name> = vec![];

    {
        let mut seen: HashSet<Name> = HashSet::new();
        let mut visited: HashSet<Name> = HashSet::new();

        // Keep track of the depth-first search stack for error message
        // purposes (it shows the user where the cycle is).
        let mut stack: Vec<Name> = vec![];

        for &(ref typedef, _) in preprocessed_types.iter() {
            if !visited.contains(&typedef.fq_name) {
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
        env.clone().walk_type_declaration(typedef.ast);
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

