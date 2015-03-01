use ast;
use name::*;
use walker::*;
use span::Span;

use name_resolve_structs::*;
use collect_types::collect_types;
use arena::Arena;

use rbtree::RbMap;

use std::borrow::ToOwned;
use std::collections::{HashMap, HashSet};

#[derive(Show, Clone)]
enum Referent<'a, 'ast: 'a> {
    Type(TypeDefinitionRef<'a, 'ast>),
    Variable(Variable<'a, 'ast>),
    Method(MethodRef<'a, 'ast>),
}

#[derive(Show, Clone)]
enum Variable<'a, 'ast: 'a> {
    LocalVariable,
    Field(FieldRef<'a, 'ast>, Type<'a, 'ast>),
}


type TypesEnvironment<'a, 'ast> = RbMap<Symbol, TypeDefinitionRef<'a, 'ast>>;
type VariablesEnvironment<'a, 'ast> = RbMap<Symbol, Variable<'a, 'ast>>;

#[derive(Show, Clone)]
struct EnvironmentStack<'a, 'ast: 'a> {
    // Since there are no nested classes and only one type per file,
    // this is not a stack/vector and we just use mutation to add the
    // single class/interface in the file to the initial environment
    // containing imported types.
    types: TypesEnvironment<'a, 'ast>,

    variables: Vec<VariablesEnvironment<'a, 'ast>>,
    toplevel: PackageRef<'a, 'ast>,
    package: Option<QualifiedIdentifier>,

    // Search here for more types.
    on_demand_packages: Vec<PackageRef<'a, 'ast>>
}

type TypeEnvironmentPair<'a, 'ast> = (TypeDefinitionRef<'a, 'ast>, EnvironmentStack<'a, 'ast>);

impl<'a, 'ast> Walker<'ast> for EnvironmentStack<'a, 'ast> {
    fn walk_class(&mut self, class: &'ast ast::Class) {
        let ref class_name = class.node.name;
        let typedef = self.get_type_declaration(class_name).unwrap();
        let mut vars_env = self.variables.last().unwrap().clone();

        // Add the class itself to the environment.
        self.types = insert_declared_type(&self.types, class_name, typedef);

        // Process class body.
        vars_env = self.collect_fields(vars_env, typedef);
        *typedef.all_methods.borrow_mut() = self.collect_class_methods(typedef);
    }

    fn walk_interface(&mut self, interface: &'ast ast::Interface) {
        let ref interface_name = interface.node.name;
        let typedef = self.get_type_declaration(interface_name).unwrap();
        let mut vars_env = self.variables.last().unwrap().clone();

        // Add the interface itself to the environment.
        self.types = insert_declared_type(&self.types, interface_name, typedef);

        // Process interface body.
        *typedef.all_methods.borrow_mut() = self.collect_interface_methods(typedef);
    }
}

impl<'a, 'ast> EnvironmentStack<'a, 'ast> {
    fn get_type_declaration(&self, ident: &Ident) -> Option<TypeDefinitionRef<'a, 'ast>> {
        let p = if let Some(ref package) = self.package {
            self.resolve_package_name(&*package.node.parts).unwrap()
        } else {
            // Top level.
            self.toplevel
        };
        // FIXME: This is kinda bad
        if let Some(&PackageItem::TypeDefinition(typedef)) =
                p.contents.borrow().get(&ident.node) {
            Some(typedef)
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
            -> TypeDefinitionRef<'a, 'ast> {
        match typedcl.node {
            ast::TypeDeclaration_::Class(ref class) => {
                let ref class_name = class.node.name;
                let typedef = self.get_type_declaration(class_name).unwrap();

                if let Some(ref extension) = class.node.extends {
                    self.resolve_extensions(typedef,
                                            &[extension.clone()]);
                }
                self.resolve_implements(typedef,
                                        class.node.implements.as_slice());
                typedef
            },
            ast::TypeDeclaration_::Interface(ref interface) => {
                let ref interface_name = interface.node.name;
                let typedef = self.get_type_declaration(interface_name).unwrap();

                self.resolve_extensions(typedef,
                                        interface.node.extends.as_slice());
                typedef
            },
        }
    }

    fn resolve_extensions(&self,
                          typedef: TypeDefinitionRef<'a, 'ast>,
                          extensions: &[QualifiedIdentifier]) {
        for extension in extensions.iter() {
            match self.resolve_type_name(&*extension.node.parts) {
                Some(extended_type) => {
                    typedef.extends.borrow_mut().push(extended_type);
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    fn resolve_implements(&self,
                          typedef: TypeDefinitionRef<'a, 'ast>,
                          implements: &[QualifiedIdentifier]) {
        for implement in implements.iter() {
            match self.resolve_type_name(&*implement.node.parts) {
                Some(implemented_type) => {
                    typedef.implements.borrow_mut().push(implemented_type);
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    fn resolve_type(&self, ty: &ast::Type) -> Type<'a, 'ast> {
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
            -> Option<SimpleType<'a, 'ast>> {
        match ty.node {
            ast::SimpleType_::Boolean => Some(SimpleType::Boolean),
            ast::SimpleType_::Int => Some(SimpleType::Int),
            ast::SimpleType_::Short => Some(SimpleType::Short),
            ast::SimpleType_::Char => Some(SimpleType::Char),
            ast::SimpleType_::Byte => Some(SimpleType::Byte),
            ast::SimpleType_::Other(ref qident) =>
                self.resolve_type_name(&*qident.node.parts)
                    .map(|ty| SimpleType::Other(ty)),
        }
    }

    fn collect_fields(&self,
                      vars_env: VariablesEnvironment<'a, 'ast>,
                      typedef: TypeDefinitionRef<'a, 'ast>) -> VariablesEnvironment<'a, 'ast> {
        typedef.fields
               .borrow()
               .values()
               .fold(vars_env, |env, field| {
            let ref field_ast = field.ast.node;
            let field_ref = *typedef.fields.borrow().get(&field_ast.name.node).unwrap();
            let field_type = self.resolve_type(&field_ast.ty);

            let variable = Variable::Field(field_ref, field_type);
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

    fn collect_class_methods(&self, class: TypeDefinitionRef<'a, 'ast>)
            -> MethodMap<'a, 'ast> {
        let parent_class_methods = match class.extends.borrow().first() {
            Some(parent) => parent.all_methods.borrow().clone(),
            None => RbMap::new()
        };

        let interface_methods = self.collect_parent_interface_methods(class);

        // Add interface's methods to parent class' methods.
        // This may replace a parent class' method. This is fine, because we still
        // expect the class to implement the interface and thus to have this method.
        let parent_methods = interface_methods.iter().fold(parent_class_methods, |methods, method| {
            let &(ref signature, ref method_info) = method;
            self.method_add_check_conflicts(&methods, signature,
                                            method_info, class)
        });

        let own_methods = self.collect_own_methods(class);

        // Merge the classe's own methods with the parent's.
        // The may replace a parent class's method (override) or an interface
        // method (implement).
        let all_methods = own_methods.iter().fold(parent_methods, |methods, method| {
            let &(ref signature, ref method_info) = method;
            self.method_add_check_conflicts(&methods, signature,
                                            method_info, class)
        });

        // TODO: This would be a good place to check that all methods have
        // been implemented (no method with no body that isn't abstract).

        all_methods
    }

    fn collect_interface_methods(&self, interface: TypeDefinitionRef<'a, 'ast>)
            -> MethodMap<'a, 'ast> {
        // Pull in all the parent's methods first.
        let parent_methods = self.collect_parent_interface_methods(interface);

        let own_methods = self.collect_own_methods(interface);

        // Merge interface's own methods with the parent's.
        own_methods.iter().fold(parent_methods, |methods, method| {
            let &(ref signature, ref method_info) = method;
            self.method_add_check_conflicts(&methods, signature,
                                            method_info, interface)
        })
    }

    fn collect_parent_interface_methods(&self, typedef: TypeDefinitionRef<'a, 'ast>)
            -> MethodMap<'a, 'ast> {
        let mut all_methods = RbMap::new();

        let interface_methods = match typedef.kind {
            TypeKind::Class => typedef.implements.borrow(),
            TypeKind::Interface => typedef.extends.borrow(),
        };

        for parent in interface_methods.iter() {
            all_methods = parent.all_methods.borrow().iter()
                                            .fold(all_methods, |methods, method| {
                let &(ref signature, ref method_info) = method;
                self.method_add_check_conflicts(&methods, signature,
                                                method_info, typedef)
            });
        }

        all_methods
    }

    fn collect_own_methods(&self, typedef: TypeDefinitionRef<'a, 'ast>) -> MethodMap<'a, 'ast> {
        let mut own_methods = RbMap::new();
        for (method_name, ref method_group) in typedef.methods.borrow().iter() {
            own_methods = method_group.iter().fold(own_methods, |methods, &method| {
                let types: Vec<Type> =
                    method.ast.node.params.iter()
                          .map(|dcl| self.resolve_type(&dcl.node.ty))
                          .collect();
                let signature = (method_name.clone(), types);
                let method_info = MethodInfo {
                    method: method,
                    source: typedef,
                    return_type: method.ast.node.return_type
                                       .as_ref().map(|ty| self.resolve_type(ty)),
                };

                let (new_methods, existing_method) =
                    methods.insert(signature.clone(), method_info.clone());

                // Check for duplicate method signatures.
                if let Some(_) = existing_method {
                    span_error!(typedef.ast.span,
                                "method conflict: `{}` declared in previously",
                                method_signature_string(&signature));
                }

                new_methods
            });
        }

        own_methods
    }

    // Adds a method to an existing set of methods.
    // Raise an error if there is already a method with the same signature,
    // but different return types, otherwise replace it.
    fn method_add_check_conflicts(&self,
        methods: &MethodMap<'a, 'ast>,
        signature: &MethodSignature<'a, 'ast>,
        method_info: &MethodInfo<'a, 'ast>,
        typedef: TypeDefinitionRef<'a, 'ast>) -> MethodMap<'a, 'ast> {

        let (new_methods, existing_method) =
            methods.insert(signature.clone(), method_info.clone());

        // Check if the interface's method conflicts with another
        // parent interface's method (same signature, different return types).
        if let Some(&(_, ref existing_info)) = existing_method {
            if method_info.return_type != existing_info.return_type {
                span_error!(typedef.ast.span,
                            "method conflict: `{}` declared in `{}` and `{}`",
                            method_signature_string(signature),
                            method_info.source.fq_name,
                            existing_info.source.fq_name);
                span_note!(typedef.ast.span,
                           "the return types {:?} and {:?} are incompatible",
                           type_as_string(&method_info.return_type),
                           type_as_string(&existing_info.return_type));
            }
        }

        new_methods
    }

    // Look up a package by its fully qualified name.
    // Emits an error on failure.
    fn resolve_package_name(&self, id: &[Ident]) -> Option<PackageRef<'a, 'ast>> {
        assert!(!id.is_empty());
        resolve_package(self.toplevel, id)
    }

    // Look up a (user-defined) type by either a qualified or simple name.
    // Emits an error on failure.
    fn resolve_type_name(&self, id: &[Ident]) -> Option<TypeDefinitionRef<'a, 'ast>> {
        match id {
            [] => panic!("bug: tried to resolve an empty type name"),
            // simple name
            [ref ident] => match self.find_type(ident) {
                Some(tydef) => Some(tydef),
                None => {
                    span_error!(ident.span, "unresolved type name");
                    None
                }
            },
            // fully-qualified name
            // in Joos, `init` must refer to a package
            [init.., ref last] => self.resolve_package_name(init).and_then(|package| {
                match package.contents.borrow().get(&last.node) {
                    Some(&PackageItem::TypeDefinition(tydef)) => Some(tydef),
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
                          vars_env: &VariablesEnvironment<'a, 'ast>)
            -> Option<Referent<'a, 'ast>> {
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
                                     expression: &Referent<'a, 'ast>)
            -> Option<Referent<'a, 'ast>> {
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
                               typedef: TypeDefinitionRef<'a, 'ast>)
            -> Option<Referent<'a, 'ast>> {
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
                                  package: PackageRef<'a, 'ast>)
            -> Option<Referent<'a, 'ast>> {
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
    fn find_type(&self, ty: &Ident) -> Option<TypeDefinitionRef<'a, 'ast>> {
        self.types.get(&ty.node)
                  .cloned()
                  .or_else(|| {
            // If the type is not in the current environment, we can look in the
            // on-demand import packages.
            // It's necessary to look through every package to check for
            // ambiguities.
            println!("here for {}", ty);
            let mut found_type: Option<TypeDefinitionRef<'a, 'ast>> = None;
            for package in self.on_demand_packages.iter() {
                println!("try {}", package.fq_name);
                match package.contents.borrow().get(&ty.node) {
                    Some(&PackageItem::TypeDefinition(typedef)) => {
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
                            found_type = Some(typedef);
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
fn resolve_package<'a, 'ast>(toplevel: PackageRef<'a, 'ast>, id: &[Ident]) -> Option<PackageRef<'a, 'ast>> {
    let mut package = toplevel;
    for (ix, ident) in id.iter().enumerate() {
        package = match package.contents.borrow().get(&ident.node) {
            Some(&PackageItem::Package(it)) => {
                it // Found it
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

fn insert_declared_type<'a, 'ast>(env: &TypesEnvironment<'a, 'ast>,
                                  ident: &Ident,
                                  typedef: TypeDefinitionRef<'a, 'ast>) -> TypesEnvironment<'a, 'ast> {

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

fn insert_type_import<'a, 'ast>(symbol: Symbol,
                                typedef: TypeDefinitionRef<'a, 'ast>,
                                imported: &QualifiedIdentifier,
                                current_env: TypesEnvironment<'a, 'ast>)
        -> TypesEnvironment<'a, 'ast> {
    let (new_env, previous_opt) = current_env.insert(symbol, typedef);
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

fn import_single_type<'a, 'ast>(imported: &QualifiedIdentifier,
                                toplevel: PackageRef<'a, 'ast>,
                                current_env: TypesEnvironment<'a, 'ast>)
        -> TypesEnvironment<'a, 'ast> {
    match &*imported.node.parts {
        [] => panic!("impossible: imported empty type"),
        [ref id] => {
            span_error!(id.span,
                        "imported type name must fully qualified");
            current_env
        },
        // FIXME: Deduplicate this code with `resolve_type_name`
        // (factor into `resole_fq_type_name` or something)
        [init.., ref last] => match resolve_package(toplevel, init) {
            None => current_env,
            Some(package) => match package.contents.borrow().get(&last.node) {
                Some(&PackageItem::TypeDefinition(tydef)) => {
                    insert_type_import(last.node, tydef, imported, current_env)
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

fn import_on_demand<'a, 'ast>(imported: &QualifiedIdentifier,
                              toplevel: PackageRef<'a, 'ast>,
                              on_demand_packages: &mut Vec<PackageRef<'a, 'ast>>) {
    if let Some(package) = resolve_package(toplevel, &*imported.node.parts) {
        on_demand_packages.push(package);
    }
}

fn inheritance_topological_sort_search<'a, 'ast>(typedef: TypeDefinitionRef<'a, 'ast>,
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
        if !visited.contains(&parent.fq_name) {
            try!(inheritance_topological_sort_search(*parent, seen,
                                                     visited, stack, sorted));
        }
    }

    sorted.push(typedef.fq_name);
    visited.insert(typedef.fq_name);
    stack.pop();
    Ok(())
}

fn inheritance_topological_sort<'a, 'ast>(preprocessed_types: &[TypeEnvironmentPair<'a, 'ast>])
        -> Option<Vec<TypeEnvironmentPair<'a, 'ast>>> {

    // To find items in processed_types by fully-qualified names.
    let mut lookup = HashMap::new();
    for &(typedef, ref env) in preprocessed_types.iter() {
        lookup.insert(typedef.fq_name, (typedef, env.clone()));
    }

    let mut sorted: Vec<Name> = vec![];

    {
        let mut seen: HashSet<Name> = HashSet::new();
        let mut visited: HashSet<Name> = HashSet::new();

        // Keep track of the depth-first search stack for error message
        // purposes (it shows the user where the cycle is).
        let mut stack: Vec<Name> = vec![];

        for &(typedef, _) in preprocessed_types.iter() {
            if !visited.contains(&typedef.fq_name) {
                let result = inheritance_topological_sort_search(
                    typedef, &mut seen, &mut visited,
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

fn build_environments<'a, 'ast>(toplevel: PackageRef<'a, 'ast>,
                                asts: &'ast [ast::CompilationUnit]) {
    let mut preprocessed_types = vec![];

    for ast in asts.iter() {
        let mut types_env: TypesEnvironment<'a, 'ast> = RbMap::new();

        let mut on_demand_packages = vec![];

        // Add all imports to initial environment for this compilation unit.
        for import in ast.imports.iter() {
            match import.node {
                ast::ImportDeclaration_::SingleType(ref qident) => {
                    types_env = import_single_type(qident,
                                                   toplevel,
                                                   types_env);
                },
                ast::ImportDeclaration_::OnDemand(ref qident) => {
                    import_on_demand(qident,
                                     toplevel,
                                     &mut on_demand_packages);
                },
            }
        }

        // TODO: For testing - remove later.
        println!("{} types environment: {:?}", ast.types[0].name(), types_env);

        let env_stack = EnvironmentStack {
            types: types_env,
            variables: vec![RbMap::new()],
            toplevel: toplevel,
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

    for &(typedef, ref env) in preprocessed_types.iter() {
        env.clone().walk_type_declaration(typedef.ast);
    }
}

pub fn name_resolve<'a, 'ast>(arena: &'a Arena<'a, 'ast>, asts: &'ast [ast::CompilationUnit]) -> PackageRef<'a, 'ast> {
    let toplevel = arena.alloc(Package::new("top level".to_owned()));
    collect_types(arena, toplevel, asts);
    build_environments(toplevel, asts);

    // TODO: For testing - remove when name resolution is finished.
    PackageItem::Package(toplevel).print_light();
    toplevel
}

