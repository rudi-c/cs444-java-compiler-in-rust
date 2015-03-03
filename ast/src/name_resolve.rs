use ast;
use name::*;
use span::{DUMMY, Span, spanned};

use middle::*;
use collect_types::collect_types;
use collect_members::collect_members;
use tycheck::{populate_method, populate_constructor, populate_field};
use arena::Arena;
use walker::*;

use rbtree::RbMap;

use std::borrow::ToOwned;
use std::collections::{HashMap, HashSet};

#[derive(Show, Clone)]
pub enum Variable<'a, 'ast: 'a> {
    LocalVariable(VariableRef<'a, 'ast>),
    Field(FieldRef<'a, 'ast>),
}

pub type TypesEnvironment<'a, 'ast> = RbMap<Symbol, TypeDefinitionRef<'a, 'ast>>;
pub type VariablesEnvironment<'a, 'ast> = RbMap<Symbol, Variable<'a, 'ast>>;

#[derive(Show, Clone)]
pub struct Environment<'a, 'ast: 'a> {
    pub types: TypesEnvironment<'a, 'ast>,
    pub variables: VariablesEnvironment<'a, 'ast>,
    pub toplevel: PackageRef<'a, 'ast>,
    pub package: PackageRef<'a, 'ast>,
    pub ty: TypeDefinitionRef<'a, 'ast>,

    // Search here for more types.
    pub on_demand_packages: Vec<PackageRef<'a, 'ast>>
}

pub type TypeEnvironmentPair<'a, 'ast> = (TypeDefinitionRef<'a, 'ast>, Environment<'a, 'ast>);

impl<'a, 'ast> Environment<'a, 'ast> {
    // Resolve extends and implements. This is done separately from walking
    // the AST because we need to process the compilations in topological
    // order (with respect to inheritance).
    fn resolve_inheritance(&self, tydef: TypeDefinitionRef<'a, 'ast>) {
        match tydef.ast.node {
            ast::TypeDeclaration_::Class(ref class) => {
                if let Some(ref extension) = class.node.extends {
                    // This class extends a parent class.
                    self.resolve_class_extension(tydef, &*extension.node.parts);
                } else {
                    // This class does not extend a parent class. Therefore,
                    // we make it extend java.lang.Object by default.
                    let name = &[spanned(DUMMY, Symbol::from_str("java")),
                                 spanned(DUMMY, Symbol::from_str("lang")),
                                 spanned(DUMMY, Symbol::from_str("Object"))];
                    if let Some(parent) = self.resolve_type_name(name) {
                        // Don't make java.lang.Object inherit itself.
                        if parent.fq_name != tydef.fq_name {
                            self.resolve_class_extension(tydef, name);
                        }
                    }
                }
                self.resolve_implements(tydef,
                                        class.node.implements.as_slice());
            }
            ast::TypeDeclaration_::Interface(ref interface) => {
                self.resolve_interface_extensions(tydef,
                                                  interface.node.extends.as_slice());
            },
        }
    }

    fn resolve_class_extension(&self,
                               typedef: TypeDefinitionRef<'a, 'ast>,
                               extension: &[Ident]) {
        match self.resolve_type_name(extension) {
            Some(extended_type) => {
                if extended_type.kind == TypeKind::Interface {
                    // ($8.1.3, dOvs simple constraint 1)
                    span_error!(typedef.ast.span,
                                "class cannot extend interface");
                }
                if extended_type.has_modifier(ast::Modifier_::Final) {
                    // ($8.1.1.2/$8.1.3 dOvs simple constraint 4)
                    span_error!(typedef.ast.span,
                                "cannot extend final class `{}`",
                                extended_type.fq_name);
                }
                typedef.extends.borrow_mut().push(extended_type);
            },
            None => {
                // an error was already printed
            },
        }
    }

    fn resolve_interface_extensions(&self,
                                    typedef: TypeDefinitionRef<'a, 'ast>,
                                    extensions: &[QualifiedIdentifier]) {
        let mut seen = HashSet::new();
        for extension in extensions.iter() {
            match self.resolve_type_name(&*extension.node.parts) {
                Some(extended_type) => {
                    if extended_type.kind == TypeKind::Class {
                        // ($9.1.2)
                        span_error!(extension.span,
                                    "interface cannot extend class");
                    }
                    // An interface must not be repeated in an implements clause,
                    // or in an extends clause of an interface.
                    // (JLS 8.1.4, dOvs simple constraint 3)
                    if !seen.insert(&extended_type.fq_name) {
                        span_error!(extension.span,
                                    "duplicate extended interface");
                    }
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
        let mut seen = HashSet::new();
        for implement in implements.iter() {
            match self.resolve_type_name(&*implement.parts) {
                Some(implemented_type) => {
                    if implemented_type.kind == TypeKind::Class {
                        // ($8.1.4, dOvs simple constraint 2)
                        span_error!(implement.span,
                                    "class cannot implement class");
                    }
                    // An interface must not be repeated in an implements clause,
                    // or in an extends clause of an interface.
                    // (JLS 8.1.4, dOvs simple constraint 3)
                    if !seen.insert(&implemented_type.fq_name) {
                        span_error!(implement.span,
                                    "duplicate implemented interface");
                    }
                    typedef.implements.borrow_mut().push(implemented_type);
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    pub fn resolve_type(&self, ty: &ast::Type) -> Type<'a, 'ast> {
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

    pub fn resolve_simple_type(&self,
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

    // Look up a package or type name, but always fails when a type is found.
    // (This is because types in Joos never contain types.)
    // Emits an error on failure.
    fn resolve_package_or_type(&self, id: &[Ident]) -> Option<PackageRef<'a, 'ast>> {
        (match id {
            [] => panic!("bug: tried to resolve an empty package name"),
            [ref ident] => {
                // Because Java has nested types, we first have to check if
                // there is a type obscuring `ident`.
                match self.find_type(ident) {
                    Some(tydef) => {
                        span_error!(ident.span,
                                    "`{}` is a type, not a package",
                                    tydef.fq_name);
                        None
                    },
                    None => Some((self.toplevel, ident)),
                }
            }
            [init.., ref last] => self.resolve_package_or_type(init).map(|package| (package, last)),
        }).and_then(|(package, name)| match package.contents.borrow().get(&name.node) {
            // FIXME: duplicated code with `resolve_package`...
            Some(&PackageItem::Package(package)) => Some(package),
            Some(&PackageItem::TypeDefinition(..)) => {
                // There was a type instead!
                span_error!(Span::range(&id[0], id.last().unwrap()),
                            "no such package `{}`; found a type instead",
                            Qualified(id.iter()));
                None
            }
            None => {
                span_error!(Span::range(&id[0], id.last().unwrap()),
                            "no such package `{}`",
                            Qualified(id.iter()));
                None
            }
        })
    }

    // Look up a (user-defined) type by either a qualified or simple name.
    // Emits an error on failure.
    pub fn resolve_type_name(&self, id: &[Ident]) -> Option<TypeDefinitionRef<'a, 'ast>> {
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
            // in Joos, `init` must refer to a package... but if it's a type, we need to error
            [init.., ref last] => self.resolve_package_or_type(init).and_then(|package| {
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

    // Look up a type by simple name, using the current environment.
    // TODO: Clean up the error story here (don't want to emit multiple errors)
    fn find_type(&self, ty: &Ident) -> Option<TypeDefinitionRef<'a, 'ast>> {
        self.types.get(&ty.node)
        .cloned()
        .or_else(|| {
            // Check the current package. This has higher precedence than on-demand imports.
            match self.package.contents.borrow().get(&ty.node) {
                Some(&PackageItem::TypeDefinition(tydef)) => Some(tydef),
                _ => None,
            }
        })
        .or_else(|| {
            // If the type is not in the current environment, we can look in the
            // on-demand import packages.
            // It's necessary to look through every package to check for
            // ambiguities.
            let mut found_type: Option<TypeDefinitionRef<'a, 'ast>> = None;
            for package in self.on_demand_packages.iter() {
                match package.contents.borrow().get(&ty.node) {
                    Some(&PackageItem::TypeDefinition(typedef)) => {
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

    fn add_fields(&mut self, tydef: TypeDefinitionRef<'a, 'ast>) {
        for (&name, &field) in tydef.fields.borrow().iter() {
            self.add_field(name, field);
        }
    }

    fn add_field(&mut self, name: Symbol, field: FieldRef<'a, 'ast>) {
        if let Some(_) = self.variables.insert_in_place(name, Variable::Field(field)) {
            panic!("bug: multiple fields with the same name in scope, somehow");
        }
    }

    pub fn add_var(&mut self, name: Symbol, var: VariableRef<'a, 'ast>) {
        if let Some(old) = self.variables.insert_in_place(name, Variable::LocalVariable(var)) {
            if let (_, Variable::LocalVariable(v)) = *old {
                span_error!(var.ast.span,
                            "variable `{}` already defined",
                            name);
                span_note!(v.ast.span, "the old definition is here");
            }
        }
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
        if previous_item.fq_name != typedef.fq_name {
            // TODO: Shouldn't continue after this error - how to do that?
            span_error!(ident.span,
                        "type `{}` declared in this file conflicts with import `{}`",
                        ident,
                        previous_item.fq_name);
        }
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

    Some(sorted.iter().map(|name| lookup.get(name).unwrap().clone()).collect())
}

fn build_environments<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                toplevel: PackageRef<'a, 'ast>,
                                java_lang: PackageRef<'a, 'ast>,
                                units: &[(PackageRef<'a, 'ast>, &'ast ast::CompilationUnit, Vec<TypeDefinitionRef<'a, 'ast>>)])
-> Vec<(Environment<'a, 'ast>, ToPopulate<'a, 'ast>)> {
    let java_lang_object = match java_lang.contents.borrow().get(&Symbol::from_str("Object")) {
        Some(&PackageItem::TypeDefinition(tydef)) => tydef,
        _ => panic!("java.lang.Object not found"),
    };

    let mut preprocessed_types = vec![];

    for &(package, ast, ref tydefs) in units.iter() {
        let mut types_env: TypesEnvironment<'a, 'ast> = RbMap::new();

        let mut on_demand_packages = vec![java_lang];

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

        // Uniquify `on_demand_packages`.
        let on_demand_packages = on_demand_packages.into_iter()
            .map(|package| (package.fq_name, package))
            .collect::<HashMap<_, _> >()
            .into_iter()
            .map(|(_, package)| package)
            .collect();

        match &**tydefs {
            [tydef] => {
                let env = Environment {
                    types: types_env,
                    variables: RbMap::new(),
                    toplevel: toplevel,
                    package: package,
                    ty: tydef,
                    on_demand_packages: on_demand_packages,
                };

                env.resolve_inheritance(tydef);
                preprocessed_types.push((tydef, env));
                if tydef == java_lang_object {
                    // Make sure `java.lang.Object` is processed first...
                    // XXX: This is such a hack!
                    let ix = preprocessed_types.len()-1;
                    preprocessed_types.swap(0, ix);
                }
            }
            [] => {}
            _ => panic!("wrong number of types: {}", tydefs.len())
        }
    }

    if let Some(sorted) = inheritance_topological_sort(preprocessed_types.as_slice()) {
        preprocessed_types = sorted;
    } else {
        return vec![];
    }

    let mut r = vec![];

    for (tydef, mut env) in preprocessed_types.into_iter() {
        let name = tydef.ast.name();

        // Add the type itself to the environment.
        env.types = insert_declared_type(&env.types, name, tydef);

        collect_members(arena, &env, tydef, java_lang_object);

        env.add_fields(tydef);

        if tydef.kind == TypeKind::Class {
            // ($8.1.1.1) well-formedness contraint 4 - abstract methods => abstract class
            let should_be_abstract =
                tydef.methods.borrow().iter()
                    .any(|(_, &method)| method.impled == Abstract);
            if should_be_abstract && !tydef.has_modifier(ast::Modifier_::Abstract) {
                span_error!(tydef.ast.span,
                            "class with abstract methods must be abstract");
            }
        }

        for (_, &method) in tydef.methods.borrow().iter() {
            r.push((env.clone(), ToPopulate::Method(method)));
        }
        for (_, &constructor) in tydef.constructors.borrow().iter() {
            r.push((env.clone(), ToPopulate::Constructor(constructor)));
        }
        for (_, &field) in tydef.fields.borrow().iter() {
            r.push((env.clone(), ToPopulate::Field(field)));
        }
    }
    r
}

enum ToPopulate<'a, 'ast: 'a> {
    Method(MethodRef<'a, 'ast>),
    Constructor(ConstructorRef<'a, 'ast>),
    Field(FieldRef<'a, 'ast>),
}

fn populate<'a, 'ast>(arena: &'a Arena<'a, 'ast>, methods: Vec<(Environment<'a, 'ast>, ToPopulate<'a, 'ast>)>) {
    for (env, thing) in methods.into_iter() {
        match thing {
            ToPopulate::Method(method) => populate_method(arena, env, method),
            ToPopulate::Constructor(constructor) => populate_constructor(arena, env, constructor),
            ToPopulate::Field(field) => populate_field(arena, env, field),
        }
    }
}

pub fn name_resolve<'a, 'ast>(arena: &'a Arena<'a, 'ast>, asts: &'ast [ast::CompilationUnit]) -> PackageRef<'a, 'ast> {
    let toplevel = arena.alloc(Package::new("top level".to_owned()));
    let types = collect_types(arena, toplevel, asts);
    let java_lang = resolve_package(toplevel, &[
        spanned(DUMMY, Symbol::from_str("java")),
        spanned(DUMMY, Symbol::from_str("lang")),
    ]).unwrap();
    let methods = build_environments(arena, toplevel, java_lang, &*types);
    populate(arena, methods);

    toplevel
}

