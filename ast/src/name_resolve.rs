use ast::*;
use name::*;

use std::collections::HashMap;

#[derive(Show)]
pub enum NamedItem {
    Package(Package),
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

#[derive(Show)]
pub struct TypeDefinition {
    // Note that members and methods can have the same name, therefore
    // need to be be in separate namespaces.
    members: HashMap<Symbol, Name>,
    methods: HashMap<Symbol, Name>,
}

impl TypeDefinition {
    pub fn new() -> TypeDefinition {
        TypeDefinition { members: HashMap::new(), methods: HashMap::new() }
    }
}

// Returns the type definition contained within the compilation unit,
// of which we know there is only one.
pub fn get_type_definition(compilationUnit: &CompilationUnit) -> TypeDefinition {
    match compilationUnit.types[0].node {
        TypeDeclaration_::Class(ref class) => {
            let mut typeDefinition = TypeDefinition::new();

            typeDefinition
        },
        TypeDeclaration_::Interface(ref interface) => {
            let mut typeDefinition = TypeDefinition::new();

            typeDefinition
        },
    }
}

pub fn name_resolve(asts: &Vec<CompilationUnit>) {
    let mut all: HashMap<Name, NamedItem> = HashMap::new();

    for ast in asts.iter() {
        if let Some(ref identifier) = ast.package {
            let package_resolved_name = Name::fresh(identifier.as_symbol());
            let typeDefinition = get_type_definition(ast);
            let type_resolved_name =
                Name::fresh(identifier.append_ident(ast.name()).as_symbol());

            let mut present = false;
            if let Some(&mut NamedItem::Package(ref mut package)) =
                   all.get_mut(&package_resolved_name) {
                package.contents.insert(Symbol::from_str(ast.name().as_slice()),
                                        type_resolved_name);
                present = true;
            }

            if !present {
                let mut package = Package { contents: HashMap::new() };
                package.contents.insert(Symbol::from_str(ast.name().as_slice()),
                                        type_resolved_name);
                all.insert(package_resolved_name, NamedItem::Package(package));
            }

            // This is more idiomatic than having a boolean flag, but breaks
            // borrow checking rules.
            // match all.get(&package_resolved_name) {
            //     Some(&NamedItem::Package(ref package)) => {
            //         package.contents.insert(Symbol::from_str(ast.name().as_slice()),
            //                                 type_resolved_name);
            //     }
            //     None => {
            //         let mut package = Package { contents: HashMap::new() };
            //         package.contents.insert(Symbol::from_str(ast.name().as_slice()),
            //                                 type_resolved_name);
            //         all.insert(package_resolved_name, NamedItem::Package(package));
            //     }
            // }
        }
    }

    // For testing - remove when name resolution is finished.
    for (name, named_item) in all.iter() {
        println!("{:?}: {:?}", name, named_item);
    }
}
