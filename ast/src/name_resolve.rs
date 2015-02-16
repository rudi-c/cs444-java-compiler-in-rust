use ast::*;
use name::*;

use std::collections::HashMap;

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

#[derive(Show)]
pub enum NamedItem {
    Package(Package),
    TypeDefinition(TypeDefinition),
    // TODO: Store AST bits.
    Field,
    Method,
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
    kind: TypeKind,

    // Note that members and methods can have the same name, therefore
    // need to be be in separate namespaces.
    members: HashMap<Symbol, Name>,
    methods: HashMap<Symbol, Name>,
}

pub fn process_class_body(class: &Class,
                          type_definition: &mut TypeDefinition,
                          all: &mut HashMap<Name, NamedItem>,
                          class_identifier: &QualifiedIdentifier) {
    for body_declaration in class.node.body.iter() {
        match body_declaration.node {
            ClassBodyDeclaration_::FieldDeclaration(ref field) => {
                let field_symbol = Symbol::from_str(field.node.name.as_slice());
                let fq_ident = class_identifier.append_ident(&field.node.name);
                let fq_field_name = Name::fresh(fq_ident.as_symbol());

                let exists = all.contains_key(&fq_field_name);
                if exists {
                    span_error!(field.span, "field {} already exists in {}",
                                field.node.name, class_identifier.as_string());
                } else {
                    all.insert(fq_field_name, NamedItem::Field);
                    type_definition.members.insert(field_symbol, fq_field_name);
                }
            },
            ClassBodyDeclaration_::MethodDeclaration(ref method) => {
                let method_symbol = Symbol::from_str(method.node.name.as_slice());
                let fq_ident = class_identifier.append_ident(&method.node.name);
                let fq_method_name = Name::fresh(fq_ident.as_symbol());

                let exists = all.contains_key(&fq_method_name);
                if !exists {
                    all.insert(fq_method_name, NamedItem::Method);
                    type_definition.methods.insert(method_symbol, fq_method_name);
                }
            },
            ClassBodyDeclaration_::ConstructorDeclaration(_) => {

            },
        }
    }
}

pub fn process_interface_body(interface: &Interface,
                              type_definition: &mut TypeDefinition,
                              all: &mut HashMap<Name, NamedItem>,
                              interface_identifier: &QualifiedIdentifier) {
    for method in interface.node.body.iter() {
        let method_symbol = Symbol::from_str(method.node.name.as_slice());
        let fq_ident = interface_identifier.append_ident(&method.node.name);
        let fq_method_name = Name::fresh(fq_ident.as_symbol());

        let exists = all.contains_key(&fq_method_name);
        if !exists {
            all.insert(fq_method_name, NamedItem::Method);
            type_definition.methods.insert(method_symbol, fq_method_name);
        }
    }
}

// Process and return the type definition contained within the compilation unit,
// of which we know there is only one.
pub fn get_type_definition(compilation_unit: &CompilationUnit,
                           all: &mut HashMap<Name, NamedItem>,
                           package_identifier: &QualifiedIdentifier) -> TypeDefinition {

    let fq_type_identifier = package_identifier.append_ident(compilation_unit.name());

    match compilation_unit.types[0].node {
        TypeDeclaration_::Class(ref class) => {
            let mut type_definition = TypeDefinition
                { kind: TypeKind::Class, members: HashMap::new(),
                  methods: HashMap::new() };

            process_class_body(class, &mut type_definition, all, &fq_type_identifier);

            type_definition
        },
        TypeDeclaration_::Interface(ref interface) => {
            let mut type_definition = TypeDefinition
                { kind: TypeKind::Interface, members: HashMap::new(),
                  methods: HashMap::new() };

            process_interface_body(interface, &mut type_definition, all,
                                   &fq_type_identifier);

            type_definition
        },
    }
}

pub fn name_resolve(asts: &Vec<CompilationUnit>) {
    let mut all: HashMap<Name, NamedItem> = HashMap::new();

    for ast in asts.iter() {
        if let Some(ref package_identifier) = ast.package {
            let type_definition = get_type_definition(ast, &mut all,
                                                      package_identifier);

            let fq_package_name = Name::fresh(package_identifier.as_symbol());
            let fq_type_name =
                Name::fresh(package_identifier.append_ident(ast.name()).as_symbol());

            // This is kind of a weird pattern but the scope of the .get_mut
            // borrow is the entire if statement, including the else branch,
            // so it becomes impossible to use .insert in the else branch.
            let mut present = false;
            if let Some(&mut NamedItem::Package(ref mut package)) =
                   all.get_mut(&fq_package_name) {
                package.contents.insert(Symbol::from_str(ast.name().as_slice()),
                                        fq_type_name);
                present = true;
            }

            if !present {
                let mut package = Package { contents: HashMap::new() };
                package.contents.insert(Symbol::from_str(ast.name().as_slice()),
                                        fq_type_name);
                all.insert(fq_package_name, NamedItem::Package(package));
            }

            all.insert(fq_type_name, NamedItem::TypeDefinition(type_definition));
        }
    }

    // For testing - remove when name resolution is finished.
    for (name, named_item) in all.iter() {
        println!("{:?}: {:?}", name, named_item);
    }
}
