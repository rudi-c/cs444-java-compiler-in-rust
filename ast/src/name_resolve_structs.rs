use ast;
use name::*;

use rbtree::RbMap;

use std::cell::RefCell;
use std::collections::HashMap;
use std::cmp::{Ord, Ordering};
use std::fmt::{Show, Formatter, Error};

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

// The idea here is to have an object graph represented using borrowed references.
// RefCell allows fields in the graph to be mutated by later passes.
// All objects are allocated from a single `Arena`, and are deallocated at the same time.
// This allows circular references without leaks or unsafety.
// Every object has a unique `Name`, which can be used as an key for external maps. There is
// intentionally no way to go from a `Name` back to the object: if you need it, just store the
// pointer! `Name`s are also generally associated with fully-qualified identifiers, suitable for
// printing in error messages.
// Many objects also hold references to their associated AST nodes, hence the 'ast lifetime
// everywhere.

#[derive(Show, Clone)]
pub enum PackageItem<'a, 'ast: 'a> {
    Package(PackageRef<'a, 'ast>),
    TypeDefinition(TypeDefinitionRef<'a, 'ast>),
}

impl<'a, 'ast> PackageItem<'a, 'ast> {
    pub fn fq_name(&self) -> Name {
        match self {
            &PackageItem::Package(ref package) => package.fq_name,
            &PackageItem::TypeDefinition(ref typedef) => typedef.fq_name,
        }
    }

    pub fn print_light(&self) {
        println!("{}", self.fq_name());
        if let &PackageItem::Package(ref package) = self {
            for package_item in package.contents.borrow().values() {
                package_item.print_light();
            }
        }
    }
}

#[derive(Show)]
pub struct Package<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub contents: RefCell<HashMap<Symbol, PackageItem<'a, 'ast>>>
}
pub type PackageRef<'a, 'ast> = &'a Package<'a, 'ast>;

impl<'a, 'ast> Package<'a, 'ast> {
    pub fn new(name: String) -> Package<'a, 'ast> {
        Package {
            fq_name: Name::fresh(name),
            contents: RefCell::new(HashMap::new()),
        }
    }
}

#[derive(Show)]
pub struct Field<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub ast: &'ast ast::Field,
}
pub type FieldRef<'a, 'ast> = &'a Field<'a, 'ast>;

impl<'a, 'ast> Field<'a, 'ast> {
    pub fn new(name: String, ast: &'ast ast::Field) -> Field<'a, 'ast> {
        Field {
            fq_name: Name::fresh(name),
            ast: ast,
        }
    }
}

#[derive(Show)]
pub struct Method<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub ast: &'ast ast::Method,
}
pub type MethodRef<'a, 'ast> = &'a Method<'a, 'ast>;

impl<'a, 'ast> Method<'a, 'ast> {
    pub fn new(name: String, ast: &'ast ast::Method) -> Method<'a, 'ast> {
        Method {
            fq_name: Name::fresh(name),
            ast: ast,
        }
    }
}

pub type MethodSignature<'a, 'ast> = (Symbol, Vec<Type<'a, 'ast>>);

pub fn method_signature_string(signature: &MethodSignature) -> String {
    let mut string = String::new();
    let &(ref name, ref types) = signature;

    string.push_str(name.as_slice());
    string.push('(');
    for t in types.iter() {
        string.push_str(format!("{:?}, ", t).as_slice());
    }
    string.push(')');

    string
}

#[derive(Show, Clone)]
pub struct MethodInfo<'a, 'ast: 'a> {
    pub method: MethodRef<'a, 'ast>,
    pub source: TypeDefinitionRef<'a, 'ast>,
    pub return_type: Option<Type<'a, 'ast>>,
}
pub type MethodMap<'a, 'ast> = RbMap<MethodSignature<'a, 'ast>, MethodInfo<'a, 'ast>>;

#[derive(Show, Copy, Eq, PartialEq)]
pub enum TypeKind {
    Class,
    Interface,
}

#[derive(Show)]
pub struct TypeDefinition<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub kind: TypeKind,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    pub fields: RefCell<HashMap<Symbol, FieldRef<'a, 'ast>>>,

    // Method overloads can have the same name.
    pub methods: RefCell<HashMap<Symbol, Vec<MethodRef<'a, 'ast>>>>,
    pub all_methods: RefCell<MethodMap<'a, 'ast>>,

    pub extends: RefCell<Vec<TypeDefinitionRef<'a, 'ast>>>,
    pub implements: RefCell<Vec<TypeDefinitionRef<'a, 'ast>>>,

    pub ast: &'ast ast::TypeDeclaration,
}
pub type TypeDefinitionRef<'a, 'ast> = &'a TypeDefinition<'a, 'ast>;

impl<'a, 'ast> TypeDefinition<'a, 'ast> {
    pub fn new(name: String, kind: TypeKind, ast: &'ast ast::TypeDeclaration) -> TypeDefinition<'a, 'ast> {
        TypeDefinition {
            fq_name: Name::fresh(name),
            kind: kind,
            fields: RefCell::new(HashMap::new()),
            methods: RefCell::new(HashMap::new()),
            all_methods: RefCell::new(RbMap::new()),
            extends: RefCell::new(vec![]),
            implements: RefCell::new(vec![]),
            ast: ast,
        }
    }

    pub fn has_modifier(&self, modifier: ast::Modifier_) -> bool {
        match self.ast.node {
            ast::TypeDeclaration_::Class(ref class) =>
                class.node.modifiers.iter().any(|spanned| spanned.node == modifier),
            ast::TypeDeclaration_::Interface(ref interface) =>
                interface.node.modifiers.iter().any(|spanned| spanned.node == modifier),
        }
    }
}

impl<'a, 'ast> PartialEq for TypeDefinitionRef<'a, 'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.fq_name.eq(&other.fq_name)
    }
}

impl<'a, 'ast> Eq for TypeDefinitionRef<'a, 'ast> {}

impl<'a, 'ast> PartialOrd for TypeDefinitionRef<'a, 'ast> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.fq_name.partial_cmp(&other.fq_name)
    }
}

impl<'a, 'ast> Ord for TypeDefinitionRef<'a, 'ast> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.fq_name.cmp(&other.fq_name)
    }
}

// Helper function for printing a Type, handles the void case.
pub fn type_as_string(ty: &Option<Type>) -> String {
    if let &Some(ref t) = ty {
        format!("{:?}", t)
    } else {
        format!("void")
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a, 'ast: 'a> {
    SimpleType(SimpleType<'a, 'ast>),
    ArrayType(SimpleType<'a, 'ast>),

    // Placeholder when name resolution fails.
    Unknown,
}

impl<'a, 'ast> Show for Type<'a, 'ast> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            &Type::SimpleType(ref t) => t.fmt(f),
            &Type::ArrayType(ref t) => {
                try!(write!(f, "{:?}[]", t));
                Ok(())
            },
            &Type::Unknown => {
                try!(write!(f, "Unknown"));
                Ok(())
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum SimpleType<'a, 'ast: 'a> {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(TypeDefinitionRef<'a, 'ast>),
}

impl<'a, 'ast> Show for SimpleType<'a, 'ast> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        match self {
            &SimpleType::Boolean => try!(write!(f, "boolean")),
            &SimpleType::Int => try!(write!(f, "int")),
            &SimpleType::Short => try!(write!(f, "short")),
            &SimpleType::Char => try!(write!(f, "char")),
            &SimpleType::Byte => try!(write!(f, "byte")),
            &SimpleType::Other(ref typedef) => try!(write!(f, "{}", typedef.fq_name)),
        }
        Ok(())
    }
}

#[derive(Show)]
pub struct VariableDefinition<'a, 'ast: 'a> {
    fq_name: Name,
    ty: Type<'a, 'ast>,
    ast: &'ast ast::VariableDeclaration,
}
pub type VariableDefinitionRef<'a, 'ast> = &'a RefCell<VariableDefinition<'a, 'ast>>;
