use ast;
use name::*;

use std::cell::RefCell;
use std::collections::HashMap;
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
    pub fq_name: Name,
    pub contents: HashMap<Symbol, PackageItem<'ast>>
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
    pub fq_name: Name,
    pub ast: &'ast ast::Field,
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
    pub fq_name: Name,
    pub ast: &'ast ast::Method,
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
    pub fq_name: Name,
    pub kind: TypeKind,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    pub fields: HashMap<Symbol, FieldRef<'ast>>,

    // Method overloads can have the same name.
    pub methods: HashMap<Symbol, Vec<MethodRef<'ast>>>,

    pub extends: Vec<TypeDefinitionWeak<'ast>>,
    pub implements: Vec<TypeDefinitionWeak<'ast>>,

    pub ast: &'ast ast::TypeDeclaration,
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

