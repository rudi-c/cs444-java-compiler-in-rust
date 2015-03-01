use ast;
use name::*;
use span::*;

use rbtree::RbMap;

use std::cell::RefCell;
use std::collections::HashMap;
use std::cmp::{Ord, Ordering};
use std::{fmt, hash};

pub use self::Impled::*;

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
    pub origin: TypeDefinitionRef<'a, 'ast>,
    pub ty: Type<'a, 'ast>,
    pub initializer: RefCell<Option<TypedExpression<'a, 'ast>>>,
    pub ast: &'ast ast::Field,
}
pub type FieldRef<'a, 'ast> = &'a Field<'a, 'ast>;

impl<'a, 'ast> Field<'a, 'ast> {
    pub fn new(name: String, origin: TypeDefinitionRef<'a, 'ast>, ty: Type<'a, 'ast>, ast: &'ast ast::Field) -> Field<'a, 'ast> {
        Field {
            fq_name: Name::fresh(name),
            origin: origin,
            ty: ty,
            initializer: RefCell::new(None),
            ast: ast,
        }
    }
}

pub type Arguments<'a, 'ast> = Vec<Type<'a, 'ast>>;
#[derive(Show, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MethodSignature<'a, 'ast: 'a> {
    pub name: Symbol,
    pub args: Arguments<'a, 'ast>,
}

#[derive(Show, Clone, Copy, PartialEq, Eq)]
pub enum Impled {
    Abstract,
    Concrete,
}

#[derive(Show)]
pub struct Method<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub origin: TypeDefinitionRef<'a, 'ast>,
    pub ret_ty: Option<Type<'a, 'ast>>, // None = void
    pub impled: Impled,
    /// These can be None if the method has no implementation, or if we simply haven't reached the
    /// typechecking phase yet.
    pub body: RefCell<Option<TypedBlock<'a, 'ast>>>,
    pub args: RefCell<Vec<VariableRef<'a, 'ast>>>,
    pub ast: &'ast ast::Method,
}
pub type MethodRef<'a, 'ast> = &'a Method<'a, 'ast>;

impl<'a, 'ast> Method<'a, 'ast> {
    pub fn new(name: String,
               origin: TypeDefinitionRef<'a, 'ast>,
               ret_ty: Option<Type<'a, 'ast>>,
               impled: Impled,
               ast: &'ast ast::Method) -> Method<'a, 'ast> {
        Method {
            fq_name: Name::fresh(name),
            origin: origin,
            ret_ty: ret_ty,
            impled: impled,
            body: RefCell::new(None),
            args: RefCell::new(vec![]),
            ast: ast,
        }
    }

    pub fn has_modifier(&self, modifier: ast::Modifier_) -> bool {
        self.ast.node.has_modifier(modifier)
    }
}

impl<'a, 'ast> fmt::String for MethodSignature<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "{}(", self.name));
        for t in self.args.iter() {
            try!(write!(f, "{}, ", t));
        }
        write!(f, ")")
    }
}

#[derive(Show)]
pub struct Constructor<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub body: RefCell<Option<TypedBlock<'a, 'ast>>>,
    pub args: RefCell<Vec<VariableRef<'a, 'ast>>>,
    pub ast: &'ast ast::Constructor,
}
pub type ConstructorRef<'a, 'ast> = &'a Constructor<'a, 'ast>;

impl<'a, 'ast> Constructor<'a, 'ast> {
    pub fn new(name: String, ast: &'ast ast::Constructor) -> Self {
        Constructor {
            fq_name: Name::fresh(name),
            body: RefCell::new(None),
            args: RefCell::new(vec![]),
            ast: ast,
        }
    }
}

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

    // Method overloads can have the same name, but must have different signatures.
    pub methods: RefCell<HashMap<MethodSignature<'a, 'ast>, MethodRef<'a, 'ast>>>,
    // Similarly, we can have multiple constructors, as long as their arguments have different
    // types.
    pub constructors: RefCell<HashMap<Arguments<'a, 'ast>, ConstructorRef<'a, 'ast>>>,

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
            constructors: RefCell::new(HashMap::new()),
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
impl<'a, 'ast> fmt::String for Option<Type<'a, 'ast>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let Some(ref t) = *self {
            t.fmt(f)
        } else {
            "void".fmt(f)
        }
    }
}

impl<'a, 'ast, H: hash::Hasher + hash::Writer> hash::Hash<H> for TypeDefinition<'a, 'ast> {
    fn hash(&self, state: &mut H) {
        self.fq_name.hash(state)
    }
}

#[derive(Show, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a, 'ast: 'a> {
    SimpleType(SimpleType<'a, 'ast>),
    ArrayType(SimpleType<'a, 'ast>),

    // The type of `null`.
    Null,

    // Placeholder when name resolution fails.
    Unknown,
}

impl<'a, 'ast> Type<'a, 'ast> {
    pub fn object(t: Option<TypeDefinitionRef<'a, 'ast>>) -> Type<'a, 'ast> {
        if let Some(t) = t {
            Type::SimpleType(SimpleType::Other(t))
        } else {
            Type::Unknown
        }
    }
}

impl<'a, 'ast> fmt::String for Type<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Type::SimpleType(ref t) => t.fmt(f),
            &Type::ArrayType(ref t) => write!(f, "{}[]", t),
            &Type::Null => write!(f, "null"),
            &Type::Unknown => write!(f, "_"),
        }
    }
}

#[derive(Show, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SimpleType<'a, 'ast: 'a> {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(TypeDefinitionRef<'a, 'ast>),
}

impl<'a, 'ast> fmt::String for SimpleType<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &SimpleType::Boolean => write!(f, "bool"),
            &SimpleType::Int => write!(f, "int"),
            &SimpleType::Short => write!(f, "short"),
            &SimpleType::Char => write!(f, "char"),
            &SimpleType::Byte => write!(f, "byte"),
            &SimpleType::Other(ref typedef) => typedef.fq_name.fmt(f),
        }
    }
}

#[derive(Show)]
pub struct VariableDef<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub ty: Type<'a, 'ast>,
    pub ast: &'ast ast::VariableDeclaration,
}
pub type VariableRef<'a, 'ast> = &'a VariableDef<'a, 'ast>;

impl<'a, 'ast> VariableDef<'a, 'ast> {
    pub fn new(name: String, ty: Type<'a, 'ast>, ast: &'ast ast::VariableDeclaration) -> Self {
        VariableDef {
            fq_name: Name::fresh(name),
            ty: ty,
            ast: ast,
        }
    }
}

#[derive(Show)]
pub struct TypedLocalVariable_<'a, 'ast: 'a> {
    pub variable: VariableRef<'a, 'ast>,
    pub initializer: TypedExpression<'a, 'ast>,
}
pub type TypedLocalVariable<'a, 'ast> = Spanned<TypedLocalVariable_<'a, 'ast>>;

#[derive(Show)]
pub enum TypedBlockStatement_<'a, 'ast: 'a> {
    LocalVariable(TypedLocalVariable<'a, 'ast>),
    Statement(TypedStatement<'a, 'ast>),
}
pub type TypedBlockStatement<'a, 'ast> = Spanned<TypedBlockStatement_<'a, 'ast>>;

#[derive(Show)]
pub struct TypedBlock_<'a, 'ast: 'a> {
    pub stmts: Vec<TypedBlockStatement<'a, 'ast>>,
}
pub type TypedBlock<'a, 'ast> = Spanned<TypedBlock_<'a, 'ast>>;

#[derive(Show)]
pub enum TypedStatement_<'a, 'ast: 'a> {
    Expression(TypedExpression<'a, 'ast>),
    If(TypedExpression<'a, 'ast>, Box<TypedStatement<'a, 'ast>>, Option<Box<TypedStatement<'a, 'ast>>>),
    While(TypedExpression<'a, 'ast>, Box<TypedStatement<'a, 'ast>>),
    For(Option<TypedExpression<'a, 'ast>>,
        Option<TypedExpression<'a, 'ast>>,
        Option<TypedExpression<'a, 'ast>>,
        Box<TypedStatement<'a, 'ast>>),
    ForDecl(TypedLocalVariable<'a, 'ast>,
            Option<TypedExpression<'a, 'ast>>,
            Option<TypedExpression<'a, 'ast>>,
            Box<TypedStatement<'a, 'ast>>),
    Empty,
    Return(TypedExpression<'a, 'ast>),
    Block(TypedBlock<'a, 'ast>),
}
pub type TypedStatement<'a, 'ast> = Spanned<TypedStatement_<'a, 'ast>>;

#[derive(Show)]
pub enum TypedExpression_<'a, 'ast: 'a> {
    Literal(&'ast ast::Literal),
    This,
    NewStaticClass(Type<'a, 'ast>, Vec<TypedExpression<'a, 'ast>>),
    NewDynamicClass(Box<TypedExpression<'a, 'ast>>, Ident, Vec<TypedExpression<'a, 'ast>>),
    NewArray(Type<'a, 'ast>, Box<TypedExpression<'a, 'ast>>),
    Variable(VariableRef<'a, 'ast>),
    StaticFieldAccess(FieldRef<'a, 'ast>),
    FieldAccess(Box<TypedExpression<'a, 'ast>>, FieldRef<'a, 'ast>),
    MethodInvocation(Option<Box<TypedExpression<'a, 'ast>>>, MethodRef<'a, 'ast>, Vec<TypedExpression<'a, 'ast>>),
    ArrayAccess(Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    Assignment(Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    InstanceOf(Box<TypedExpression<'a, 'ast>>, Type<'a, 'ast>),
    Prefix(ast::PrefixOperator, Box<TypedExpression<'a, 'ast>>),
    Infix(ast::InfixOperator, Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    Cast(Type<'a, 'ast>, Box<TypedExpression<'a, 'ast>>),
}
pub type TypedExpression<'a, 'ast> = Spanned<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)>;

impl<'a, 'ast> TypedExpression<'a, 'ast> {
    pub fn ty(&self) -> &Type<'a, 'ast> {
        &self.1
    }
}
