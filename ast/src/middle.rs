use ast;
use name::*;
use span::*;

use ivar::Ivar;
use std::cell::RefCell;
use std::collections::HashMap;
use std::cmp::{Ord, Ordering};
use std::{fmt, hash};

pub use self::Impled::*;
pub use self::Accessibility::*;

// In this file, the variable name prefix 'fq_' abbreviates fully_qualified_

// The idea here is to have an object graph represented using borrowed references.
// RefCell allows fields in the graph to be mutated by later passes,
// while Ivar allows fields to be initially unset but assigned just once.
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

impl<'a, 'ast> PartialEq for Package<'a, 'ast> {
    fn eq(&self, other: &Self) -> bool {
        self.fq_name.eq(&other.fq_name)
    }
}

impl<'a, 'ast> Eq for Package<'a, 'ast> {}

#[derive(Show)]
pub struct Universe<'a, 'ast: 'a> {
    pub toplevel: PackageRef<'a, 'ast>,
    pub default: PackageRef<'a, 'ast>,
    pub main: TypeDefinitionRef<'a, 'ast>,
}

impl<'a, 'ast> Universe<'a, 'ast> {
    pub fn each_type<F: FnMut(TypeDefinitionRef<'a, 'ast>)>(&self, f: F) {
        use typed_walker::{Walker, StatementWalker, ExpressionWalker};
        struct Find<'a, 'ast: 'a, G>(G);
        impl<'a, 'ast, G: FnMut(TypeDefinitionRef<'a, 'ast>)> Walker<'a, 'ast> for Find<'a, 'ast, G> {
            fn walk_type_definition(&mut self, tydef: TypeDefinitionRef<'a, 'ast>) {
                self.0(tydef);
            }
        }
        impl<'a, 'ast, G> StatementWalker<'a, 'ast> for Find<'a, 'ast, G> { }
        impl<'a, 'ast, G> ExpressionWalker<'a, 'ast> for Find<'a, 'ast, G> { }
        Find(f).walk_universe(self);
    }
}

#[derive(Show)]
pub struct Field<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub origin: TypeDefinitionRef<'a, 'ast>,
    pub ty: Type<'a, 'ast>,
    pub initializer: Ivar<Option<TypedExpression<'a, 'ast>>>,
    pub ast: &'ast ast::Field,
}
pub type FieldRef<'a, 'ast> = &'a Field<'a, 'ast>;

impl<'a, 'ast> Field<'a, 'ast> {
    pub fn new(name: String, origin: TypeDefinitionRef<'a, 'ast>, ty: Type<'a, 'ast>, ast: &'ast ast::Field) -> Field<'a, 'ast> {
        Field {
            fq_name: Name::fresh(name),
            origin: origin,
            ty: ty,
            initializer: Ivar::new(),
            ast: ast,
        }
    }

    pub fn is_static(&self) -> bool {
        self.ast.node.has_modifier(ast::Modifier_::Static)
    }

    pub fn is_protected(&self) -> bool {
        self.ast.node.has_modifier(ast::Modifier_::Protected)
    }
}

pub type Arguments<'a, 'ast> = Vec<Type<'a, 'ast>>;
#[derive(Show, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct MethodSignature<'a, 'ast: 'a> {
    pub name: Symbol,
    pub args: Arguments<'a, 'ast>,
}

#[derive(Show, Clone)]
pub enum Impled<'a, 'ast: 'a> {
    Abstract,
    Concrete(MethodImplRef<'a, 'ast>),
}

#[derive(Show, Clone)]
pub enum Accessibility<'a, 'ast: 'a> {
    Protected(TypeDefinitionRef<'a, 'ast>),
    Public,
}

#[derive(Show)]
pub struct Method<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub ret_ty: Type<'a, 'ast>,
    pub impled: Impled<'a, 'ast>,
    pub is_final: bool,
    pub is_static: bool,
    pub accessibility: Accessibility<'a, 'ast>,
    // In the case of inheritance, there may be no associated AST fragment.
    pub ast: Option<&'ast ast::Method>,
}
pub type MethodRef<'a, 'ast> = &'a Method<'a, 'ast>;

impl<'a, 'ast> Method<'a, 'ast> {
    pub fn new(name: String,
               ret_ty: Type<'a, 'ast>,
               impled: Impled<'a, 'ast>,
               is_final: bool,
               is_static: bool,
               accessibility: Accessibility<'a, 'ast>,
               ast: Option<&'ast ast::Method>) -> Method<'a, 'ast> {
        Method {
            fq_name: Name::fresh(name),
            ret_ty: ret_ty,
            impled: impled,
            is_final: is_final,
            is_static: is_static,
            accessibility: accessibility,
            ast: ast,
        }
    }
}

// Despite the name, MethodImpls are created even for abstract methods.
// Each MethodImpl corresponds to one method declaration in the source.
#[derive(Show)]
pub struct MethodImpl<'a, 'ast: 'a> {
    pub fq_name: Name,
    pub origin: TypeDefinitionRef<'a, 'ast>,
    pub arg_types: Arguments<'a, 'ast>,
    // FIXME: This is duplicated from Method
    pub ret_ty: Type<'a, 'ast>,
    pub is_static: bool,
    pub is_native: bool,
    // These are initially uninitialized  until typechecking.
    pub args: Ivar<Vec<VariableRef<'a, 'ast>>>,
    pub body: Ivar<Option<TypedBlock<'a, 'ast>>>,
    pub ast: &'ast ast::Method,
}
pub type MethodImplRef<'a, 'ast> = &'a MethodImpl<'a, 'ast>;

impl<'a, 'ast> MethodImpl<'a, 'ast> {
    pub fn new(name: String,
               origin: TypeDefinitionRef<'a, 'ast>,
               arg_types: Arguments<'a, 'ast>,
               ret_ty: Type<'a, 'ast>,
               is_static: bool,
               ast: &'ast ast::Method) -> Self {
        MethodImpl {
            fq_name: Name::fresh(name),
            origin: origin,
            arg_types: arg_types,
            ret_ty: ret_ty,
            is_static: is_static,
            is_native: ast.node.has_modifier(ast::Modifier_::Native),
            args: Ivar::new(),
            body: Ivar::new(),
            ast: ast,
        }
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
    pub args: Ivar<Vec<VariableRef<'a, 'ast>>>,
    pub body: Ivar<TypedBlock<'a, 'ast>>,
    pub ast: &'ast ast::Constructor,
}
pub type ConstructorRef<'a, 'ast> = &'a Constructor<'a, 'ast>;

impl<'a, 'ast> Constructor<'a, 'ast> {
    pub fn new(name: String, ast: &'ast ast::Constructor) -> Self {
        Constructor {
            fq_name: Name::fresh(name),
            args: Ivar::new(),
            body: Ivar::new(),
            ast: ast,
        }
    }

    pub fn is_protected(&self) -> bool {
        self.ast.node.has_modifier(ast::Modifier_::Protected)
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
    pub package: PackageRef<'a, 'ast>,

    // Note that fields and methods can have the same name, therefore
    // need to be be in separate namespaces.
    pub ordered_fields: Ivar<Vec<Symbol>>,
    pub fields: Ivar<HashMap<Symbol, FieldRef<'a, 'ast>>>,

    // Method overloads can have the same name, but must have different signatures.
    pub methods: Ivar<HashMap<MethodSignature<'a, 'ast>, MethodRef<'a, 'ast>>>,
    pub method_impls: Ivar<Vec<MethodImplRef<'a, 'ast>>>,
    // Similarly, we can have multiple constructors, as long as their arguments have different
    // types.
    pub constructors: Ivar<HashMap<Arguments<'a, 'ast>, ConstructorRef<'a, 'ast>>>,

    pub extends: Ivar<Vec<TypeDefinitionRef<'a, 'ast>>>,
    pub implements: Ivar<Vec<TypeDefinitionRef<'a, 'ast>>>,

    pub ast: &'ast ast::TypeDeclaration,
}
pub type TypeDefinitionRef<'a, 'ast> = &'a TypeDefinition<'a, 'ast>;

impl<'a, 'ast> TypeDefinition<'a, 'ast> {
    pub fn new(name: String, kind: TypeKind,
               package: PackageRef<'a, 'ast>,
               ast: &'ast ast::TypeDeclaration) -> TypeDefinition<'a, 'ast> {
        TypeDefinition {
            fq_name: Name::fresh(name),
            kind: kind,
            package: package,
            ordered_fields: Ivar::new(),
            fields: Ivar::new(),
            methods: Ivar::new(),
            method_impls: Ivar::new(),
            constructors: Ivar::new(),
            extends: Ivar::new(),
            implements: Ivar::new(),
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

    Void,

    // Placeholder when name resolution fails.
    Unknown,
}

impl<'a, 'ast> Type<'a, 'ast> {
    pub fn is_numeric(&self) -> bool {
        match *self {
            Type::SimpleType(ref t) => t.is_numeric(),
            _ => false,
        }
    }

    pub fn is_unknown(&self) -> bool {
        match *self {
            Type::Unknown => true,
            _ => false,
        }
    }

    pub fn is_reference(&self) -> bool {
        match *self {
            Type::SimpleType(SimpleType::Other(..)) => true,
            Type::ArrayType(..) => true,
            _ => false,
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            Type::Null => true,
            _ => false,
        }
    }

    pub fn object(t: TypeDefinitionRef<'a, 'ast>) -> Type<'a, 'ast> {
        Type::SimpleType(SimpleType::Other(t))
    }
}

impl<'a, 'ast> fmt::String for Type<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &Type::SimpleType(ref t) => t.fmt(f),
            &Type::ArrayType(ref t) => write!(f, "{}[]", t),
            &Type::Null => write!(f, "null"),
            &Type::Void => write!(f, "void"),
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

impl<'a, 'ast> SimpleType<'a, 'ast> {
    pub fn is_numeric(&self) -> bool {
        use self::SimpleType::*;
        match *self {
            Int | Short | Char | Byte => true,
            Boolean | Other(..) => false,
        }
    }
    pub fn is_strict_numeric(&self) -> bool {
        use self::SimpleType::*;
        match *self {
            Int | Short | Byte => true,
            Boolean | Char | Other(..) => false,
        }
    }
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
    Return(Option<TypedExpression<'a, 'ast>>),
    Block(TypedBlock<'a, 'ast>),
}
pub type TypedStatement<'a, 'ast> = Spanned<TypedStatement_<'a, 'ast>>;

#[derive(Show)]
pub enum TypedExpression_<'a, 'ast: 'a> {
    Literal(&'ast ast::Literal),
    This,
    NewStaticClass(TypeDefinitionRef<'a, 'ast>, ConstructorRef<'a, 'ast>,
                   Vec<TypedExpression<'a, 'ast>>),
    NewArray(SimpleType<'a, 'ast>, Box<TypedExpression<'a, 'ast>>),
    Variable(VariableRef<'a, 'ast>),
    StaticFieldAccess(FieldRef<'a, 'ast>),
    FieldAccess(Box<TypedExpression<'a, 'ast>>, FieldRef<'a, 'ast>),
    ThisFieldAccess(FieldRef<'a, 'ast>),
    // FIXME: Holy hack
    ArrayLength(Box<TypedExpression<'a, 'ast>>),
    MethodInvocation(Option<Box<TypedExpression<'a, 'ast>>>, MethodRef<'a, 'ast>,
                     Vec<TypedExpression<'a, 'ast>>),
    ArrayAccess(Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    Assignment(Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    InstanceOf(Box<TypedExpression<'a, 'ast>>, Type<'a, 'ast>),
    Prefix(ast::PrefixOperator, Box<TypedExpression<'a, 'ast>>),
    Infix(ast::InfixOperator, Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    Cast(Type<'a, 'ast>, Box<TypedExpression<'a, 'ast>>),
    // String concatenation.
    Concat(Box<TypedExpression<'a, 'ast>>, Box<TypedExpression<'a, 'ast>>),
    // An implicit widening conversion.
    Widen(Box<TypedExpression<'a, 'ast>>),
    // A "string conversion", as described in $15.18.1.1.
    ToString(Box<TypedExpression<'a, 'ast>>),
}
pub type TypedExpression<'a, 'ast> = Spanned<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)>;

impl<'a, 'ast> TypedExpression<'a, 'ast> {
    pub fn ty(&self) -> &Type<'a, 'ast> {
        &self.1
    }
}
