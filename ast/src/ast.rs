use span::Spanned;
use name::{Ident, QualifiedIdentifier};

#[derive(Show)]
pub enum ImportDeclaration_ {
    SingleType(QualifiedIdentifier),
    OnDemand(QualifiedIdentifier),
}
pub type ImportDeclaration = Spanned<ImportDeclaration_>;

#[derive(Show)]
pub enum TypeDeclaration_ {
    Class(Class),
    Interface(Interface),
}
pub type TypeDeclaration = Spanned<TypeDeclaration_>;

impl TypeDeclaration {
    pub fn name(&self) -> &Ident {
        match self.node {
            TypeDeclaration_::Class(ref class) => &class.node.name,
            TypeDeclaration_::Interface(ref interface) => &interface.node.name,
        }
    }
}

#[derive(Show)]
pub struct CompilationUnit {
    pub package: Option<QualifiedIdentifier>,
    pub imports: Vec<ImportDeclaration>,
    pub types: Vec<TypeDeclaration>,
}

impl CompilationUnit {
    pub fn name(&self) -> &Ident {
        self.types[0].name()
    }
}

#[derive(Show)]
pub struct Class_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Option<QualifiedIdentifier>,
    pub implements: Vec<QualifiedIdentifier>,
    pub body: Vec<ClassBodyDeclaration>,
}
pub type Class = Spanned<Class_>;

#[derive(Show)]
pub enum ClassBodyDeclaration_ {
    FieldDeclaration(Field),
    MethodDeclaration(Method),
    ConstructorDeclaration(Constructor),

    // Not in Joos: InstanceInitializer, StaticInitializer
}
pub type ClassBodyDeclaration = Spanned<ClassBodyDeclaration_>;

#[derive(Show)]
pub struct Constructor_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    pub body: Block,
}
pub type Constructor = Spanned<Constructor_>;

#[derive(Show)]
pub struct Method_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    // `void` == None
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}
pub type Method = Spanned<Method_>;

impl Method_ {
    pub fn has_modifier(&self, modifier: Modifier_) -> bool {
        self.modifiers.iter().any(|spanned| spanned.node == modifier)
    }
}

#[derive(Show)]
pub struct Field_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub ty: Type,
    pub initializer: Option<Expression>,
}
pub type Field = Spanned<Field_>;

#[derive(Show)]
pub struct Interface_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Vec<QualifiedIdentifier>,
    pub body: Vec<Method>,
}
pub type Interface = Spanned<Interface_>;

#[derive(Show, Hash, PartialEq, Eq, Copy)]
pub enum Modifier_ {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
    Native,
}
pub type Modifier = Spanned<Modifier_>;

#[derive(Show)]
pub struct VariableDeclaration_ {
    pub ty: Type,
    pub name: Ident,
}
pub type VariableDeclaration = Spanned<VariableDeclaration_>;

#[derive(Show)]
pub enum Type_ {
    SimpleType(SimpleType),
    ArrayType(SimpleType),
}
pub type Type = Spanned<Type_>;

#[derive(Show)]
pub enum SimpleType_ {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(QualifiedIdentifier),
}
pub type SimpleType = Spanned<SimpleType_>;

#[derive(Show)]
pub struct LocalVariable_ {
    pub variable: VariableDeclaration,
    pub initializer: Expression,
}
pub type LocalVariable = Spanned<LocalVariable_>;

#[derive(Show)]
pub struct Block_ {
    pub stmts: Vec<BlockStatement>,
}
pub type Block = Spanned<Block_>;

#[derive(Show)]
pub enum BlockStatement_ {
    LocalVariable(LocalVariable),
    LocalClass(Class),
    Statement(Statement),
}
pub type BlockStatement = Spanned<BlockStatement_>;

#[derive(Show)]
pub enum Statement_ {
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    For(Option<Expression>, Option<Expression>, Option<Expression>, Box<Statement>),
    ForDecl(LocalVariable, Option<Expression>, Option<Expression>, Box<Statement>),
    Empty,
    Return(Option<Expression>),
    Block(Block),
}
pub type Statement = Spanned<Statement_>;

#[derive(Show)]
pub enum Expression_ {
    Literal(Literal),
    This,
    NewStaticClass(QualifiedIdentifier, Vec<Expression>),
    NewDynamicClass(Box<Expression>, Ident, Vec<Expression>),
    NewArray(SimpleType, Box<Expression>),
    FieldAccess(Box<Expression>, Ident),
    NamedMethodInvocation(QualifiedIdentifier, Vec<Expression>),
    MethodInvocation(Option<Box<Expression>>, Ident, Vec<Expression>),
    ArrayAccess(Box<Expression>, Box<Expression>),
    Name(QualifiedIdentifier),
    Assignment(Box<Expression>, Box<Expression>),
    InstanceOf(Box<Expression>, Type),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Cast(Type, Box<Expression>),
}
pub type Expression = Spanned<Expression_>;

#[derive(Show)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    Character(char),
    String(String),
    Null,
}

#[derive(Show, Copy)]
pub enum InfixOperator {
    Xor,
    EagerOr,
    EagerAnd,
    LazyOr,
    LazyAnd,
    Equals,
    NotEquals,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
    Plus,
    Minus,
    Mult,
    Div,
    Modulo,
}

#[derive(Show, Copy)]
pub enum PrefixOperator {
    Minus,
    Not,
}
