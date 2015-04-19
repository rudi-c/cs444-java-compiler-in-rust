use span::Spanned;
use name::{Ident, QualifiedIdentifier};

#[derive(Debug)]
pub enum ImportDeclaration_ {
    SingleType(QualifiedIdentifier),
    OnDemand(QualifiedIdentifier),
}
pub type ImportDeclaration = Spanned<ImportDeclaration_>;

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Class_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Option<QualifiedIdentifier>,
    pub implements: Vec<QualifiedIdentifier>,
    pub body: Vec<ClassBodyDeclaration>,
}
pub type Class = Spanned<Class_>;

#[derive(Debug)]
pub enum ClassBodyDeclaration_ {
    FieldDeclaration(Field),
    MethodDeclaration(Method),
    ConstructorDeclaration(Constructor),

    // Not in Joos: InstanceInitializer, StaticInitializer
}
pub type ClassBodyDeclaration = Spanned<ClassBodyDeclaration_>;

#[derive(Debug)]
pub struct Constructor_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    pub body: Block,
}
pub type Constructor = Spanned<Constructor_>;

impl Constructor_ {
    pub fn has_modifier(&self, modifier: Modifier_) -> bool {
        self.modifiers.iter().any(|spanned| spanned.node == modifier)
    }
}

#[derive(Debug)]
pub struct Method_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    pub return_type: Type,
    pub body: Option<Block>,
}
pub type Method = Spanned<Method_>;

impl Method_ {
    pub fn has_modifier(&self, modifier: Modifier_) -> bool {
        self.modifiers.iter().any(|spanned| spanned.node == modifier)
    }
}

#[derive(Debug)]
pub struct Field_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub ty: Type,
    pub initializer: Option<Expression>,
}
pub type Field = Spanned<Field_>;

impl Field_ {
    pub fn has_modifier(&self, modifier: Modifier_) -> bool {
        self.modifiers.iter().any(|spanned| spanned.node == modifier)
    }
}

#[derive(Debug)]
pub struct Interface_ {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Vec<QualifiedIdentifier>,
    pub body: Vec<Method>,
}
pub type Interface = Spanned<Interface_>;

#[derive(Debug, Hash, PartialEq, Eq, Copy, Clone)]
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

#[derive(Debug)]
pub struct VariableDeclaration_ {
    pub ty: Type,
    pub name: Ident,
}
pub type VariableDeclaration = Spanned<VariableDeclaration_>;

#[derive(Debug)]
pub enum Type_ {
    SimpleType(SimpleType),
    ArrayType(SimpleType),
    Void,
}
pub type Type = Spanned<Type_>;

#[derive(Debug)]
pub enum SimpleType_ {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(QualifiedIdentifier),
}
pub type SimpleType = Spanned<SimpleType_>;

#[derive(Debug)]
pub struct LocalVariable_ {
    pub variable: VariableDeclaration,
    pub initializer: Expression,
}
pub type LocalVariable = Spanned<LocalVariable_>;

#[derive(Debug)]
pub struct Block_ {
    pub stmts: Vec<BlockStatement>,
}
pub type Block = Spanned<Block_>;

#[derive(Debug)]
pub enum BlockStatement_ {
    LocalVariable(LocalVariable),
    Statement(Statement),
}
pub type BlockStatement = Spanned<BlockStatement_>;

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Expression_ {
    Literal(Literal),
    This,
    QualifiedThis(QualifiedIdentifier),
    NewStaticClass(QualifiedIdentifier, Vec<Expression>),
    NewArray(SimpleType, Box<Expression>),
    FieldAccess(Box<Expression>, Ident),
    NamedMethodInvocation(QualifiedIdentifier, Vec<Expression>),
    MethodInvocation(Box<Expression>, Ident, Vec<Expression>),
    ArrayAccess(Box<Expression>, Box<Expression>),
    Name(QualifiedIdentifier),
    Assignment(Box<Expression>, Box<Expression>),
    InstanceOf(Box<Expression>, Type),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Cast(Type, Box<Expression>),
}
pub type Expression = Spanned<Expression_>;

#[derive(Debug)]
pub enum Literal {
    Integer(i64),
    Boolean(bool),
    Character(char),
    String(String),
    Null,
}

#[derive(Debug, Copy, Clone)]
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

#[derive(Debug, Copy, Clone)]
pub enum PrefixOperator {
    Minus,
    Not,
}
