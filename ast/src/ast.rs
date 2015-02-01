pub type Ident = String;

#[derive(Show)]
pub enum ImportDeclaration {
    SingleType(QualifiedIdentifier),
    OnDemand(QualifiedIdentifier),
}

#[derive(Show)]
pub enum TypeDeclaration {
    Class(Class),
    Interface(Interface),
}

#[derive(Show)]
pub struct CompilationUnit {
    pub package: Option<QualifiedIdentifier>,
    pub imports: Vec<ImportDeclaration>,
    pub types: Vec<TypeDeclaration>,
}

#[derive(Show)]
pub struct Class {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Option<QualifiedIdentifier>,
    pub implements: Vec<QualifiedIdentifier>,
    pub body: Vec<ClassBodyDeclaration>,
}

#[derive(Show)]
pub enum ClassBodyDeclaration {
    FieldDeclaration(Field),
    MethodDeclaration(Method),
    ConstructorDeclaration(Constructor),

    // Not in Joos: InstanceInitializer, StaticInitializer
}

#[derive(Show)]
pub struct Constructor {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    pub body: Block,
}

#[derive(Show)]
pub struct Method {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    // `void` == None
    pub return_type: Option<Type>,
    pub body: Option<Block>,
}

#[derive(Show)]
pub struct Field {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub ty: Type,
    pub initializer: Option<Expression>,
}

#[derive(Show)]
pub struct Interface {
    pub name: Ident,
    pub modifiers: Vec<Modifier>,
    pub extends: Vec<QualifiedIdentifier>,
    pub body: Vec<Method>,
}

#[derive(Show, Hash, PartialEq, Eq)]
pub enum Modifier {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
    Native,
}

#[derive(Show)]
pub struct VariableDeclaration {
    pub ty: Type,
    pub name: Ident,
}

#[derive(Show)]
pub struct QualifiedIdentifier {
    pub parts: Vec<Ident>,
}

#[derive(Show)]
pub enum Type {
    SimpleType(SimpleType),
    ArrayType(SimpleType)
}

#[derive(Show)]
pub enum SimpleType {
    Boolean,
    Int,
    Short,
    Char,
    Byte,
    Other(QualifiedIdentifier),
}

#[derive(Show)]
pub enum BlockStatement {
    LocalVariable(LocalVariable),
    LocalClass(Class),
    Statement(Statement),
}

#[derive(Show)]
pub struct Block {
    pub stmts: Vec<BlockStatement>,
}

#[derive(Show)]
pub struct LocalVariable {
    pub variable: VariableDeclaration,
    pub initializer: Expression,
}

#[derive(Show)]
pub enum Statement {
    // FIXME
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    While(Expression, Box<Statement>),
    // FIXME
    For(Option<Expression>, Option<Expression>, Option<Expression>, Box<Statement>),
    ForDecl(LocalVariable, Option<Expression>, Option<Expression>, Box<Statement>),
    Empty,
    Return(Expression),
    Block(Block),
}

#[derive(Show)]
pub enum Expression {
    Literal(Literal),
    This,
    QualifiedThis(QualifiedIdentifier),
    NewStaticClass(QualifiedIdentifier, Vec<Expression>,
                   Option<Vec<ClassBodyDeclaration>>),
    NewDynamicClass(Box<Expression>, Ident, Vec<Expression>,
                    Option<Vec<ClassBodyDeclaration>>),
    NewArray(SimpleType, Box<Expression>),
    FieldAccess(Box<Expression>, Ident),
    MethodInvocation(Option<Box<Expression>>, Ident, Vec<Expression>),
    ArrayAccess(Box<Expression>, Box<Expression>),
    Name(QualifiedIdentifier),
    Assignment(Box<Expression>, Box<Expression>),
    InstanceOf(Box<Expression>, Type),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(InfixOperator, Box<Expression>, Box<Expression>),
    Cast(Type, Box<Expression>),
}

#[derive(Show)]
pub enum Literal {
    Integer(u64),
    Boolean(bool),
    Character(char),
    String(String),
    Null,
}

#[derive(Show)]
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

#[derive(Show)]
pub enum PrefixOperator {
    Minus,
    Not,
}
