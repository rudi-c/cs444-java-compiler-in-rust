#[derive(Show)]
pub enum Ast {
    Assign(Lhs, Rhs),
    Expr(Rhs),
}
#[derive(Show)]
pub enum Lhs {
    Deref(Rhs),
    Var(String),
}
#[derive(Show)]
pub enum Rhs {
    Lhs(Box<Lhs>),
}

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
    pub packages: QualifiedIdentifier,
    pub imports: Vec<ImportDeclaration>,
    pub types: Vec<TypeDeclaration>,
}

#[derive(Show)]
pub struct Class {
    pub name: String,
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
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    pub body: Vec<BlockStatement>
}

#[derive(Show)]
pub struct Method {
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub params: Vec<VariableDeclaration>,
    // Void == None
    pub returnType: Option<Type>,
    pub body: Vec<BlockStatement>,
}

#[derive(Show)]
pub struct Field {
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub jType: Type,
    pub initializer: Option<VariableInitializer>,
}

#[derive(Show)]
pub struct Interface {
    pub name: String,
    pub modifiers: Vec<Modifier>,
    pub extends: Vec<QualifiedIdentifier>,
}

#[derive(Show)]
pub enum Modifier {
    Public,
    Protected,
    Private,
    Abstract,
    Static,
    Final,
}

#[derive(Show)]
pub struct VariableDeclaration {
    pub jType: Type,
    pub name: String,
}

#[derive(Show)]
pub struct QualifiedIdentifier {
    pub parts: Vec<String>,
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
pub struct LocalVariable {
    pub variable: VariableDeclaration,
    pub initializer: VariableInitializer,
}

#[derive(Show)]
pub enum VariableInitializer {
    Expression(Expression),
    Array(Vec<VariableInitializer>),
}

#[derive(Show)]
pub struct Statement {
    pub temp: i32,
}

#[derive(Show)]
pub enum Expression {
    Literal(Literal),
    ClassLiteral(Option<Type>),
    This,
    QualifiedThis(QualifiedIdentifier),
    NewStaticClass(QualifiedIdentifier, Vec<Expression>,
                   Option<Vec<ClassBodyDeclaration>>),
    NewDynamicClass(Box<Expression>, Vec<Expression>,
                    Option<Vec<ClassBodyDeclaration>>),
    NewArray(SimpleType, Box<Expression>),
    NewArrayInit(SimpleType, Vec<VariableInitializer>),
    FieldAccess(Box<Expression>, String),
    MethodInvocation(Option<Box<Expression>>, String, Vec<Expression>),
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
    Integer(i64),
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
