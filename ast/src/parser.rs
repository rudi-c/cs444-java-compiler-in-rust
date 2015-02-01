use ast::*;
use tokenizer::*;
use tokenizer::Token::*;

// See Chapter 18 for full grammar (p.449) (pdf p.475)

parser! parse {
    Token;

    // Compilation unit ($7.3)
    root: CompilationUnit {
        packageDeclaration[pkg] importDeclarations[imports] typeDeclarations[types] =>
            CompilationUnit { package: pkg,
                              imports: imports,
                              types: types },
    }

    // Package declarations ($7.4)
    packageDeclaration: Option<QualifiedIdentifier> {
        => None,
        PACKAGE qualifiedIdentifier[ident] Semicolon => Some(ident),
    }

    // Import declarations ($7.5)
    importDeclaration: ImportDeclaration {
        IMPORT qualifiedIdentifier[ident] Semicolon => ImportDeclaration::SingleType(ident),
        IMPORT qualifiedIdentifierHelperDot[ident] Star Semicolon =>
            ImportDeclaration::OnDemand(QualifiedIdentifier { parts: ident }),
    }

    importDeclarations: Vec<ImportDeclaration> {
        => vec![],
        importDeclarations[mut dcls] importDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Top-level type declarations ($7.6)
    // Note that Joos 1W only supports one of these per file, but I leave that to the
    // weeding phase so it's easier to output a clearer error message.
    typeDeclaration: TypeDeclaration {
        classDeclaration[class] => TypeDeclaration::Class(class),
        interfaceDeclaration[interface] => TypeDeclaration::Interface(interface),
    }

    typeDeclarations: Vec<TypeDeclaration> {
        => vec![],
        typeDeclarations[mut dcls] typeDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Literals ($3.10)
    literal: Literal {
        IntegerLiteral(lit) => Literal::Integer(lit),
        BooleanLiteral(lit) => Literal::Boolean(lit),
        CharacterLiteral(lit) => Literal::Character(lit),
        StringLiteral(lit) => Literal::String(lit),
        NullLiteral => Literal::Null,
    }

    // Types ($4.1 - $4.3)
    // Since type is a Rust keyword, use the term ty.
    // Note that multidimensional arrays are not in Joos 1W.
    ty: Type {
        primitiveType[t] => Type::SimpleType(t),
        referenceType[t] => t,
    }

    primitiveType: SimpleType {
        BOOLEAN => SimpleType::Boolean,
        INT => SimpleType::Int,
        SHORT => SimpleType::Short,
        CHAR => SimpleType::Char,
        BYTE => SimpleType::Byte,
    }

    referenceType: Type {
        arrayType[t] => Type::ArrayType(t),
        typeName[t] => Type::SimpleType(t),
    }

    arrayType: SimpleType {
        primitiveType[t] LBracket RBracket => t,
        expressionNameOrType[q] LBracket RBracket => q.into(),
    }

    typeName: SimpleType {
        qualifiedIdentifier[q] => SimpleType::Other(q),
    }

    // Needed to resolve shift-reduce conflicts due of the Type Dot Something
    // because Type can be a qualifiedIdentifier
    // TODO: Remove
    // tyDot: Type {
    //     primitiveType[t] Dot => Type::SimpleType(t),
    //     arrayType[t] Dot => Type::ArrayType(t),
    //     qualifiedIdentifierHelperDot[q] => {
    //         let ident = QualifiedIdentifier { parts: q };
    //         Type::SimpleType(SimpleType::Other(ident))
    //     }
    // }


    // Identifiers ($6.7)
    qualifiedIdentifier: QualifiedIdentifier {
        qualifiedIdentifierHelper[list] => QualifiedIdentifier { parts: list }
    }

    qualifiedIdentifierHelperDot: Vec<String> {
        qualifiedIdentifierHelper[list] Dot => list,
    }

    qualifiedIdentifierHelper: Vec<String> {
        Identifier(ident) => vec![ident],
        qualifiedIdentifierHelperDot[mut list] Identifier(ident) => {
            list.push(ident);
            list
        }
    }

    // Classes ($8.1)
    classDeclaration : Class {
        modifierList[mods] CLASS Identifier(ident) superType[s]
                interfaceImplementations[impls] classBody[body] =>
            Class {
                name: ident,
                modifiers: mods,
                extends: s,
                implements: impls,
                body: body,
            },
    }

    superType: Option<QualifiedIdentifier> {
        => None,
        EXTENDS qualifiedIdentifier[extension] => Some(extension)
    }

    interfaceImplementations: Vec<QualifiedIdentifier> {
        => vec![],
        IMPLEMENTS interfaceList[impls] => impls,
    }

    // Class body ($8.1.5)
    classBody: Vec<ClassBodyDeclaration> {
        LBrace classBodyDeclarations[body] RBrace => body
    }

    classBodyDeclarations: Vec<ClassBodyDeclaration> {
        => vec![],
        classBodyDeclarations[dcls] Semicolon => { dcls }
        classBodyDeclarations[mut dcls] classBodyDeclaration[dcl] => { dcls.push(dcl); dcls },
    }

    classBodyDeclaration: ClassBodyDeclaration {
        fieldDeclaration[dcl] => ClassBodyDeclaration::FieldDeclaration(dcl),
        methodDeclaration[dcl] => ClassBodyDeclaration::MethodDeclaration(dcl),
        constructorDeclaration[dcl] => ClassBodyDeclaration::ConstructorDeclaration(dcl),
    }

    // Field declaration ($8.3)
    // Multiple fields per declarations not required.
    fieldDeclaration: Field {
        modifierList[mods] ty[t] variableDeclarator[(name, expr)] Semicolon => {
            Field { name: name, modifiers: mods, ty: t, initializer: expr }
        }
    }

    variableDeclarator: (String, Option<VariableInitializer>) {
        Identifier(name) => (name, None),
        Identifier(name) Assignment variableInitializer[init] => (name, Some(init)),
    }

    methodDeclaration: Method {
        modifierList[mods] VOID Identifier(name)
                LParen parameterList[params] RParen block[b] =>
            Method { name: name, modifiers: mods, params: params, return_type: None, body: Some(b) },
        modifierList[mods] ty[t] Identifier(name)
                LParen parameterList[params] RParen block[b] =>
            Method { name: name, modifiers: mods, params: params, return_type: Some(t), body: Some(b) },
        methodDeclarationNoBody[dcl] => dcl,
    }

    // Method declaration ($8.4)
    methodDeclarationNoBody: Method {
        modifierList[mods] VOID Identifier(name)
                LParen parameterList[params] RParen Semicolon =>
            Method { name: name, modifiers: mods, params: params, return_type: None, body: None },
        modifierList[mods] ty[t] Identifier(name)
                LParen parameterList[params] RParen Semicolon =>
            Method { name: name, modifiers: mods, params: params, return_type: Some(t), body: None },
    }

    // Class constructor ($8.8)
    constructorDeclaration: Constructor {
        modifierList[mods] Identifier(name) LParen parameterList[params] RParen block[b] =>
            Constructor { name: name, modifiers: mods, params: params, body: b },
    }

    // Interfaces ($9.1)
    interfaceDeclaration: Interface {
        modifierList[mods] INTERFACE Identifier(ident)
                interfaceExtensions[exts] interfaceBody[body] =>
            Interface {
                name: ident,
                modifiers: mods,
                extends: exts,
                body: body,
            },
    }

    interfaceExtensions: Vec<QualifiedIdentifier> {
        => vec![],
        EXTENDS interfaceList[impls] => impls,
    }

    // Interface body ($9.1.3)
    interfaceBody: Vec<Method> {
        LBrace interfaceMemberDeclarations[dcls] RBrace => dcls
    }

    interfaceMemberDeclarations: Vec<Method> {
        => vec![],
        interfaceMemberDeclarations[dcls] Semicolon => dcls,

        // Since we're not supposed to support nested type nor interface constants
        // in Joos 1W, there's only one type of interface member.
        interfaceMemberDeclarations[mut dcls] methodDeclarationNoBody[dcl] =>
            { dcls.push(dcl); dcls },
    }

    // Common to classes and interfaces

    interfaceList: Vec<QualifiedIdentifier> {
        qualifiedIdentifier[i] => vec![i],
        interfaceList[mut impls] Comma qualifiedIdentifier[i] => { impls.push(i); impls }
    }

    modifierList: Vec<Modifier> {
        => vec![],
        modifierList[mut list] modifier[m] => { list.push(m); list }
    }

    modifier: Modifier {
        PUBLIC[_] => Modifier::Public,
        PROTECTED[_] => Modifier::Protected,
        PRIVATE[_] => Modifier::Private,
        ABSTRACT[_] => Modifier::Abstract,
        STATIC[_] => Modifier::Static,
        FINAL[_] => Modifier::Final,
        NATIVE[_] => Modifier::Native,
    }

    // TODO: Check this comment.
    // For array types ($8.3)
    variableDeclaration: VariableDeclaration {
        ty[t] Identifier(name) => VariableDeclaration { ty: t, name: name }
    }

    // Method parameters ($8.4.1). The reference refers to them as formal parameters.
    parameterList: Vec<VariableDeclaration> {
        => vec![],
        variableDeclaration[p] => vec![p],
        parameterList[mut list] Comma variableDeclaration[p] => { list.push(p); list },
    }

    // Variable initializers ($8.3)
    // Note that array initializers ("array data expressions") not in Joos 1W.
    variableInitializer: VariableInitializer {
        expression[expr] => VariableInitializer::Expression(expr),
    }

    //
    // Expressions ($15)
    //
    expressionNameOrType: ExpressionOrType {
        qualifiedIdentifier[q] => ExpressionOrType::Name(q),
    }

    // Primary expression ($15.8)
    // Note that class literals are not needed.
    primary: Expression {
        primaryNoNewArray[expr] => expr,
        arrayCreationExpression[expr] => expr,
    }

    primaryNoNewArray: Expression {
        literal[expr] => Expression::Literal(expr),
        THIS => Expression::This,
        qualifiedIdentifierHelperDot[q] THIS => {
            let ident = QualifiedIdentifier { parts: q };
            Expression::QualifiedThis(ident)
        },
        LParen expressionNameOrType[expr] RParen => expr.into(),
        LParen expressionNotName[expr] RParen => expr,
        classInstanceCreationExpression[expr] => expr,
        fieldAccess[expr] => expr,
        methodInvocation[expr] => expr,
        arrayAccess[expr] => expr,
    }

    // Class instance creation expression ($15.9)
    classInstanceCreationExpression: Expression {
        NEW qualifiedIdentifier[q] LParen argumentList[args] RParen =>
            Expression::NewStaticClass(q, args, None),
        primary[expr] Dot NEW Identifier(ident) LParen argumentList[args] RParen =>
            Expression::NewDynamicClass(box expr, ident, args, None),
        // TODO: Confirm that anonymous classes count as nested types.
        // NEW qualifiedIdentifier[q] LParen argumentList[args] RParen classBody[body] =>
        //     Expression::NewStaticClass(q, args, Some(body)), // "Somebody har har har"
        // primary[expr] Dot NEW Identifier(ident)
        //         LParen argumentList[args] RParen classBody[body] =>
        //     Expression::NewDynamicClass(box expr, args, Some(body)),
    }

    argumentList: Vec<Expression> {
        => vec![],
        expression[expr] => vec![expr],
        argumentList[mut args] Comma expression[expr] => { args.push(expr); args },
    }

    // Array creation expression ($15.10)
    // Note that Joos 1W only has 1D arrays and does not support array initializers.
    arrayCreationExpression: Expression {
        NEW primitiveType[t] LBracket expression[expr] RBracket =>
            Expression::NewArray(t, box expr),
        NEW typeName[t] LBracket expression[expr] RBracket =>
            Expression::NewArray(t, box expr),
    }

    // Field access expressions ($15.11)
    // Super field access not required in Joos 1W
    fieldAccess: Expression {
        primary[expr] Dot Identifier(ident) => Expression::FieldAccess(box expr, ident),
    }

    // Method invocation expressions ($15.12)
    methodInvocation: Expression {
        expressionNameOrType[ExpressionOrType::Name(mut ids)] LParen argumentList[args] RParen => {
            let name = ids.parts.pop().unwrap();
            match ids.parts.len() {
                0 => Expression::MethodInvocation(None, name, args),
                _ => Expression::MethodInvocation(Some(box Expression::Name(ids)), name, args),
            }
        }
        primary[expr] Dot Identifier(ident) LParen argumentList[args] RParen =>
            Expression::MethodInvocation(Some(box expr), ident, args),
    }

    // Array access expressions ($15.13)
    arrayAccess: Expression {
        expressionNameOrType[name] LBracket expression[expr] RBracket =>
            Expression::ArrayAccess(box name.into(), box expr),
        primaryNoNewArray[primary] LBracket expression[expr] RBracket =>
            Expression::ArrayAccess(box primary, box expr),
    }

    // Postfix expression (%15.14)
    // No -- or ++ in Joss 1W
    postfixExpression: Expression {
        primary[expr] => expr,
        expressionNameOrType[expr] => expr.into(),
    }

    // Unary operators ($15.15)
    unaryExpression: Expression {
        Minus unaryExpression[expr] => Expression::Prefix(PrefixOperator::Minus, box expr),
        unaryExpressionNotPlusMinus[expr] => expr
    }

    // Separate rule due to casting rules.
    unaryExpressionNotPlusMinus: Expression {
        castExpression[expr] => expr,
        postfixExpression[expr] => expr,
        Bang unaryExpression[expr] => Expression::Prefix(PrefixOperator::Not, box expr),
    }

    // Casting ($15.16)
    // Note that the grammar in the ref appears to have a mistake (I don't think
    // there should be an Dim_opt since it would make it a reference type, plus
    // it doesn't make sense to cast something that supports unary minus to an
    // array type.)
    castExpression: Expression {
        LParen primitiveType[t] RParen unaryExpression[expr] =>
            Expression::Cast(Type::SimpleType(t), box expr),
        LParen arrayType[t] RParen unaryExpressionNotPlusMinus[expr] =>
            Expression::Cast(Type::ArrayType(t), box expr),
        LParen expressionNameOrType[q] RParen unaryExpressionNotPlusMinus[expr] =>
            Expression::Cast(q.into(), box expr),
    }

    // Multiplicative expressions ($15.17)
    multiplicativeExpression: Expression {
        unaryExpression[expr] => expr,

        multiplicativeExpression[expr1] Star unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Mult, box expr1, box expr2),
        multiplicativeExpression[expr1] Slash unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Div, box expr1, box expr2),
        multiplicativeExpression[expr1] Percent unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Modulo, box expr1, box expr2),
    }

    // Additive expressions ($15.18)
    additiveExpression: Expression {
        multiplicativeExpression[expr] => expr,
        additiveExpression[expr1] Plus multiplicativeExpression[expr2] =>
            Expression::Infix(InfixOperator::Plus, box expr1, box expr2),
        additiveExpression[expr1] Minus multiplicativeExpression[expr2] =>
            Expression::Infix(InfixOperator::Minus, box expr1, box expr2),
    }

    // Shift operators ($15.19)
    // Not supported in Joos 1W, so we skip shiftExpression and go directly
    // from relationalExpression to additiveExpression.

    // Relational operators ($15.20)
    relationalExpression: Expression {
        additiveExpression[expr] => expr,
        relationalExpression[expr1] comparisonOperator[op] additiveExpression[expr2] =>
            Expression::Infix(op, box expr1, box expr2),
        relationalExpression[expr] INSTANCEOF referenceType[t] =>
            Expression::InstanceOf(box expr, t),
    }

    comparisonOperator: InfixOperator {
        LessThan => InfixOperator::LessThan,
        GreaterThan => InfixOperator::GreaterThan,
        LessEqual => InfixOperator::LessEqual,
        GreaterEqual => InfixOperator::GreaterEqual,
    }

    // Equality operator ($15.21)
    equalityExpression: Expression {
        relationalExpression[expr] => expr,
        equalityExpression[expr1] Equals relationalExpression[expr2] =>
            Expression::Infix(InfixOperator::Equals, box expr1, box expr2),
        equalityExpression[expr1] NotEquals relationalExpression[expr2] =>
            Expression::Infix(InfixOperator::NotEquals, box expr1, box expr2),
    }

    // Logical operators ($15.22)
    andExpression: Expression {
        equalityExpression[expr] => expr,
        andExpression[expr1] And equalityExpression[expr2] =>
            Expression::Infix(InfixOperator::EagerAnd, box expr1, box expr2),
    }

    exclusiveOrExpression: Expression {
        andExpression[expr] => expr,
        exclusiveOrExpression[expr1] Xor andExpression[expr2] =>
            Expression::Infix(InfixOperator::Xor, box expr1, box expr2),
    }

    inclusiveOrExpression: Expression {
        exclusiveOrExpression[expr] => expr,
        inclusiveOrExpression[expr1] Or exclusiveOrExpression[expr2] =>
            Expression::Infix(InfixOperator::EagerOr, box expr1, box expr2),
    }

    // Skipping the ternary operator ($15.25)

    // Conditional operators ($15.23) ($15.24)
    conditionalAndExpression: Expression {
        inclusiveOrExpression[expr] => expr,
        conditionalAndExpression[expr1] AndAnd inclusiveOrExpression[expr2] =>
            Expression::Infix(InfixOperator::LazyAnd, box expr1, box expr2),
    }

    conditionalOrExpression: Expression {
        conditionalAndExpression[expr] => expr,
        conditionalOrExpression[expr1] OrOr conditionalAndExpression[expr2] =>
            Expression::Infix(InfixOperator::LazyOr, box expr1, box expr2),
    }

    // Assignment operator ($15.26)
    assignmentExpression: Expression {
        conditionalOrExpression[expr] => expr,
        assignment[a] => a,
    }

    assignment: Expression {
        leftHandSide[lhs] Assignment conditionalOrExpression[expr] =>
            Expression::Assignment(box lhs, box expr),
    }

    leftHandSide: Expression {
        qualifiedIdentifier[q] => Expression::Name(q),
        fieldAccess[expr] => expr,
        arrayAccess[expr] => expr,
    }

    // Expressions:
    expression: Expression {
        assignmentExpression[expr] => expr,
    }
    // Same as `expression`, but can't expand into just `expressionName`.
    // All the rules are inlined.
    expressionNotName: Expression {
        Minus unaryExpression[expr] => Expression::Prefix(PrefixOperator::Minus, box expr),
        castExpression[expr] => expr,
        primary[expr] => expr,
        Bang unaryExpression[expr] => Expression::Prefix(PrefixOperator::Not, box expr),

        multiplicativeExpression[expr1] Star unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Mult, box expr1, box expr2),
        multiplicativeExpression[expr1] Slash unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Div, box expr1, box expr2),
        multiplicativeExpression[expr1] Percent unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Modulo, box expr1, box expr2),
        additiveExpression[expr1] Plus multiplicativeExpression[expr2] =>
            Expression::Infix(InfixOperator::Plus, box expr1, box expr2),
        additiveExpression[expr1] Minus multiplicativeExpression[expr2] =>
            Expression::Infix(InfixOperator::Minus, box expr1, box expr2),
        relationalExpression[expr1] comparisonOperator[op] additiveExpression[expr2] =>
            Expression::Infix(op, box expr1, box expr2),
        relationalExpression[expr] INSTANCEOF referenceType[t] =>
            Expression::InstanceOf(box expr, t),
        equalityExpression[expr1] Equals relationalExpression[expr2] =>
            Expression::Infix(InfixOperator::Equals, box expr1, box expr2),
        equalityExpression[expr1] NotEquals relationalExpression[expr2] =>
            Expression::Infix(InfixOperator::NotEquals, box expr1, box expr2),
        andExpression[expr1] And equalityExpression[expr2] =>
            Expression::Infix(InfixOperator::EagerAnd, box expr1, box expr2),
        exclusiveOrExpression[expr1] Xor andExpression[expr2] =>
            Expression::Infix(InfixOperator::Xor, box expr1, box expr2),
        inclusiveOrExpression[expr1] Or exclusiveOrExpression[expr2] =>
            Expression::Infix(InfixOperator::EagerOr, box expr1, box expr2),
        conditionalAndExpression[expr1] AndAnd inclusiveOrExpression[expr2] =>
            Expression::Infix(InfixOperator::LazyAnd, box expr1, box expr2),
        conditionalOrExpression[expr1] OrOr conditionalAndExpression[expr2] =>
            Expression::Infix(InfixOperator::LazyOr, box expr1, box expr2),
        assignment[a] => a,
    }

    // Block ($14.2)
    block: Block {
        LBrace blockStatements[stmts] RBrace => Block { stmts: stmts }
    }

    blockStatements: Vec<BlockStatement> {
        => vec![],
        blockStatements[mut stmts] blockStatement[stmt] => { stmts.push(stmt); stmts }
    }

    blockStatement: BlockStatement {
        localVariableDeclaration[local] Semicolon => BlockStatement::LocalVariable(local),
        classDeclaration[c] => BlockStatement::LocalClass(c),
        statement[s] => BlockStatement::Statement(s),
    }

    // Local declarations ($14.4)
    localVariableDeclaration: LocalVariable {
        variableDeclaration[var] Assignment variableInitializer[init] =>
            LocalVariable { variable: var, initializer: init }
    }

    // Statements ($14.5)
    statement: Statement {
        block[b] => Statement::Block(b),
        #[no_reduce(ELSE)]
        IF LParen expression[test] RParen statement[s1] =>
            Statement::If(test, box s1, None),
        IF LParen expression[test] RParen statement[s1] ELSE statement[s2] =>
            Statement::If(test, box s1, Some(box s2)),
        WHILE LParen expression[test] RParen statement[body] =>
            Statement::While(test, box body),
        FOR LParen maybeStatementExpression[f1] Semicolon maybeExpression[f2] Semicolon maybeStatementExpression[f3] RParen statement[body] =>
            Statement::For(f1, f2, f3, box body),
        FOR LParen localVariableDeclaration[f1] Semicolon maybeExpression[f2] Semicolon maybeStatementExpression[f3] RParen statement[body] =>
            Statement::ForDecl(f1, f2, f3, box body),
        Semicolon => Statement::Empty,
        statementExpression[expr] Semicolon => Statement::Expression(expr),
        RETURN expression[expr] Semicolon => Statement::Return(expr),
    }

    statementExpression: Expression {
        assignment[e] => e,
        methodInvocation[e] => e,
        classInstanceCreationExpression[e] => e,
    }

    maybeStatementExpression: Option<Expression> {
        statementExpression[e] => Some(e),
        => None,
    }

    maybeExpression: Option<Expression> {
        expression[e] => Some(e),
        => None,
    }
}

pub fn make_ast<I: Iterator<Item=Token>>(tokens: I)
        -> Result<CompilationUnit, (Option<Token>, &'static str)> {
    parse(tokens)
}

// An intermediate type for parsing. Represents syntax that can be interpreted as
// either an expression or a type.
#[derive(Show)]
pub enum ExpressionOrType {
    Name(QualifiedIdentifier),
}

pub trait IsExpressionOrType {
    fn convert(ExpressionOrType) -> Self;
}
impl IsExpressionOrType for Expression {
    fn convert(x: ExpressionOrType) -> Expression {
        match x {
            ExpressionOrType::Name(n) => Expression::Name(n),
        }
    }
}
impl IsExpressionOrType for SimpleType {
    fn convert(x: ExpressionOrType) -> SimpleType {
        match x {
            ExpressionOrType::Name(n) => SimpleType::Other(n),
        }
    }
}
impl IsExpressionOrType for Type {
    fn convert(x: ExpressionOrType) -> Type {
        Type::SimpleType(x.into())
    }
}
impl ExpressionOrType {
    pub fn into<T: IsExpressionOrType>(self) -> T { IsExpressionOrType::convert(self) }
}
