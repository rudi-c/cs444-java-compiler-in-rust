use ast::*;
use tokenizer::*;
use tokenizer::Token::*;

// See Chapter 18 for full grammar (p.449) (pdf p.475)

parser! parse {
    Token;

    // Compilation unit ($7.3)
    root: CompilationUnit {
        packageDeclaration[pkg] importDeclarations[imports] typeDeclarations[types] =>
            CompilationUnit { packages: pkg,
                              imports: imports,
                              types: types },
    }

    // Package declarations ($7.4)
    packageDeclaration: QualifiedIdentifier {
        PACKAGE qualifiedIdentifier[ident] Semicolon => ident,
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
    // Since type is a Rust keyword, use the term jType.
    // Note that multidimensional arrays are not in Joos 1W.
    jType: Type {
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
        qualifiedIdentifier[q] LBracket RBracket => SimpleType::Other(q),
    }

    typeName: SimpleType {
        qualifiedIdentifier[q] => SimpleType::Other(q),
    }

    // Needed to resolve shift-reduce conflicts due of the Type Dot Something
    // because Type can be a qualifiedIdentifier
    // TODO: Remove
    // jTypeDot: Type {
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
        modifierList[mods] jType[t] variableDeclarator[v] Semicolon => {
            let (name, expr) = v;
            Field { name: name, modifiers: mods, jType: t, initializer: expr }
        }
    }

    variableDeclarator: (String, Option<VariableInitializer>) {
        Identifier(name) => (name, None),
        Identifier(name) Assignment variableInitializer[init] => (name, Some(init)),
    }

    methodDeclaration: Method {
        modifierList[mods] VOID Identifier(name)
                LParen parameterList[params] RParen block[b] =>
            Method { name: name, modifiers: mods, params: params, returnType: None, body: b },
        modifierList[mods] jType[t] Identifier(name)
                LParen parameterList[params] RParen block[b] =>
            Method { name: name, modifiers: mods, params: params, returnType: Some(t), body: b }
    }

    // Class constructor ($8.8)
    constructorDeclaration: Constructor {
        modifierList[mods] Identifier(name) LParen parameterList[params] RParen block[b] =>
            Constructor { name: name, modifiers: mods, params: params, body: b },
    }

    // Interfaces ($9.1)
    interfaceDeclaration: Interface {
        modifierList[mods] INTERFACE Identifier(ident)
                interfaceExtensions[exts] interfaceBody[x] =>
            Interface {
                name: ident,
                modifiers: mods,
                extends: exts,
            },
    }

    interfaceExtensions: Vec<QualifiedIdentifier> {
        => vec![],
        EXTENDS interfaceList[impls] => impls,
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
    }

    // Interface body ($9.1.3)
    interfaceBody: i32 {
        LBrace RBrace => 0
    }

    // TODO: Check this comment.
    // For array types ($8.3)
    variableDeclaration: VariableDeclaration {
        jType[t] Identifier(name) => VariableDeclaration { jType: t, name: name }
    }

    // Method parameters ($8.4.1). The reference refers to them as formal parameters.
    parameterList: Vec<VariableDeclaration> {
        => vec![],
        variableDeclaration[p] => vec![p],
        parameterList[mut list] Comma variableDeclaration[p] => { list.push(p); list },
    }

    // Variable initializers ($8.3)
    variableInitializer: VariableInitializer {
        expression[expr] => VariableInitializer::Expression(expr),
        arrayInitializer[init] => VariableInitializer::Array(init),
    }

    // Array initializers ($10.6)
    // TODO: Check if test cases have any of these, but I think that "no array data"
    //       means that there shouldn't be?
    // TODO: Will want test cases in regards to the last comma.
    // TODO: I think we're not supposed to have nested initializers.
    arrayInitializer: Vec<VariableInitializer> {
        LBrace RBrace => vec![],
        LBrace variableInitializers[inits] RBrace => inits,
        LBrace variableInitializers[inits] Comma RBrace => inits,
    }

    variableInitializers: Vec<VariableInitializer> {
        variableInitializer[init] => vec![init],
        variableInitializers[mut inits] Comma variableInitializer[init] =>
            { inits.push(init); inits }
    }

    //
    // Expressions ($15)
    //
    expressionName: Expression {
        qualifiedIdentifier[q] => Expression::Name(q),
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
        LParen expression[expr] RParen => expr,
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
            Expression::NewDynamicClass(box expr, args, None),
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
    // Note that Joos 1W only has 1D arrays.
    arrayCreationExpression: Expression {
        NEW primitiveType[t] LBracket expression[expr] RBracket =>
            Expression::NewArray(t, box expr),
        NEW typeName[t] LBracket expression[expr] RBracket =>
            Expression::NewArray(t, box expr),
        NEW primitiveType[t] LBracket expression[expr] RBracket arrayInitializer[init] =>
            Expression::NewArrayInit(t, init),
        NEW typeName[t] LBracket expression[expr] RBracket arrayInitializer[init] =>
            Expression::NewArrayInit(t, init),
    }

    // Field access expressions ($15.11)
    // Super field access not required in Joos 1W
    fieldAccess: Expression {
        primary[expr] Dot Identifier(ident) => Expression::FieldAccess(box expr, ident),
    }

    // Method invocation expressions ($15.12)
    methodInvocation: Expression {
        Identifier(ident) LParen argumentList[args] RParen =>
            Expression::MethodInvocation(None, ident, args),
        primary[expr] Dot Identifier(ident) LParen argumentList[args] RParen =>
            Expression::MethodInvocation(Some(box expr), ident, args),
    }

    // Array access expressions ($15.13)
    arrayAccess: Expression {
        expressionName[name] LBracket expression[expr] RBracket =>
            Expression::ArrayAccess(box name, box expr),
        primaryNoNewArray[primary] LBracket expression[expr] RBracket =>
            Expression::ArrayAccess(box primary, box expr),
    }

    // Postfix expression (%15.14)
    // No -- or ++ in Joss 1W
    postfixExpression: Expression {
        primary[expr] => expr,
        expressionName[expr] => expr,
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
        LParen referenceType[t] RParen unaryExpressionNotPlusMinus[expr] =>
            Expression::Cast(t, box expr),
    }

    // Multiplicative expressions ($15.17)
    multiplicativeExpression: Expression {
        unaryExpression[expr] => expr,

        multiplicativeExpression[expr1] Star unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Mult, box expr1, box expr2),
        multiplicativeExpression[expr1] Slash unaryExpression[expr2] =>
            Expression::Infix(InfixOperator::Div, box expr1, box expr2),
        multiplicativeExpression[expr1] Percentage unaryExpression[expr2] =>
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
            Expression::Assignment(lhs, box expr),
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

    // Block ($14.2)
    block: Vec<BlockStatement> {
        LBrace blockStatements[stmts] RBrace => stmts
    }

    blockStatements : Vec<BlockStatement> {
        => vec![],
        blockStatements[mut stmts] blockStatement[stmt] => { stmts.push(stmt); stmts }
    }

    blockStatement : BlockStatement {
        localVariableDeclaration[local] => BlockStatement::LocalVariable(local),
        classDeclaration[c] => BlockStatement::LocalClass(c),
    }

    // Local declarations ($14.4)
    localVariableDeclaration : LocalVariable {
        variableDeclaration[var] Assignment variableInitializer[init] Semicolon =>
            LocalVariable { variable: var, initializer: init }
    }
}

pub fn make_ast<I: Iterator<Item=Token>>(tokens: I)
        -> Result<CompilationUnit, (Option<Token>, &'static str)> {
    parse(tokens)
}

