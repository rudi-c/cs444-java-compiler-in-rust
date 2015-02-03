use ast::*;
use tokenizer::*;
use tokenizer::Token::*;

// See Chapter 18 for full grammar (p.449) (pdf p.475)

macro_rules! spanned {
    ($node: expr) => (spanned(span!(), $node));
}

// FIXME(spans)
macro_rules! span {
    () => (Span { lo: 0, hi: 0, file: () });
}

// FIXME(spans)
fn span_of<T>(_: &T) -> Span {
    span!()
}

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
        IMPORT qualifiedIdentifier[ident] Semicolon =>
            spanned!(ImportDeclaration_::SingleType(ident)),
        IMPORT qualifiedIdentifierHelperDot[ident] Star Semicolon =>
            spanned!(ImportDeclaration_::OnDemand(QualifiedIdentifier::new(ident))),
    }

    importDeclarations: Vec<ImportDeclaration> {
        => vec![],
        importDeclarations[mut dcls] importDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Top-level type declarations ($7.6)
    // Note that Joos 1W only supports one of these per file, but I leave that to the
    // weeding phase so it's easier to output a clearer error message.
    typeDeclaration: TypeDeclaration {
        classDeclaration[class] =>
            spanned!(TypeDeclaration_::Class(class)),
        interfaceDeclaration[interface] =>
            spanned!(TypeDeclaration_::Interface(interface)),
    }

    typeDeclarations: Vec<TypeDeclaration> {
        => vec![],
        typeDeclarations[mut dcls] typeDeclaration[dcl] => { dcls.push(dcl); dcls }
    }

    // Literals ($3.10)
    literal: Spanned<Literal> {
        IntegerLiteral(lit) => spanned!(Literal::Integer(lit)),
        BooleanLiteral(lit) => spanned!(Literal::Boolean(lit)),
        CharacterLiteral(lit) => spanned!(Literal::Character(lit)),
        StringLiteral(lit) => spanned!(Literal::String(lit)),
        NullLiteral => spanned!(Literal::Null),
    }

    // Types ($4.1 - $4.3)
    // Since type is a Rust keyword, use the term ty.
    // Note that multidimensional arrays are not in Joos 1W.
    ty: Type {
        primitiveType[t] => spanned!(Type_::SimpleType(t)),
        referenceType[t] => t,
    }

    primitiveType: SimpleType {
        BOOLEAN => spanned!(SimpleType_::Boolean),
        INT => spanned!(SimpleType_::Int),
        SHORT => spanned!(SimpleType_::Short),
        CHAR => spanned!(SimpleType_::Char),
        BYTE => spanned!(SimpleType_::Byte),
    }

    referenceType: Type {
        arrayType[t] => spanned!(Type_::ArrayType(t)),
        typeName[t] => spanned!(Type_::SimpleType(t)),
        NullLiteral => spanned!(Type_::Null),
    }

    arrayType: SimpleType {
        primitiveType[t] LBracket RBracket => t,
        expressionNameOrType[q] LBracket RBracket => q.into(),
    }

    typeName: SimpleType {
        qualifiedIdentifier[q] => spanned!(SimpleType_::Other(q)),
    }

    // Needed to resolve shift-reduce conflicts due of the Type Dot Something
    // because Type can be a qualifiedIdentifier
    // TODO: Remove
    // tyDot: Type {
    //     primitiveType[t] Dot => Type_::SimpleType(t),
    //     arrayType[t] Dot => Type_::ArrayType(t),
    //     qualifiedIdentifierHelperDot[q] => {
    //         let ident = QualifiedIdentifier { parts: q };
    //         Type_::SimpleType(SimpleType_::Other(ident))
    //     }
    // }


    // Identifiers ($6.7)
    identifier: Ident {
        Identifier(ident) => spanned!(ident),
    }

    qualifiedIdentifier: QualifiedIdentifier {
        qualifiedIdentifierHelper[list] =>
            spanned!(QualifiedIdentifier_ { parts: list })
    }

    qualifiedIdentifierHelperDot: Vec<Ident> {
        qualifiedIdentifierHelper[list] Dot => list,
    }

    qualifiedIdentifierHelper: Vec<Ident> {
        identifier[ident] => vec![ident],
        qualifiedIdentifierHelperDot[mut list] identifier[ident] => {
            list.push(ident);
            list
        }
    }

    // Classes ($8.1)
    classDeclaration : Class {
        modifierList[mods] CLASS identifier[ident] superType[s]
                interfaceImplementations[impls] classBody[body] =>
            spanned!(Class_ {
                name: ident,
                modifiers: mods,
                extends: s,
                implements: impls,
                body: body,
            }),
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
        fieldDeclaration[dcl] =>
            spanned!(ClassBodyDeclaration_::FieldDeclaration(dcl)),
        methodDeclaration[dcl] =>
            spanned!(ClassBodyDeclaration_::MethodDeclaration(dcl)),
        constructorDeclaration[dcl] =>
            spanned!(ClassBodyDeclaration_::ConstructorDeclaration(dcl)),
    }

    // Field declaration ($8.3)
    // Multiple fields per declarations not required.
    fieldDeclaration: Field {
        modifierList[mods] ty[t] variableDeclarator[(name, expr)] Semicolon => {
            spanned!(Field_ {
                name: name,
                modifiers: mods,
                ty: t,
                initializer: expr,
            })
        }
    }

    variableDeclarator: (Ident, Option<Expression>) {
        identifier[name] => (name, None),
        identifier[name] Assignment variableInitializer[init] => (name, Some(init)),
    }

    methodDeclaration: Method {
        modifierList[mods] VOID identifier[name]
                LParen parameterList[params] RParen block[b] =>
            spanned!(Method_ { name: name, modifiers: mods, params: params, return_type: None, body: Some(b) }),
        modifierList[mods] ty[t] identifier[name]
                LParen parameterList[params] RParen block[b] =>
            spanned!(Method_ { name: name, modifiers: mods, params: params, return_type: Some(t), body: Some(b) }),
        methodDeclarationNoBody[dcl] => dcl,
    }

    // Method declaration ($8.4)
    methodDeclarationNoBody: Method {
        modifierList[mods] VOID identifier[name]
                LParen parameterList[params] RParen Semicolon =>
            spanned!(Method_ { name: name, modifiers: mods, params: params, return_type: None, body: None }),
        modifierList[mods] ty[t] identifier[name]
                LParen parameterList[params] RParen Semicolon =>
            spanned!(Method_ { name: name, modifiers: mods, params: params, return_type: Some(t), body: None }),
    }

    // Class constructor ($8.8)
    constructorDeclaration: Constructor {
        modifierList[mods] identifier[name] LParen parameterList[params] RParen block[b] =>
            spanned!(Constructor_ { name: name, modifiers: mods, params: params, body: b }),
    }

    // Interfaces ($9.1)
    interfaceDeclaration: Interface {
        modifierList[mods] INTERFACE identifier[ident]
                interfaceExtensions[exts] interfaceBody[body] =>
            spanned!(Interface_ {
                name: ident,
                modifiers: mods,
                extends: exts,
                body: body,
            }),
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
        PUBLIC[_]    => spanned!(Modifier_::Public),
        PROTECTED[_] => spanned!(Modifier_::Protected),
        PRIVATE[_]   => spanned!(Modifier_::Private),
        ABSTRACT[_]  => spanned!(Modifier_::Abstract),
        STATIC[_]    => spanned!(Modifier_::Static),
        FINAL[_]     => spanned!(Modifier_::Final),
        NATIVE[_]    => spanned!(Modifier_::Native),
    }

    // TODO: Check this comment.
    // For array types ($8.3)
    variableDeclaration: VariableDeclaration {
        ty[t] identifier[name] => spanned!(VariableDeclaration_ {
            ty: t, name: name
        }),
    }

    // Method parameters ($8.4.1). The reference refers to them as formal parameters.
    parameterList: Vec<VariableDeclaration> {
        => vec![],
        variableDeclaration[p] => vec![p],
        parameterList[mut list] Comma variableDeclaration[p] => { list.push(p); list },
    }

    // Variable initializers ($8.3)
    // Note that array initializers ("array data expressions") not in Joos 1W.
    variableInitializer: Expression {
        expression[expr] => expr,
    }

    //
    // Expressions ($15)
    //
    expressionNameOrType: ExpressionOrType {
        qualifiedIdentifier[q] => spanned!(ExpressionOrType_::Name(q)),
    }

    // Primary expression ($15.8)
    // Note that class literals are not needed.
    primary: Expression {
        primaryNoNewArray[expr] => expr,
        arrayCreationExpression[expr] => expr,
    }

    primaryNoNewArray: Expression {
        literal[lit] => spanned!(Expression_::Literal(lit.node)),
        THIS => spanned!(Expression_::This),
        qualifiedIdentifierHelperDot[q] THIS => {
            let ident = QualifiedIdentifier::new(q);
            spanned!(Expression_::QualifiedThis(ident))
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
            spanned!(Expression_::NewStaticClass(q, args, None)),
        primary[expr] Dot NEW identifier[ident] LParen argumentList[args] RParen =>
            spanned!(Expression_::NewDynamicClass(box expr, ident, args, None)),
        // TODO: Confirm that anonymous classes count as nested types.
        // NEW qualifiedIdentifier[q] LParen argumentList[args] RParen classBody[body] =>
        //     spanned!(Expression_::NewStaticClass(q, args, Some(body))), // "Somebody har har har"
        // primary[expr] Dot NEW identifier[ident]
        //         LParen argumentList[args] RParen classBody[body] =>
        //     spanned!(Expression_::NewDynamicClass(box expr, args, Some(body))),
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
            spanned!(Expression_::NewArray(t, box expr)),
        NEW typeName[t] LBracket expression[expr] RBracket =>
            spanned!(Expression_::NewArray(t, box expr)),
    }

    // Field access expressions ($15.11)
    // Super field access not required in Joos 1W
    fieldAccess: Expression {
        primary[expr] Dot identifier[ident] => spanned!(Expression_::FieldAccess(box expr, ident)),
    }

    // Method invocation expressions ($15.12)
    methodInvocation: Expression {
        expressionNameOrType[node!(ExpressionOrType_::Name(mut ids))] LParen argumentList[args] RParen => {
            let name = ids.node.parts.pop().unwrap();
            match ids.node.parts.len() {
                0 => spanned!(Expression_::MethodInvocation(None, name, args)),
                _ => {
                    let sp = Span::range(ids.node.parts.first().unwrap().span,
                                         ids.node.parts.last().unwrap().span);
                    ids.span = sp;
                    spanned!(Expression_::MethodInvocation(
                            Some(box spanned(sp, Expression_::Name(ids))), name, args))
                }
            }
        }
        primary[expr] Dot identifier[ident] LParen argumentList[args] RParen =>
            spanned!(Expression_::MethodInvocation(Some(box expr), ident, args)),
    }

    // Array access expressions ($15.13)
    arrayAccess: Expression {
        expressionNameOrType[name] LBracket expression[expr] RBracket =>
            spanned!(Expression_::ArrayAccess(box name.into(), box expr)),
        primaryNoNewArray[primary] LBracket expression[expr] RBracket =>
            spanned!(Expression_::ArrayAccess(box primary, box expr)),
    }

    // Postfix expression (%15.14)
    // No -- or ++ in Joss 1W
    postfixExpression: Expression {
        primary[expr] => expr,
        expressionNameOrType[expr] => expr.into(),
    }

    // Unary operators ($15.15)
    unaryExpression: Expression {
        Minus unaryExpression[expr] => spanned!(Expression_::Prefix(PrefixOperator::Minus, box expr)),
        unaryExpressionNotPlusMinus[expr] => expr
    }

    // Separate rule due to casting rules.
    unaryExpressionNotPlusMinus: Expression {
        castExpression[expr] => expr,
        postfixExpression[expr] => expr,
        Bang unaryExpression[expr] => spanned!(Expression_::Prefix(PrefixOperator::Not, box expr)),
    }

    // Casting ($15.16)
    // Note that the grammar in the ref appears to have a mistake (I don't think
    // there should be an Dim_opt since it would make it a reference type, plus
    // it doesn't make sense to cast something that supports unary minus to an
    // array type.)
    castExpression: Expression {
        LParen primitiveType[t] RParen unaryExpression[expr] =>
            spanned!(Expression_::Cast(spanned(span_of(&t), Type_::SimpleType(t)), box expr)),
        LParen arrayType[t] RParen unaryExpressionNotPlusMinus[expr] =>
            spanned!(Expression_::Cast(spanned(span_of(&t), Type_::ArrayType(t)), box expr)),
        LParen expressionNameOrType[q] RParen unaryExpressionNotPlusMinus[expr] =>
            spanned!(Expression_::Cast(q.into(), box expr)),
    }

    // Multiplicative expressions ($15.17)
    multiplicativeExpression: Expression {
        unaryExpression[expr] => expr,

        multiplicativeExpression[expr1] multiplicativeOperator[op] unaryExpression[expr2] =>
            spanned!(Expression_::Infix(op, box expr1, box expr2)),
    }
    multiplicativeOperator: InfixOperator {
        Star => InfixOperator::Mult,
        Slash => InfixOperator::Div,
        Percent => InfixOperator::Modulo,
    }

    // Additive expressions ($15.18)
    additiveExpression: Expression {
        multiplicativeExpression[expr] => expr,
        additiveExpression[expr1] Plus multiplicativeExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Plus, box expr1, box expr2)),
        additiveExpression[expr1] Minus multiplicativeExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Minus, box expr1, box expr2)),
    }

    // Shift operators ($15.19)
    // Not supported in Joos 1W, so we skip shiftExpression and go directly
    // from relationalExpression to additiveExpression.

    // Relational operators ($15.20)
    relationalExpression: Expression {
        additiveExpression[expr] => expr,
        relationalExpression[expr1] comparisonOperator[op] additiveExpression[expr2] =>
            spanned!(Expression_::Infix(op, box expr1, box expr2)),
        relationalExpression[expr] INSTANCEOF referenceType[t] =>
            spanned!(Expression_::InstanceOf(box expr, t)),
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
            spanned!(Expression_::Infix(InfixOperator::Equals, box expr1, box expr2)),
        equalityExpression[expr1] NotEquals relationalExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::NotEquals, box expr1, box expr2)),
    }

    // Logical operators ($15.22)
    andExpression: Expression {
        equalityExpression[expr] => expr,
        andExpression[expr1] And equalityExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::EagerAnd, box expr1, box expr2)),
    }

    exclusiveOrExpression: Expression {
        andExpression[expr] => expr,
        exclusiveOrExpression[expr1] Xor andExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Xor, box expr1, box expr2)),
    }

    inclusiveOrExpression: Expression {
        exclusiveOrExpression[expr] => expr,
        inclusiveOrExpression[expr1] Or exclusiveOrExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::EagerOr, box expr1, box expr2)),
    }

    // Skipping the ternary operator ($15.25)

    // Conditional operators ($15.23) ($15.24)
    conditionalAndExpression: Expression {
        inclusiveOrExpression[expr] => expr,
        conditionalAndExpression[expr1] AndAnd inclusiveOrExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::LazyAnd, box expr1, box expr2)),
    }

    conditionalOrExpression: Expression {
        conditionalAndExpression[expr] => expr,
        conditionalOrExpression[expr1] OrOr conditionalAndExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::LazyOr, box expr1, box expr2)),
    }

    // Assignment operator ($15.26)
    assignmentExpression: Expression {
        conditionalOrExpression[expr] => expr,
        assignment[a] => a,
    }

    assignment: Expression {
        leftHandSide[lhs] Assignment conditionalOrExpression[expr] =>
            spanned!(Expression_::Assignment(box lhs, box expr)),
    }

    leftHandSide: Expression {
        qualifiedIdentifier[q] => spanned!(Expression_::Name(q)),
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
        Minus unaryExpression[expr] => spanned!(Expression_::Prefix(PrefixOperator::Minus, box expr)),
        castExpression[expr] => expr,
        primary[expr] => expr,
        Bang unaryExpression[expr] => spanned!(Expression_::Prefix(PrefixOperator::Not, box expr)),

        multiplicativeExpression[expr1] multiplicativeOperator[op] unaryExpression[expr2] =>
            spanned!(Expression_::Infix(op, box expr1, box expr2)),
        additiveExpression[expr1] Plus multiplicativeExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Plus, box expr1, box expr2)),
        additiveExpression[expr1] Minus multiplicativeExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Minus, box expr1, box expr2)),
        relationalExpression[expr1] comparisonOperator[op] additiveExpression[expr2] =>
            spanned!(Expression_::Infix(op, box expr1, box expr2)),
        relationalExpression[expr] INSTANCEOF referenceType[t] =>
            spanned!(Expression_::InstanceOf(box expr, t)),
        equalityExpression[expr1] Equals relationalExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Equals, box expr1, box expr2)),
        equalityExpression[expr1] NotEquals relationalExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::NotEquals, box expr1, box expr2)),
        andExpression[expr1] And equalityExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::EagerAnd, box expr1, box expr2)),
        exclusiveOrExpression[expr1] Xor andExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::Xor, box expr1, box expr2)),
        inclusiveOrExpression[expr1] Or exclusiveOrExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::EagerOr, box expr1, box expr2)),
        conditionalAndExpression[expr1] AndAnd inclusiveOrExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::LazyAnd, box expr1, box expr2)),
        conditionalOrExpression[expr1] OrOr conditionalAndExpression[expr2] =>
            spanned!(Expression_::Infix(InfixOperator::LazyOr, box expr1, box expr2)),
        assignment[a] => a,
    }

    // Block ($14.2)
    block: Block {
        LBrace blockStatements[stmts] RBrace => spanned!(Block_ { stmts: stmts })
    }

    blockStatements: Vec<BlockStatement> {
        => vec![],
        blockStatements[mut stmts] blockStatement[stmt] => { stmts.push(stmt); stmts }
    }

    blockStatement: BlockStatement {
        localVariableDeclaration[local] Semicolon => spanned!(BlockStatement_::LocalVariable(local)),
        classDeclaration[c] => spanned!(BlockStatement_::LocalClass(c)),
        statement[s] => spanned!(BlockStatement_::Statement(s)),
    }

    // Local declarations ($14.4)
    localVariableDeclaration: LocalVariable {
        variableDeclaration[var] Assignment variableInitializer[init] =>
            spanned!(LocalVariable_ { variable: var, initializer: init })
    }

    // Statements ($14.5)
    statement: Statement {
        block[b] => spanned!(Statement_::Block(b)),
        #[no_reduce(ELSE)]
        IF LParen expression[test] RParen statement[s1] =>
            spanned!(Statement_::If(test, box s1, None)),
        IF LParen expression[test] RParen statement[s1] ELSE statement[s2] =>
            spanned!(Statement_::If(test, box s1, Some(box s2))),
        WHILE LParen expression[test] RParen statement[body] =>
            spanned!(Statement_::While(test, box body)),
        FOR LParen maybeStatementExpression[f1] Semicolon maybeExpression[f2] Semicolon maybeStatementExpression[f3] RParen statement[body] =>
            spanned!(Statement_::For(f1, f2, f3, box body)),
        FOR LParen localVariableDeclaration[f1] Semicolon maybeExpression[f2] Semicolon maybeStatementExpression[f3] RParen statement[body] =>
            spanned!(Statement_::ForDecl(f1, f2, f3, box body)),
        Semicolon => spanned!(Statement_::Empty),
        statementExpression[expr] Semicolon => spanned!(Statement_::Expression(expr)),
        RETURN expression[expr] Semicolon => spanned!(Statement_::Return(expr)),
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
pub enum ExpressionOrType_ {
    Name(QualifiedIdentifier),
}
type ExpressionOrType = Spanned<ExpressionOrType_>;

pub trait IsExpressionOrType {
    fn convert(ExpressionOrType) -> Self;
}
impl IsExpressionOrType for Expression {
    fn convert(x: ExpressionOrType) -> Expression {
        match x.node {
            ExpressionOrType_::Name(n) => spanned(x.span, Expression_::Name(n)),
        }
    }
}
impl IsExpressionOrType for SimpleType {
    fn convert(x: ExpressionOrType) -> SimpleType {
        match x.node {
            ExpressionOrType_::Name(n) => spanned(x.span, SimpleType_::Other(n)),
        }
    }
}
impl IsExpressionOrType for Type {
    fn convert(x: ExpressionOrType) -> Type {
        spanned(x.span, Type_::SimpleType(x.into()))
    }
}
impl ExpressionOrType {
    pub fn into<T: IsExpressionOrType>(self) -> T { IsExpressionOrType::convert(self) }
}
