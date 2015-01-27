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

    // Types ($4.1) ($4.3)
    // Since type is a Rust keyword, use the term jType.
    jType: Type {
        simpleType[t] => Type::SimpleType(t),
        simpleType[t] LBracket RBracket => Type::ArrayType(t),
    }

    simpleType: SimpleType {
        BOOLEAN => SimpleType::Boolean,
        INT => SimpleType::Int,
        SHORT => SimpleType::Short,
        CHAR => SimpleType::Char,
        BYTE => SimpleType::Byte,
        qualifiedIdentifier[q] => SimpleType::Other(q),
    }

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
    // TODO: Will want test cases in regards to the last comma.
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

    expression: Expression {
        IntegerLiteral(i) => Expression::NothingYet,
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

