use ast;
use name::*;
use span::{Span, spanned, IntoSpan};

use middle::*;
use lang_items::*;
use collect_types::collect_types;
use collect_members::collect_members;
use tycheck::{populate_method, populate_constructor, populate_field};
use arena::Arena;

use rbtree::RbMap;

use std::fmt;
use std::borrow::ToOwned;
use std::collections::{HashMap, HashSet, RingBuf};

#[derive(Clone)]
pub enum Variable<'a, 'ast: 'a> {
    LocalVariable(VariableRef<'a, 'ast>),
    Field(FieldRef<'a, 'ast>),
}
impl<'a, 'ast> fmt::Show for Variable<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            Variable::LocalVariable(var) => {
                write!(f, "var {}: {}", var.fq_name, var.ty)
            }
            Variable::Field(field) => {
                write!(f, "field {}: {}", field.fq_name, field.ty)
            }
        }
    }
}

pub type TypesEnvironment<'a, 'ast> = RbMap<Symbol, TypeDefinitionRef<'a, 'ast>>;
pub type VariablesEnvironment<'a, 'ast> = RbMap<Symbol, Variable<'a, 'ast>>;

#[derive(Clone)]
pub struct Environment<'a, 'ast: 'a> {
    pub types: TypesEnvironment<'a, 'ast>,
    pub variables: VariablesEnvironment<'a, 'ast>,
    pub toplevel: PackageRef<'a, 'ast>,
    pub package: PackageRef<'a, 'ast>,
    pub enclosing_type: TypeDefinitionRef<'a, 'ast>,

    pub lang_items: LangItems<'a, 'ast>,

    // Search here for more types.
    pub on_demand_packages: Vec<PackageRef<'a, 'ast>>
}

impl<'a, 'ast> fmt::Show for Environment<'a, 'ast> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(writeln!(f, "Environment {{"));
        try!(writeln!(f, "\ttypes:"));
        for &(name, tyref) in self.types.iter() {
            try!(writeln!(f, "\t\t{} => {}", name, tyref.fq_name));
        }
        try!(writeln!(f, "\tvars:"));
        for &(name, ref var) in self.variables.iter() {
            try!(writeln!(f, "\t\t{} => {:?}", name, var));
        }
        try!(writeln!(f, "\tcurrent package: {}", self.package.fq_name));
        try!(writeln!(f, "\tenclosing type: {}", self.enclosing_type.fq_name));
        try!(writeln!(f, "}}"));
        Ok(())
    }
}

pub type TypeEnvironmentPair<'a, 'ast> = (TypeDefinitionRef<'a, 'ast>, Environment<'a, 'ast>);

enum AmbiguousResult<'a, 'ast: 'a> {
    Package(PackageRef<'a, 'ast>),
    Type(TypeDefinitionRef<'a, 'ast>),
    Expression(TypedExpression<'a, 'ast>),
    Unknown
}

impl<'a, 'ast> Environment<'a, 'ast> {
    /// Is `sub` a subtype of `sup`?
    /// (Every type is a subtype of `Object`.)
    pub fn is_subtype(&self, sub: TypeDefinitionRef<'a, 'ast>, sup: TypeDefinitionRef<'a, 'ast>) -> bool {
        if sup == self.lang_items.object {
            return true;
        }
        let mut q: RingBuf<TypeDefinitionRef<'a, 'ast>> = RingBuf::new();
        let mut visited: HashSet<TypeDefinitionRef<'a, 'ast>> = HashSet::new();
        q.push_back(sub);
        visited.insert(sub);
        while let Some(next) = q.pop_front() {
            if next == sup {
                return true;
            }
            for &parent in next.extends.borrow().iter()
                .chain(next.implements.borrow().iter()) {
                if visited.insert(parent) {
                    q.push_back(parent);
                }
            }
        }
        false
    }

    // Resolve extends and implements. This is done separately from walking
    // the AST because we need to process the compilations in topological
    // order (with respect to inheritance).
    fn resolve_inheritance(&self, tydef: TypeDefinitionRef<'a, 'ast>) {
        match tydef.ast.node {
            ast::TypeDeclaration_::Class(ref class) => {
                if let Some(ref extension) = class.node.extends {
                    // This class extends a parent class.
                    self.resolve_class_extension(tydef, &*extension.parts);
                }
                self.resolve_implements(tydef,
                                        class.node.implements.as_slice());
            }
            ast::TypeDeclaration_::Interface(ref interface) => {
                self.resolve_interface_extensions(tydef,
                                                  interface.node.extends.as_slice());
            },
        }
    }

    fn resolve_class_extension(&self,
                               typedef: TypeDefinitionRef<'a, 'ast>,
                               extension: &[Ident]) {
        match self.resolve_type_name(extension) {
            Some(extended_type) => {
                if extended_type.kind == TypeKind::Interface {
                    // ($8.1.3, dOvs simple constraint 1)
                    span_error!(typedef.ast.span,
                                "class cannot extend interface");
                }
                if extended_type.has_modifier(ast::Modifier_::Final) {
                    // ($8.1.1.2/$8.1.3 dOvs simple constraint 4)
                    span_error!(typedef.ast.span,
                                "cannot extend final class `{}`",
                                extended_type.fq_name);
                }
                typedef.extends.borrow_mut().push(extended_type);
            },
            None => {
                // an error was already printed
            },
        }
    }

    fn resolve_interface_extensions(&self,
                                    typedef: TypeDefinitionRef<'a, 'ast>,
                                    extensions: &[QualifiedIdentifier]) {
        let mut seen = HashSet::new();
        for extension in extensions.iter() {
            match self.resolve_type_name(&*extension.parts) {
                Some(extended_type) => {
                    if extended_type.kind == TypeKind::Class {
                        // ($9.1.2)
                        span_error!(extension,
                                    "interface cannot extend class");
                    }
                    // An interface must not be repeated in an implements clause,
                    // or in an extends clause of an interface.
                    // (JLS 8.1.4, dOvs simple constraint 3)
                    if !seen.insert(&extended_type.fq_name) {
                        span_error!(extension,
                                    "duplicate extended interface");
                    }
                    typedef.extends.borrow_mut().push(extended_type);
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    fn resolve_implements(&self,
                          typedef: TypeDefinitionRef<'a, 'ast>,
                          implements: &[QualifiedIdentifier]) {
        let mut seen = HashSet::new();
        for implement in implements.iter() {
            match self.resolve_type_name(&*implement.parts) {
                Some(implemented_type) => {
                    if implemented_type.kind == TypeKind::Class {
                        // ($8.1.4, dOvs simple constraint 2)
                        span_error!(implement,
                                    "class cannot implement class");
                    }
                    // An interface must not be repeated in an implements clause,
                    // or in an extends clause of an interface.
                    // (JLS 8.1.4, dOvs simple constraint 3)
                    if !seen.insert(&implemented_type.fq_name) {
                        span_error!(implement,
                                    "duplicate implemented interface");
                    }
                    typedef.implements.borrow_mut().push(implemented_type);
                },
                None => {
                    // an error was already printed
                },
            }
        }
    }

    pub fn resolve_type(&self, ty: &ast::Type) -> Type<'a, 'ast> {
        match ty.node {
            ast::Type_::SimpleType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type) {
                    Type::SimpleType(ty)
                } else {
                    Type::Unknown
                },
            ast::Type_::ArrayType(ref simple_type) =>
                if let Some(ty) = self.resolve_simple_type(simple_type) {
                    Type::ArrayType(ty)
                } else {
                    Type::Unknown
                },
            ast::Type_::Void => Type::Void,
        }
    }

    pub fn resolve_simple_type(&self,
                           ty: &ast::SimpleType)
            -> Option<SimpleType<'a, 'ast>> {
        match ty.node {
            ast::SimpleType_::Boolean => Some(SimpleType::Boolean),
            ast::SimpleType_::Int => Some(SimpleType::Int),
            ast::SimpleType_::Short => Some(SimpleType::Short),
            ast::SimpleType_::Char => Some(SimpleType::Char),
            ast::SimpleType_::Byte => Some(SimpleType::Byte),
            ast::SimpleType_::Other(ref qident) =>
                self.resolve_type_name(&*qident.parts)
                    .map(|ty| SimpleType::Other(ty)),
        }
    }

    // Look up a package or type name, but always fails when a type is found.
    // (This is because types in Joos never contain types.)
    // Emits an error on failure.
    fn resolve_package_or_type(&self, id: &[Ident]) -> Option<PackageRef<'a, 'ast>> {
        (match id {
            [] => panic!("bug: tried to resolve an empty package name"),
            [ref ident] => {
                // Because Java has nested types, we first have to check if
                // there is a type obscuring `ident`.
                match self.find_type(ident) {
                    Some(tydef) => {
                        span_error!(ident.span,
                                    "`{}` refers to type `{}`, not a package",
                                    ident, tydef.fq_name);
                        None
                    },
                    None => Some((self.toplevel, ident)),
                }
            }
            [init.., ref last] => self.resolve_package_or_type(init).map(|package| (package, last)),
        }).and_then(|(package, name)| match package.contents.borrow().get(&name.node) {
            // FIXME: duplicated code with `resolve_package`...
            Some(&PackageItem::Package(package)) => Some(package),
            Some(&PackageItem::TypeDefinition(..)) => {
                // There was a type instead!
                span_error!(Span::range(&id[0], id.last().unwrap()),
                            "no such package `{}`; found a type instead",
                            Qualified(id.iter()));
                None
            }
            None => {
                span_error!(Span::range(&id[0], id.last().unwrap()),
                            "no such package `{}`",
                            Qualified(id.iter()));
                None
            }
        })
    }

    // Look up a (user-defined) type by either a qualified or simple name.
    // Emits an error on failure.
    pub fn resolve_type_name(&self, id: &[Ident]) -> Option<TypeDefinitionRef<'a, 'ast>> {
        match id {
            [] => panic!("bug: tried to resolve an empty type name"),
            // simple name
            [ref ident] => match self.find_type(ident) {
                Some(tydef) => Some(tydef),
                None => {
                    span_error!(ident.span, "unresolved type name");
                    None
                }
            },
            // fully-qualified name
            // in Joos, `init` must refer to a package... but if it's a type, we need to error
            [init.., ref last] => self.resolve_package_or_type(init).and_then(|package| {
                match package.contents.borrow().get(&last.node) {
                    Some(&PackageItem::TypeDefinition(tydef)) => Some(tydef),
                    _ => {
                        span_error!(Span::range(&init[0], last),
                                    "no such type `{}` in package `{}`",
                                    last, Qualified(init.iter()));
                        None
                    }
                }
            })
        }
    }

    // Look up a type by simple name, using the current environment.
    // TODO: Clean up the error story here (don't want to emit multiple errors)
    fn find_type(&self, ty: &Ident) -> Option<TypeDefinitionRef<'a, 'ast>> {
        self.types.get(&ty.node)
        .cloned()
        .or_else(|| {
            // Check the current package. This has higher precedence than on-demand imports.
            match self.package.contents.borrow().get(&ty.node) {
                Some(&PackageItem::TypeDefinition(tydef)) => Some(tydef),
                _ => None,
            }
        })
        .or_else(|| {
            // If the type is not in the current environment, we can look in the
            // on-demand import packages.
            // It's necessary to look through every package to check for
            // ambiguities.
            let mut found_type: Option<TypeDefinitionRef<'a, 'ast>> = None;
            for package in self.on_demand_packages.iter() {
                match package.contents.borrow().get(&ty.node) {
                    Some(&PackageItem::TypeDefinition(typedef)) => {
                        // If we already have a type, then there's an ambiguity.
                        if let Some(existing) = found_type {
                            span_error!(ty.span,
                                        "ambiguous type name `{}`: could refer to `{}` or `{}`",
                                        ty,
                                        typedef.fq_name,
                                        existing.fq_name);
                            found_type = None;
                            break;
                        } else {
                            found_type = Some(typedef);
                        }
                    },
                    // Ignore subpackages.
                    Some(_) => {},
                    None => {},
                }
            }
            found_type
        })
    }

    pub fn resolve_field_access(&self, span: Span, texpr: TypedExpression<'a, 'ast>, name: &Ident)
            -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {
        // FIXME: bad clone
        match texpr.ty().clone() {
            Type::SimpleType(SimpleType::Other(tyref)) => {
                if let Some(&field) = tyref.fields.borrow().get(&name.node) {
                    self.check_field_access_allowed(span, field, tyref);

                    // Can't use static fields on expressions like a.MAX_VALUE
                    if field.is_static() {
                        span_error!(span, "using static field `{}` on instance", name);
                    }

                    Some((TypedExpression_::FieldAccess(box texpr, field),
                         field.ty.clone()))
                } else {
                    span_error!(span,
                                "reference type `{}` has no field `{}`",
                                tyref.fq_name, name);
                    None
                }
            }
            ref ty @ Type::SimpleType(_) => {
                span_error!(span,
                            "primitive type `{}` has no field `{}`",
                            ty, name);
                None
            }
            ref ty @ Type::ArrayType(_) => {
                // FIXME: Use intrinsics (?) or something
                if name.node == Symbol::from_str("length") {
                    Some((TypedExpression_::ArrayLength(box texpr),
                          Type::SimpleType(SimpleType::Int)))
                } else {
                    span_error!(span,
                                "array type `{}` has no field `{}`",
                                ty, name);
                    None
                }
            }
            Type::Null => {
                span_error!(span,
                            "`null` has no field `{}`",
                            name);
                None
            }
            Type::Void => {
                span_error!(span, "void type has no fields");
                None
            }
            Type::Unknown => None
        }
    }

    // Resolve a named method.
    pub fn resolve_named_method_access(&self, span: Span,
                                       require_static: bool,
                                       name: &QualifiedIdentifier,
                                       targ_exprs: Vec<TypedExpression<'a, 'ast>>)
            -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {

        // ($15.12.1) Rules for handling named methods.
        match name.parts.as_slice() {
            [ref ident] => {
                // "If it is a simple name, that is, just an Identifier,
                // then the name of the method is the Identifier."
                let resolved = self.resolve_typedef_method_access(span, self.enclosing_type,
                                                                  ident, targ_exprs);
                if let Some((TypedExpression_::MethodInvocation(_, method, _), _)) = resolved {
                    if !method.is_static && require_static {
                        span_error!(span, "calling non-static implicit this method on type");
                    }
                    if method.is_static {
                        span_error!(span, "calling static method without naming class");
                    }
                }
                resolved
            },
            [init.., ref last] => {
                // "If it is a qualified name of the form TypeName . Identifier"
                // "In all other cases, the qualified name has the
                //  form FieldName . Identifier" (note" spec has a mistake, it
                // can be more than just fields - e.g. local variables)
                match self.resolve_ambiguous_path(init) {
                    AmbiguousResult::Type(typedef) => {
                        let resolved = self.resolve_typedef_method_access(span, typedef,
                                                                          last, targ_exprs);
                        if let Some((TypedExpression_::MethodInvocation(_, method, _), _)) = resolved {
                            if !method.is_static {
                                span_error!(span, "calling non-static method on type");
                            }
                        }
                        resolved
                    },
                    AmbiguousResult::Expression(texpr) => {
                        let resolved = self.resolve_expr_method_access(span, texpr,
                                                                       last, targ_exprs);
                        if let Some((TypedExpression_::MethodInvocation(_, method, _), _)) = resolved {
                            if method.is_static {
                                span_error!(span, "calling static method on instance");
                            }
                        }
                        resolved
                    },
                    _ => {
                        span_error!(span, "no type for method invocation found");
                        None
                    },
                }
            },
            [] => {
                panic!("empty name?")
            },
        }
    }

    fn check_field_access_allowed(&self, span: Span, field: FieldRef<'a, 'ast>,
                                  tyref: TypeDefinitionRef<'a, 'ast>) {
        // ($6.6.2) Protected access rules.
        // Access to protected members allowed within the same package
        // or in subtypes.
        if field.is_protected() && self.package != tyref.package {
            if !self.is_subtype(self.enclosing_type, field.origin) ||
               (!field.is_static() && !self.is_subtype(tyref, self.enclosing_type)) {
                span_error!(span,
                            "cannot access protected field `{}` of `{}`",
                            field.fq_name, tyref.fq_name);
            }
        }
    }

    // ($6.2.2) Checks that we can access a given method.
    fn check_method_access_allowed(&self, span: Span,
                                   method: MethodRef<'a, 'ast>,
                                   tyref: TypeDefinitionRef<'a, 'ast>) {
        if let Protected(defining_type) = method.accessibility {
            if self.package != tyref.package
                && (!self.is_subtype(self.enclosing_type, defining_type) ||
                    (!method.is_static && !self.is_subtype(tyref, self.enclosing_type))) {
                span_error!(span,
                            "cannot access protected method `{}` of `{}` defined in `{}`",
                            method.fq_name, tyref.fq_name, defining_type.fq_name);
            }
        }
    }

    pub fn check_constructor_access_allowed(&self, span: Span, constructor: ConstructorRef<'a, 'ast>,
                                            tyref: TypeDefinitionRef<'a, 'ast>) -> bool {
        if constructor.is_protected() && self.package != tyref.package {
            span_error!(span, "cannot access protected constructor of `{}`",
                        tyref.fq_name);
            false
        } else {
            true
        }
    }

    // Resolve a method that is called on an expression.
    pub fn resolve_expr_method_access(&self, span: Span, texpr: TypedExpression<'a, 'ast>,
                                      name: &Ident, targ_exprs: Vec<TypedExpression<'a, 'ast>>)
            -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {

        let arg_types: Vec<_> = targ_exprs.iter()
            .map(|expr| expr.node.1.clone())
            .collect();
        let signature = MethodSignature { name: name.node, args: arg_types };

        // FIXME: bad clone
        match texpr.ty().clone() {
            Type::SimpleType(SimpleType::Other(tyref)) => {
                if let Some(&method) = tyref.methods.borrow().get(&signature) {
                    self.check_method_access_allowed(span, method, tyref);
                    Some((TypedExpression_::MethodInvocation(Some(box texpr),
                                                             method,
                                                             targ_exprs),
                          method.ret_ty.clone()))
                } else {
                    span_error!(span,
                                "reference type `{}` has no method `{}`",
                                tyref.fq_name, name);
                    None
                }
            }
            ref ty @ Type::SimpleType(_) => {
                span_error!(span,
                            "primitive type `{}` has no method `{}`",
                            ty, name);
                None
            }
            Type::ArrayType(_) => {
                // Array types have the same methods as Object does.
                self.resolve_typedef_method_access(span, self.lang_items.object,
                                                   name, targ_exprs)
            }
            Type::Null => {
                span_error!(span,
                            "`null` has no method `{}`",
                            name);
                None
            }
            Type::Void => {
                span_error!(span, "void type has no fields");
                None
            }
            Type::Unknown => None
        }
    }

    // Resolve a method that is called directly.
    fn resolve_typedef_method_access(&self, span: Span, tyref: TypeDefinitionRef<'a, 'ast>,
                                     name: &Ident, targ_exprs: Vec<TypedExpression<'a, 'ast>>)
            -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {

        let arg_types: Vec<_> = targ_exprs.iter()
            .map(|expr| expr.node.1.clone())
            .collect();
        let signature = MethodSignature { name: name.node, args: arg_types };

        if let Some(&method) = tyref.methods.borrow().get(&signature) {
            self.check_method_access_allowed(span, method, tyref);
            Some((TypedExpression_::MethodInvocation(None, method, targ_exprs),
                  method.ret_ty.clone()))
        } else {
            span_error!(span,
                        "reference type `{}` has no method `{}`",
                        tyref.fq_name, name);
            None
        }
    }

    // Resolve a simple name as a variable.
    // DOES NOT emit any error.
    fn resolve_variable(&self, ident: &Ident)
        -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {
        match self.variables.get(&ident.node) {
            Some(&Variable::LocalVariable(var)) => {
                // TODO: read the spec
                Some((TypedExpression_::Variable(var), var.ty.clone()))
            }
            Some(&Variable::Field(field)) => {
                if field.is_static() {
                    Some((TypedExpression_::StaticFieldAccess(field), field.ty.clone()))
                } else {
                    // FIXME: `this` existence is only checked later
                    Some((TypedExpression_::ThisFieldAccess(field), field.ty.clone()))
                }
            }
            None => {
                None
            }
        }
    }

    // Convert a qualified identifier to an expression.
    // Returns `None` to indicate that no viable interpretation was found.
    // In this case, an error was already emitted.
    pub fn resolve_expression(&self, path: &QualifiedIdentifier)
        -> Option<(TypedExpression_<'a, 'ast>, Type<'a, 'ast>)> {
        let span = path.into_span();
        match &*path.parts {
            [] => unreachable!(),
            [ref ident] => {
                let ret = self.resolve_variable(ident);
                if let None = ret {
                    span_error!(span, "no such variable `{}`", ident);
                }
                ret
            }
            [init.., ref last] => {
                match self.resolve_ambiguous_path(init) {
                    AmbiguousResult::Package(package) => {
                        span_error!(span,
                                    "cannot take member `{}` of package `{}`",
                                    last, package.fq_name);
                        None
                    }
                    AmbiguousResult::Type(tyref) => {
                        if let Some(&field) = tyref.fields.borrow().get(&last.node) {
                            self.check_field_access_allowed(span, field, tyref);

                            if !field.is_static() {
                                span_error!(span, "using non-static field `{}` on type", last);
                            }

                            Some((TypedExpression_::StaticFieldAccess(field),
                                  field.ty.clone()))
                        } else {
                            span_error!(span,
                                        "no such field `{}` in type `{}`",
                                        last, tyref.fq_name);
                            None
                        }
                    }
                    AmbiguousResult::Expression(expr) => {
                        self.resolve_field_access(path.into_span(), expr, last)
                    }
                    AmbiguousResult::Unknown => None
                }
            }
        }
    }

    // Returns `Unknown` iff an error is emitted.
    fn resolve_ambiguous_path(&self, path: &[Ident]) -> AmbiguousResult<'a, 'ast> {
        let span = Span::range(path.first().unwrap(), path.last().unwrap());
        match path {
            [] => unreachable!(),
            [ref ident] => {
                if let Some(expr) = self.resolve_variable(ident) {
                    let expr = spanned(Span::range(path.first().unwrap().span,
                                                   path.last().unwrap().span),
                                       expr);
                    AmbiguousResult::Expression(expr)
                } else if let Some(tydef) = self.find_type(ident) {
                    AmbiguousResult::Type(tydef)
                } else if let Some(&PackageItem::Package(package))
                    = self.toplevel.contents.borrow().get(&ident.node) {
                    // note: `toplevel` can only contain other packages, no types
                    AmbiguousResult::Package(package)
                } else {
                    span_error!(ident.span,
                                "no variable, type, or package named `{}`",
                                ident);
                    AmbiguousResult::Unknown
                }
            }
            [init.., ref last] => {
                match self.resolve_ambiguous_path(init) {
                    AmbiguousResult::Expression(expr) => {
                        if let Some(expr) = self.resolve_field_access(span, expr, last) {
                            AmbiguousResult::Expression(spanned(span, expr))
                        } else {
                            AmbiguousResult::Unknown
                        }
                    }
                    AmbiguousResult::Type(tydef) => {
                        match tydef.fields.borrow().get(&last.node) {
                            Some(&field) => {
                                let expr_ = (TypedExpression_::StaticFieldAccess(field),
                                             field.ty.clone());
                                AmbiguousResult::Expression(spanned(span, expr_))
                            }
                            None => {
                                span_error!(span,
                                            "no field `{}` found in type `{}`",
                                            last, tydef.fq_name);
                                AmbiguousResult::Unknown
                            }
                        }
                    }
                    AmbiguousResult::Package(package) => {
                        match package.contents.borrow().get(&last.node) {
                            Some(&PackageItem::Package(subpackage)) =>
                                AmbiguousResult::Package(subpackage),
                            Some(&PackageItem::TypeDefinition(tydef)) =>
                                AmbiguousResult::Type(tydef),
                            None => {
                                span_error!(span,
                                            "no `{}` found in package `{}`",
                                            last, package.fq_name);
                                AmbiguousResult::Unknown
                            }
                        }
                    }
                    AmbiguousResult::Unknown => AmbiguousResult::Unknown,
                }
            }
        }
    }

    fn add_fields(&mut self, tydef: TypeDefinitionRef<'a, 'ast>) {
        for (&name, &field) in tydef.fields.borrow().iter() {
            self.add_field(name, field);
        }
    }

    fn add_field(&mut self, name: Symbol, field: FieldRef<'a, 'ast>) {
        if let Some(_) = self.variables.insert_in_place(name, Variable::Field(field)) {
            panic!("bug: multiple fields with the same name in scope, somehow");
        }
    }

    pub fn add_var(&mut self, name: Symbol, var: VariableRef<'a, 'ast>) {
        if let Some(old) = self.variables.insert_in_place(name, Variable::LocalVariable(var)) {
            if let (_, Variable::LocalVariable(v)) = *old {
                span_error!(var.ast.span,
                            "variable `{}` already defined",
                            name);
                span_note!(v.ast.span, "the old definition is here");
            }
        }
    }
}

// Look up a package by its fully-qualified name.
fn resolve_package<'a, 'ast>(toplevel: PackageRef<'a, 'ast>, id: &[Ident]) -> Option<PackageRef<'a, 'ast>> {
    let mut package = toplevel;
    for (ix, ident) in id.iter().enumerate() {
        package = match package.contents.borrow().get(&ident.node) {
            Some(&PackageItem::Package(it)) => {
                it // Found it
            }
            Some(&PackageItem::TypeDefinition(..)) => {
                // There was a type instead!
                span_error!(Span::range(&id[0], &id[ix]),
                            "no such package `{}`; found a type instead",
                            Qualified(id[0..ix+1].iter()));
                return None
            }
            None => {
                span_error!(Span::range(&id[0], &id[ix]),
                            "no such package `{}`",
                            Qualified(id[0..ix+1].iter()));
                return None
            }
        };
    }
    Some(package)
}

fn insert_declared_type<'a, 'ast>(env: &TypesEnvironment<'a, 'ast>,
                                  ident: &Ident,
                                  typedef: TypeDefinitionRef<'a, 'ast>) -> TypesEnvironment<'a, 'ast> {

    let (new_env, previous) = env.insert(ident.node, typedef);
    if let Some(&(_, ref previous_item)) = previous {
        if previous_item.fq_name != typedef.fq_name {
            // TODO: Shouldn't continue after this error - how to do that?
            span_error!(ident.span,
                        "type `{}` declared in this file conflicts with import `{}`",
                        ident,
                        previous_item.fq_name);
        }
    }
    new_env
}

fn insert_type_import<'a, 'ast>(symbol: Symbol,
                                typedef: TypeDefinitionRef<'a, 'ast>,
                                imported: &QualifiedIdentifier,
                                current_env: TypesEnvironment<'a, 'ast>)
        -> TypesEnvironment<'a, 'ast> {
    let (new_env, previous_opt) = current_env.insert(symbol, typedef);
    if let Some(previous) = previous_opt {
        if previous.1.fq_name != typedef.fq_name {
            span_error!(imported,
                        "importing `{}` from `{}` conflicts with previous import",
                        symbol,
                        imported);
        }
    }
    new_env
}

fn import_single_type<'a, 'ast>(imported: &QualifiedIdentifier,
                                toplevel: PackageRef<'a, 'ast>,
                                current_env: TypesEnvironment<'a, 'ast>)
        -> TypesEnvironment<'a, 'ast> {
    match &*imported.parts {
        [] => panic!("impossible: imported empty type"),
        [ref id] => {
            span_error!(id.span,
                        "imported type name must fully qualified");
            current_env
        },
        // FIXME: Deduplicate this code with `resolve_type_name`
        // (factor into `resole_fq_type_name` or something)
        [init.., ref last] => match resolve_package(toplevel, init) {
            None => current_env,
            Some(package) => match package.contents.borrow().get(&last.node) {
                Some(&PackageItem::TypeDefinition(tydef)) => {
                    insert_type_import(last.node, tydef, imported, current_env)
                }
                _ => {
                    span_error!(Span::range(&init[0], last),
                                "no such type `{}` in package `{}`",
                                last, Qualified(init.iter()));
                    current_env
                }
            }
        },
    }
}

fn import_on_demand<'a, 'ast>(imported: &QualifiedIdentifier,
                              toplevel: PackageRef<'a, 'ast>,
                              on_demand_packages: &mut Vec<PackageRef<'a, 'ast>>) {
    if let Some(package) = resolve_package(toplevel, &*imported.parts) {
        on_demand_packages.push(package);
    }
}

fn inheritance_topological_sort_search<'a, 'ast>(typedef: TypeDefinitionRef<'a, 'ast>,
                                                 seen: &mut HashSet<Name>,
                                                 visited: &mut HashSet<Name>,
                                                 stack: &mut Vec<Name>,
                                                 sorted: &mut Vec<Name>)
        -> Result<(), ()> {
    let extends_borrow = typedef.extends.borrow();
    let implements_borrow = typedef.implements.borrow();
    let mut parents = extends_borrow.iter().chain(implements_borrow.iter());

    stack.push(typedef.fq_name);

    if !seen.insert(typedef.fq_name) {
        span_error!(typedef.ast.span,
                    "found an inheritance cycle: {:?}",
                    stack);
        return Err(());
    }

    for parent in parents {
        if !visited.contains(&parent.fq_name) {
            try!(inheritance_topological_sort_search(*parent, seen,
                                                     visited, stack, sorted));
        }
    }

    sorted.push(typedef.fq_name);
    visited.insert(typedef.fq_name);
    stack.pop();
    Ok(())
}

fn inheritance_topological_sort<'a, 'ast>(preprocessed_types: &[TypeEnvironmentPair<'a, 'ast>])
        -> Option<Vec<TypeEnvironmentPair<'a, 'ast>>> {

    // To find items in processed_types by fully-qualified names.
    let mut lookup = HashMap::new();
    for &(typedef, ref env) in preprocessed_types.iter() {
        lookup.insert(typedef.fq_name, (typedef, env.clone()));
    }

    let mut sorted: Vec<Name> = vec![];

    let mut seen: HashSet<Name> = HashSet::new();
    let mut visited: HashSet<Name> = HashSet::new();

    // Keep track of the depth-first search stack for error message
    // purposes (it shows the user where the cycle is).
    let mut stack: Vec<Name> = vec![];

    for &(typedef, _) in preprocessed_types.iter() {
        if !visited.contains(&typedef.fq_name) {
            let result = inheritance_topological_sort_search(
                typedef, &mut seen, &mut visited,
                &mut stack, &mut sorted);
            if let Err(_) = result {
                return None;
            }
        }
    }

    Some(sorted.iter().map(|name| lookup.get(name).unwrap().clone()).collect())
}

fn build_environments<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                toplevel: PackageRef<'a, 'ast>,
                                lang_items: &LangItems<'a, 'ast>,
                                units: &[(PackageRef<'a, 'ast>, &'ast ast::CompilationUnit, Vec<TypeDefinitionRef<'a, 'ast>>)])
-> Vec<(Environment<'a, 'ast>, ToPopulate<'a, 'ast>)> {
    let mut preprocessed_types = vec![];

    for &(package, ast, ref tydefs) in units.iter() {
        let mut types_env: TypesEnvironment<'a, 'ast> = RbMap::new();

        let mut on_demand_packages = vec![lang_items.lang];

        // Add all imports to initial environment for this compilation unit.
        for import in ast.imports.iter() {
            match import.node {
                ast::ImportDeclaration_::SingleType(ref qident) => {
                    types_env = import_single_type(qident,
                                                   toplevel,
                                                   types_env);
                },
                ast::ImportDeclaration_::OnDemand(ref qident) => {
                    import_on_demand(qident,
                                     toplevel,
                                     &mut on_demand_packages);
                },
            }
        }

        // Uniquify `on_demand_packages`.
        let on_demand_packages = on_demand_packages.into_iter()
            .map(|package| (package.fq_name, package))
            .collect::<HashMap<_, _> >()
            .into_iter()
            .map(|(_, package)| package)
            .collect();

        match &**tydefs {
            [tydef] => {
                let env = Environment {
                    types: types_env,
                    variables: RbMap::new(),
                    toplevel: toplevel,
                    package: package,
                    enclosing_type: tydef,
                    lang_items: lang_items.clone(),
                    on_demand_packages: on_demand_packages,
                };

                env.resolve_inheritance(tydef);
                preprocessed_types.push((tydef, env));
                if tydef == lang_items.object {
                    // Make sure `java.lang.Object` is processed first...
                    // XXX: This is such a hack!
                    let ix = preprocessed_types.len()-1;
                    preprocessed_types.swap(0, ix);
                }
            }
            [] => {}
            _ => panic!("wrong number of types: {}", tydefs.len())
        }
    }

    if let Some(sorted) = inheritance_topological_sort(preprocessed_types.as_slice()) {
        preprocessed_types = sorted;
    } else {
        return vec![];
    }

    let mut r = vec![];

    for (tydef, mut env) in preprocessed_types.into_iter() {
        let name = tydef.ast.name();

        // Add the type itself to the environment.
        env.types = insert_declared_type(&env.types, name, tydef);

        let to_populate = collect_members(arena, &env, tydef, lang_items);

        env.add_fields(tydef);

        if tydef.kind == TypeKind::Class {
            // ($8.1.1.1) well-formedness contraint 4 - abstract methods => abstract class
            let should_be_abstract =
                tydef.methods.borrow().iter()
                    .any(|(_, &method)| matches!(Abstract, method.impled));
            if should_be_abstract && !tydef.has_modifier(ast::Modifier_::Abstract) {
                span_error!(tydef.ast.span,
                            "class with abstract methods must be abstract");
            }
        }

        for v in to_populate.into_iter() {
            r.push((env.clone(), v));
        }
    }
    r
}

pub enum ToPopulate<'a, 'ast: 'a> {
    Method(MethodImplRef<'a, 'ast>),
    Constructor(ConstructorRef<'a, 'ast>),
    Field(FieldRef<'a, 'ast>),
}

fn populate<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                      methods: Vec<(Environment<'a, 'ast>, ToPopulate<'a, 'ast>)>,
                      lang_items: &LangItems<'a, 'ast>) {
    for (env, thing) in methods.into_iter() {
        match thing {
            ToPopulate::Method(method) => populate_method(arena, env, method, lang_items),
            ToPopulate::Constructor(constructor) => populate_constructor(arena, env, constructor, lang_items),
            ToPopulate::Field(field) => populate_field(arena, env, field, lang_items),
        }
    }
}

pub fn name_resolve<'a, 'ast>(arena: &'a Arena<'a, 'ast>, asts: &'ast [ast::CompilationUnit])
    -> (PackageRef<'a, 'ast>, PackageRef<'a, 'ast>) {

    let toplevel = arena.alloc(Package::new("top level".to_owned()));
    let default_package = arena.alloc(Package::new("default package".to_owned()));
    let types = collect_types(arena, toplevel, default_package, asts);
    let lang_items = find_lang_items(toplevel);
    let methods = build_environments(arena, toplevel, &lang_items, &*types);
    populate(arena, methods, &lang_items);

    (default_package, toplevel)
}

