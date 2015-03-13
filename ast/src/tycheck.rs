use ast;
use span::*;
use middle::*;
use arena::*;

use name_resolve::Environment;
use lang_items::LangItems;

use std::collections::{RingBuf, HashSet};

struct Typer<'l, 'a: 'l, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    env: Environment<'a, 'ast>,
    lang_items: &'l LangItems<'a, 'ast>,
}

fn expect<'a, 'ast, S: IntoSpan>(expected: &Type<'a, 'ast>, actual: &Type<'a, 'ast>, sp: S) {
    // If a type is `Unknown` then an error was already emitted -
    // don't make it worse
    if let Type::Unknown = *expected {
    } else if let Type::Unknown = *actual {
    } else if *expected != *actual {
        span_error!(sp.into_span(),
                    "type mismatch: expected `{}`, found `{}`",
                    expected, actual);
    }
}

fn expect_expr<'a, 'ast>(expected: &Type<'a, 'ast>, expr: &TypedExpression<'a, 'ast>) {
    expect(expected, expr.ty(), expr.span)
}

static NULL: ast::Literal = ast::Literal::Null;
fn dummy_expr_<'a, 'ast>() -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {
    (TypedExpression_::Literal(&NULL), Type::Unknown)
}

impl<'l, 'a, 'ast> Typer<'l, 'a, 'ast> {
    /// Is `sub` a subtype of `sup`?
    /// (Every type is a subtype of `Object`.)
    fn is_subtype(&self, sub: TypeDefinitionRef<'a, 'ast>, sup: TypeDefinitionRef<'a, 'ast>) -> bool {
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

    // Check that there is a legal widening conversion from `source` to `target`.
    // Emits an error if there is not.
    fn check_simple_widening(&self, span: Span,
                             source: &SimpleType<'a, 'ast>,
                             target: &SimpleType<'a, 'ast>) {
        use middle::SimpleType::*;
        if source == target {
            return;
        }
        match *source {
            Char | Boolean => {
                span_error!(span, "no implicit conversion to `{}`", source);
            }
            Byte | Short | Int => match *target {
                Byte | Short | Char | Int => {
                    fn numeric_width<'a, 'ast>(ty: &SimpleType<'a, 'ast>) -> i32 {
                        match *ty {
                            Byte => 1,
                            Short | Char => 2,
                            Int => 4,
                            _ => panic!("not numeric")
                        }
                    }
                    if numeric_width(source) < numeric_width(target) {
                        span_error!(span,
                                    "cast required for narrowing conversion from `{}` to `{}`",
                                    target, source);
                    }
                }
                Boolean => {
                    span_error!(span,
                                "cannot convert from `{}` to `{}`",
                                target, source);
                }
                _ => {
                    span_error!(span,
                                "cannot convert from non-primitive type `{}` to `{}`",
                                target, source);
                }
            },
            Other(expect_tydef) => match *target {
                Other(expr_tydef) => {
                    if !self.is_subtype(expr_tydef, expect_tydef) {
                        if self.is_subtype(expect_tydef, expr_tydef) {
                            span_error!(span,
                                        "cast required for narrowing conversion from `{}` to `{}`",
                                        target, source);
                        } else {
                            span_error!(span,
                                        "no conversion from `{}` to `{}`",
                                        target, source);
                        }
                    }
                }
                _ => {
                    span_error!(span,
                                "cannot convert from primitive type `{}` to `{}`",
                                target, source);
                }
            },
        }
    }

    fn check_widening(&self, span: Span, source: &Type<'a, 'ast>, target: &Type<'a, 'ast>) {
        if source == target {
            return;
        }
        match (source, target) {
            (_, &Type::Unknown) | (&Type::Unknown, _) => (),

            (&Type::Null, _) => panic!("coerce to null type?"),

            (&Type::Void, _)
            | (_, &Type::Void) => {
                span_error!(span, "cannot convert void types");
            }

            // simple types might be convertible to other simple types
            (&Type::SimpleType(ref a), &Type::SimpleType(ref b)) => {
                self.check_simple_widening(span, a, b);
            }
            // ... but not to arrays
            (&Type::ArrayType(_), &Type::SimpleType(_)) => {
                span_error!(span,
                            "cannot convert from `{}` to array type `{}`",
                            target, source);
            }

            // arrays can be converted to some reference types
            (&Type::SimpleType(SimpleType::Other(expect_tydef)), &Type::ArrayType(_)) => {
                if expect_tydef != self.lang_items.object
                    && expect_tydef != self.lang_items.cloneable
                    && expect_tydef != self.lang_items.serializable {
                    span_error!(span,
                                "cannot convert from array type `{}` to `{}`",
                                target, source);
                }
            }
            (&Type::ArrayType(_), &Type::ArrayType(_)) => {
                span_error!(span,
                            "cannot convert from array type `{}` to array type `{}`",
                            target, source);
            }
            // ... but not primitive types
            (&Type::SimpleType(_), &Type::ArrayType(_)) => {
                span_error!(span,
                            "cannot convert from array type `{}` to primitive type `{}`",
                            target, source);
            }

            // null can be converted to any reference type
            (&Type::SimpleType(SimpleType::Other(_)), &Type::Null) |
            (&Type::ArrayType(_), &Type::Null) => (),
            // ... but not anything else
            (&Type::SimpleType(_), &Type::Null) => {
                span_error!(span,
                            "cannot convert null to primitive type `{}`",
                            source);
            }
        }
    }

    // Returns whether there is a legal Casting Conversion ($5.5)
    // from from `source` to `target`
    // TODO: Make sure the marmoset test cases cover every single one of these rules.
    fn check_casting_conversion(&self,
                                source: &Type<'a, 'ast>,
                                target: &Type<'a, 'ast>) -> bool {
        if source == target {
            return true;
        }
        match (source, target) {
            (&Type::SimpleType(SimpleType::Other(source_tydef)),
             &Type::SimpleType(SimpleType::Other(target_tydef))) => {
                match (source_tydef.kind, target_tydef.kind) {
                    (TypeKind::Class, TypeKind::Class) => {
                        source_tydef.dominates(target_tydef) ||
                        target_tydef.dominates(source_tydef)
                    }
                    (TypeKind::Class, TypeKind::Interface) => {
                        if source_tydef.has_modifier(ast::Modifier_::Final) {
                            target_tydef.dominates(source_tydef)
                        } else {
                            true
                        }
                    }
                    (TypeKind::Interface, TypeKind::Class) => {
                        if target_tydef.has_modifier(ast::Modifier_::Final) {
                            // TODO: Check this (not explicitely written in the spec)
                            source_tydef.dominates(target_tydef)
                        } else {
                            true
                        }
                    }
                    (TypeKind::Interface, TypeKind::Interface) => {
                        // TODO
                        // Need to check that the interfaces don't have confliciting
                        // methods (different return types)
                        true
                    }
                }
            }
            (&Type::SimpleType(SimpleType::Other(typedef)), &Type::ArrayType(_))
            | (&Type::ArrayType(_), &Type::SimpleType(SimpleType::Other(typedef))) => {
                match typedef.kind {
                    TypeKind::Class => {
                        typedef == self.lang_items.object
                    }
                    TypeKind::Interface => {
                        // Arrays implement Serializable and Cloneable only
                        typedef == self.lang_items.serializable ||
                        typedef == self.lang_items.cloneable
                    }
                }
            }

            (&Type::ArrayType(ref source_array_ty),
             &Type::ArrayType(ref target_array_ty)) => {
                match (source_array_ty, target_array_ty) {
                    (&SimpleType::Other(_),
                     &SimpleType::Other(_)) => {
                        self.check_casting_conversion(&Type::SimpleType(source_array_ty.clone()),
                                                      &Type::SimpleType(target_array_ty.clone()))
                    }
                    _ => {
                        source_array_ty == target_array_ty
                    }
                }
            }
            _ => panic!("expected only reference types")
        }
    }

    // Performs an "assignment conversion" ($5.2) on the given expression,
    // which is only legal for widening conversions.
    // Returns an expression whose type is always `expect`.
    fn coerce_expr(&self, expect: &Type<'a, 'ast>, expr: TypedExpression<'a, 'ast>)
        -> TypedExpression<'a, 'ast> {
        if expect == expr.ty() {
            return expr;
        }
        self.check_widening(expr.span, expect, expr.ty());
        spanned(expr.span, (TypedExpression_::Widen(box expr), expect.clone()))
    }

    fn new_var(&mut self, var: &'ast ast::VariableDeclaration) -> VariableRef<'a, 'ast> {
        let def = self.arena.alloc(VariableDef::new(
                format!("{}.{}", self.env.enclosing_type.fq_name, var.name),
                self.env.resolve_type(&var.ty),
                var));
        self.env.add_var(var.name.node, def);
        def
    }

    fn local_variable(&mut self, var: &'ast ast::LocalVariable) -> TypedLocalVariable<'a, 'ast> {
        let v = self.new_var(&var.variable);
        let init = self.expr(&var.initializer);
        let init = self.coerce_expr(&v.ty, init);
        spanned(var.span, TypedLocalVariable_ {
            variable: v,
            initializer: init,
        })
    }

    fn block(&mut self, block: &'ast ast::Block) -> TypedBlock<'a, 'ast> {
        // FIXME: This is a little more expensive than necessary
        let save = self.env.clone();
        let block = spanned(block.span, TypedBlock_ {
            stmts: block.stmts.iter().map(|stmt| self.block_stmt(stmt)).collect()
        });
        self.env = save;
        block
    }

    fn block_stmt(&mut self, stmt: &'ast ast::BlockStatement) -> TypedBlockStatement<'a, 'ast> {
        use ast::BlockStatement_::*;
        spanned(stmt.span, match stmt.node {
            LocalVariable(ref local_var) => TypedBlockStatement_::LocalVariable(self.local_variable(local_var)),
            LocalClass(ref _class) => panic!("local class"),
            Statement(ref stmt) => TypedBlockStatement_::Statement(self.stmt(stmt)),
        })
    }

    fn stmt(&mut self, stmt: &'ast ast::Statement) -> TypedStatement<'a, 'ast> {
        use ast::Statement_::*;
        fn expect_bool<'a, 'ast>(span: Span, ty: &Type<'a, 'ast>) {
            if ty != &Type::SimpleType(SimpleType::Boolean) {
                span_error!(span, "type mismatch - expected boolean, found `{}`", ty);
            }
        }
        spanned(stmt.span, match stmt.node {
            Expression(ref expr) => TypedStatement_::Expression(self.expr(expr)),
            If(ref test, box ref then, ref els) => {
               let test_texpr = self.expr(test);
               expect_bool(test.span, test_texpr.ty());
               TypedStatement_::If(test_texpr,
                                   box self.stmt(then),
                                   els.as_ref().map(|&box ref s| box self.stmt(s)))
            }
            While(ref test, box ref inner) => {
                let test_texpr = self.expr(test);
                expect_bool(test.span, test_texpr.ty());
                TypedStatement_::While(test_texpr, box self.stmt(inner))
            }
            For(ref init, ref test, ref update, box ref inner) => {
                let init_texpr = init.as_ref().map(|i| self.expr(i));
                let test_texpr_opt = test.as_ref().map(|t| self.expr(t));
                if let Some(ref test_texpr) = test_texpr_opt {
                    expect_bool(test.as_ref().unwrap().span, test_texpr.ty());
                }
                TypedStatement_::For(
                    init_texpr,
                    test_texpr_opt,
                    update.as_ref().map(|u| self.expr(u)),
                    box self.stmt(inner))
            }
            ForDecl(ref init, ref test, ref update, box ref inner) => {
                let init_texpr = self.local_variable(init);
                let test_texpr_opt = test.as_ref().map(|t| self.expr(t));
                if let Some(ref test_texpr) = test_texpr_opt {
                    expect_bool(test.as_ref().unwrap().span, test_texpr.ty());
                }
                TypedStatement_::ForDecl(
                    init_texpr,
                    test_texpr_opt,
                    update.as_ref().map(|u| self.expr(u)),
                    box self.stmt(inner))
            }
            Empty => TypedStatement_::Empty,
            Return(ref expr) => {
                // FIXME: check return types
                let texpr = if let Some(ref expr) = *expr {
                    Some(self.expr(expr))
                } else {
                    None
                };
                TypedStatement_::Return(texpr)
            }
            Block(ref block) => TypedStatement_::Block(self.block(block)),
        })
    }

    fn expr(&mut self, expr: &'ast ast::Expression) -> TypedExpression<'a, 'ast> {
        spanned(expr.span, self.expr_(expr))
    }

    fn new_expr_(&mut self, tydef: TypeDefinitionRef<'a, 'ast>, args: Vec<TypedExpression<'a, 'ast>>)
        -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {
        // FIXME: Check constructor call
        (TypedExpression_::NewStaticClass(tydef, args), Type::object(tydef))
    }

    fn expr_(&mut self, expr: &'ast ast::Expression) -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {

        use ast::Expression_::*;
        match expr.node {
            Literal(ref lit) => self.lit(lit),
            This => {
                // TODO: Check that `this` is legal here (i.e. that we're in an instance method)
                (TypedExpression_::This, Type::object(self.env.enclosing_type))
            }
            NewStaticClass(ref id, ref args) => {
                let tydef = self.env.resolve_type_name(&*id.parts);
                let targs = args.iter().map(|arg| self.expr(arg)).collect();

                if let Some(tydef) = tydef {
                    // Some types cannot be instantiated.
                    if tydef.kind == TypeKind::Interface {
                        span_error!(expr.span, "cannot instantiate Interface type");
                        dummy_expr_()
                    } else if tydef.has_modifier(ast::Modifier_::Abstract) {
                        span_error!(expr.span, "cannot instantiate Abstract class type");
                        dummy_expr_()
                    } else {
                        self.new_expr_(tydef, targs)
                    }
                } else {
                    panic!("unable to find static class that should have been resolved");
                }
            }
            NewDynamicClass(box _, _, _) => panic!("dynamic class"),
            NewArray(ref tyname, box ref size) => {
                let sty = self.env.resolve_simple_type(tyname);
                let tsize = self.expr(size);
                let tsize = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tsize);
                match sty {
                    Some(sty) => {
                        let ty = Type::ArrayType(sty.clone());
                        (TypedExpression_::NewArray(sty, box tsize), ty)
                    }
                    None => dummy_expr_()
                }
            }
            FieldAccess(box ref target, ref name) => {
                let texpr = self.expr(target);
                Environment::resolve_field_access(expr.span, texpr, name)
                    .unwrap_or_else(dummy_expr_)
            }
            NamedMethodInvocation(ref _name, ref args) => {
                let _targs: Vec<_> = args.iter()
                    .map(|arg| self.expr(arg))
                    .collect();
                self.env.resolve_named_method_access(expr.span, _name, _targs)
                    .unwrap_or_else(dummy_expr_)
            }
            MethodInvocation(box ref callee, ref _name, ref args) => {
                let _tcallee = self.expr(callee);
                let _targs: Vec<_> = args.iter()
                    .map(|arg| self.expr(arg))
                    .collect();
                Environment::resolve_expr_method_access(expr.span, _tcallee,
                                                        _name, _targs)
                    .unwrap_or_else(dummy_expr_)
            }
            ArrayAccess(box ref array, box ref ix) => {
                let tarray = self.expr(array);
                let tix = self.expr(ix);
                let tix = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tix);
                match tarray.ty().clone() {
                    Type::ArrayType(sty) =>
                        (TypedExpression_::ArrayAccess(box tarray, box tix),
                        Type::SimpleType(sty)),
                    Type::Unknown => dummy_expr_(),
                    // TODO: how to handle null
                    ty => {
                        span_error!(array.span,
                                    "type mismatch: expected array, found `{}`",
                                    ty);
                        dummy_expr_()
                    }
                }
            }
            Name(ref ident) => {
                self.env.resolve_expression(ident)
                    .unwrap_or_else(dummy_expr_)
            }
            Assignment(box ref lhs, box ref rhs) => {
                let tlhs = self.expr(lhs);
                let trhs = self.expr(rhs);
                let trhs = self.coerce_expr(tlhs.ty(), trhs);
                let ty = tlhs.ty().clone();
                (TypedExpression_::Assignment(box tlhs, box trhs),
                 ty)
            }
            InstanceOf(box ref obj, ref tyname) => {
                // ($15.20.2) instanceof rules
                let tobj = self.expr(obj);
                let ty = self.env.resolve_type(tyname);

                if !tobj.ty().is_reference() && !tobj.ty().is_null() {
                    span_error!(obj.span,
                                "type mismatch: expected reference type, found `{}`", tobj.ty());
                }
                if !ty.is_reference() {
                    span_error!(tyname.span,
                                "type mismatch: expected reference type, found `{}`", ty);
                }

                (TypedExpression_::InstanceOf(box tobj, ty),
                 Type::SimpleType(SimpleType::Boolean))
            }
            Prefix(op, box ref arg) => {
                use ast::PrefixOperator::*;
                let mut targ = self.expr(arg);
                let ty = match op {
                    Not => {
                        expect_expr(&Type::SimpleType(SimpleType::Boolean), &targ);
                        Type::SimpleType(SimpleType::Boolean)
                    }
                    Minus => {
                        targ = self.coerce_expr(&Type::SimpleType(SimpleType::Int), targ);
                        Type::SimpleType(SimpleType::Int)
                    }
                };
                (TypedExpression_::Prefix(op, box targ), ty)
            }
            Infix(op, box ref l, box ref r) => {
                use ast::InfixOperator::*;
                let mut tl = self.expr(l);
                let mut tr = self.expr(r);
                let bool_ty = Type::SimpleType(SimpleType::Boolean);
                let ty = match op {
                    Xor | EagerOr | EagerAnd | LazyOr | LazyAnd => {
                        expect_expr(&Type::SimpleType(SimpleType::Boolean), &tl);
                        expect_expr(&Type::SimpleType(SimpleType::Boolean), &tr);
                        bool_ty
                    }
                    Equals | NotEquals => {
                        // ($15.21) Equality rules
                        let lty = tl.ty().clone();
                        let rty = tr.ty().clone();
                        if lty.is_unknown() || rty.is_unknown() {
                            // OK, already error
                        } else if lty.is_numeric() && rty.is_numeric() {
                            // OK, numeric equality
                            tl = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tl);
                            tr = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tr);
                        } else if lty == bool_ty && rty == bool_ty {
                            // OK, bool equality
                        } else if lty.is_reference() && rty.is_reference() {
                            // OK, reference equality with casting conversions
                            if !self.check_casting_conversion(&lty, &rty) {
                                span_error!(expr.span,
                                            "type mismatch: cannot cast between `{}` and `{}`",
                                            lty, rty);
                            }
                        } else if (lty.is_reference() || lty.is_null())
                               && (rty.is_reference() || rty.is_null()) {
                        } else {
                            span_error!(expr.span,
                                        "type mismatch: incomparable types `{}` and `{}`",
                                        lty, rty);
                        }
                        bool_ty
                    }
                    LessThan | GreaterThan | LessEqual | GreaterEqual => {
                        // promote
                        tl = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tl);
                        tr = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tr);
                        bool_ty
                    }
                    Plus => {
                        let string_ty = Type::object(self.lang_items.string);
                        if *tl.ty() == string_ty || *tr.ty() == string_ty {
                            if *tl.ty() == Type::Void {
                                span_error!(expr.span,
                                            "type mismatch: + operator cannot be applied to void and String");
                            } else if *tr.ty() == Type::Void {
                                span_error!(expr.span,
                                            "type mismatch: + operator cannot be applied to String and void");
                            } else {
                                // string concatenation
                                if *tl.ty() != string_ty {
                                    tl = self.stringify(tl);
                                }
                                if *tr.ty() != string_ty {
                                    tr = self.stringify(tr);
                                }
                            }
                           string_ty
                        } else {
                            tl = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tl);
                            tr = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tr);
                            Type::SimpleType(SimpleType::Int)
                        }
                    }
                    Minus | Mult | Div | Modulo => {
                        tl = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tl);
                        tr = self.coerce_expr(&Type::SimpleType(SimpleType::Int), tr);
                        Type::SimpleType(SimpleType::Int)
                    }
                };
                (TypedExpression_::Infix(op, box tl, box tr), ty)
            }
            Cast(ref to, box ref expr) => {
                let e = self.expr(expr);
                let ty = self.env.resolve_type(to);
                // FIXME check valid cast
                // TODO distinguish between down and upcasts, for codegen
                (TypedExpression_::Cast(ty.clone(), box e), ty)
            }
        }
    }

    fn lit(&self, lit: &'ast ast::Literal) -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {
        use ast::Literal::*;
        (TypedExpression_::Literal(lit),
         match *lit {
            Integer(..) => Type::SimpleType(SimpleType::Int),
            Boolean(..) => Type::SimpleType(SimpleType::Boolean),
            Character(..) => Type::SimpleType(SimpleType::Char),
            String(..) => Type::object(self.lang_items.string),
            Null => Type::Null,
        })
    }

    fn stringify(&self, expr: TypedExpression<'a, 'ast>) -> TypedExpression<'a, 'ast> {
        spanned(expr.span, (TypedExpression_::ToString(box expr),
                            Type::object(self.lang_items.string)))
    }
}

pub fn populate_method<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                 env: Environment<'a, 'ast>,
                                 method: MethodRef<'a, 'ast>,
                                 lang_items: &LangItems<'a, 'ast>) {
    let mut typer = Typer {
        arena: arena,
        env: env,
        lang_items: lang_items,
    };
    *method.args.borrow_mut() = method.ast.params.iter().map(|decl| typer.new_var(decl)).collect();
    if let Some(ref block) = method.ast.body {
        *method.body.borrow_mut() = Some(typer.block(block));
    }
}

pub fn populate_constructor<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                      env: Environment<'a, 'ast>,
                                      ctor: ConstructorRef<'a, 'ast>,
                                      lang_items: &LangItems<'a, 'ast>) {
    let mut typer = Typer {
        arena: arena,
        env: env,
        lang_items: lang_items,
    };
    *ctor.args.borrow_mut() = ctor.ast.params.iter().map(|decl| typer.new_var(decl)).collect();
    *ctor.body.borrow_mut() = Some(typer.block(&ctor.ast.body));
}

pub fn populate_field<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                env: Environment<'a, 'ast>,
                                field: FieldRef<'a, 'ast>,
                                lang_items: &LangItems<'a, 'ast>) {
    if let Some(ref expr) = field.ast.initializer {
        let mut typer = Typer {
            arena: arena,
            env: env,
            lang_items: lang_items,
        };
        let texpr = typer.expr(expr);
        let texpr = typer.coerce_expr(&field.ty, texpr);
        *field.initializer.borrow_mut() = Some(texpr);
    }
}
