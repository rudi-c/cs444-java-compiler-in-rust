use ast;
use span::*;
use middle::*;
use arena::*;

use name_resolve::Environment;
use lang_items::LangItems;
use uses::check_not_used;

use std::borrow::ToOwned;

struct Typer<'l, 'a: 'l, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    env: Environment<'a, 'ast>,
    // FIXME: This is not really the right place for this, IMO
    ret_ty: Option<Type<'a, 'ast>>,
    lang_items: &'l LangItems<'a, 'ast>,

    // true if we are typing a static method or field, can't access implicit
    // nonstatic thid members
    require_static: bool,
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

fn check_lvalue<'a, 'ast>(expr: &TypedExpression<'a, 'ast>) {
    match expr.node.0 {
        TypedExpression_::StaticFieldAccess(..) |
            TypedExpression_::FieldAccess(..) |
            TypedExpression_::ThisFieldAccess(..) |
            TypedExpression_::Variable(..) |
            TypedExpression_::ArrayAccess(..) => {
            // OK
        }
        _ => {
            span_error!(expr.span,
                        "expression is not a variable");
        }
    }
}

fn numeric_width<'a, 'ast>(ty: &SimpleType<'a, 'ast>) -> i32 {
    use middle::SimpleType::*;
    match *ty {
        Byte => 1,
        Short | Char => 2,
        Int => 4,
        _ => panic!("not numeric")
    }
}

impl<'l, 'a, 'ast> Typer<'l, 'a, 'ast> {
    // Check that there is a legal widening conversion from `source` to `target`.
    // Emits an error if there is not.
    fn check_simple_widening(&self,
                             target: &SimpleType<'a, 'ast>,
                             source: &SimpleType<'a, 'ast>) -> Result<(), String> {
        use middle::SimpleType::*;
        if target == source {
            return Ok(())
        }
        match *target {
            Char | Boolean => {
                Err(format!("no implicit conversion to `{}`", target))
            }
            Byte | Short | Int => match *source {
                Byte | Short | Char | Int => {
                    if numeric_width(target) <= numeric_width(source) {
                        Err(format!("cast required for narrowing conversion from `{}` to `{}`",
                                    source, target))
                    } else {
                        Ok(())
                    }
                }
                Boolean => {
                    Err(format!("cannot convert from `{}` to `{}`", source, target))
                }
                _ => {
                    Err(format!("cannot convert from non-primitive type `{}` to `{}`",
                                 source, target))
                }
            },
            Other(expect_tydef) => match *source {
                Other(expr_tydef) => {
                    if !self.env.is_subtype(expr_tydef, expect_tydef) {
                        if self.env.is_subtype(expect_tydef, expr_tydef) {
                            Err(format!("cast required for narrowing conversion from `{}` to `{}`",
                                        source, target))
                        } else {
                            Err(format!("no conversion from `{}` to `{}`",
                                        source, target))
                        }
                    } else {
                        Ok(())
                    }
                }
                _ => {
                    Err(format!("cannot convert from primitive type `{}` to `{}`",
                                source, target))
                }
            },
        }
    }

    fn check_widening(&self, target: &Type<'a, 'ast>, source: &Type<'a, 'ast>)
            -> Result<(), String> {

        if target == source {
            return Ok(())
        }
        match (target, source) {
            (_, &Type::Unknown) | (&Type::Unknown, _) => Ok(()),

            (&Type::Null, _) => panic!("coerce to null type?"),

            (&Type::Void, _)
            | (_, &Type::Void) => {
                Err("cannot convert void types".to_owned())
            }

            // simple types might be convertible to other simple types
            (&Type::SimpleType(ref a), &Type::SimpleType(ref b)) => {
                self.check_simple_widening(a, b)
            }
            // ... but not to arrays
            (&Type::ArrayType(_), &Type::SimpleType(_)) => {
                Err(format!("cannot convert from `{}` to array type `{}`",
                            source, target))
            }

            // arrays can be converted to some reference types
            (&Type::SimpleType(SimpleType::Other(expect_tydef)), &Type::ArrayType(_)) => {
                if expect_tydef != self.lang_items.object
                    && expect_tydef != self.lang_items.cloneable
                    && expect_tydef != self.lang_items.serializable {
                    Err(format!("cannot convert from array type `{}` to `{}`",
                                source, target))
                } else {
                    Ok(())
                }
            }
            // ... or undergo reference conversion
            (&Type::ArrayType(SimpleType::Other(expect_tydef)),
             &Type::ArrayType(SimpleType::Other(expr_tydef))) => {
                if !self.env.is_subtype(expr_tydef, expect_tydef) {
                    if self.env.is_subtype(expect_tydef, expr_tydef) {
                        Err(format!("cast required for narrowing conversion from `{}` to `{}`",
                                    source, target))
                    } else {
                        Err(format!("no conversion from `{}` to `{}`",
                                    source, target))
                    }
                } else {
                    Ok(())
                }
            }
            // ... but any other kind of conversion
            (&Type::ArrayType(_), &Type::ArrayType(_)) => {
                Err(format!("cannot convert from array type `{}` to array type `{}`",
                            source, target))
            }
            (&Type::SimpleType(_), &Type::ArrayType(_)) => {
                Err(format!("cannot convert from array type `{}` to primitive type `{}`",
                            source, target))
            }

            // null can be converted to any reference type
            (&Type::SimpleType(SimpleType::Other(_)), &Type::Null) |
            (&Type::ArrayType(_), &Type::Null) => Ok(()),
            // ... but not anything else
            (&Type::SimpleType(_), &Type::Null) => {
                Err(format!("cannot convert null to primitive type `{}`", target))
            }
        }
    }

    // (5.1.3) Narrowing conversion rules
    fn check_narrowing_conversion(&self, target: &Type<'a, 'ast>, source: &Type<'a, 'ast>) -> bool {
        use middle::SimpleType::*;
        match (target, source) {
            (&Type::SimpleType(ref starget), &Type::SimpleType(ref ssource)) => {
                match (starget, ssource) {
                    (&Byte, &Char) => true,
                    _ => {
                        if starget.is_numeric() && ssource.is_numeric() {
                            numeric_width(starget) <= numeric_width(ssource)
                        } else {
                            false
                        }
                    }
                }
            }
            _ => false
        }
    }


    // Returns whether there is a legal Casting Conversion ($5.5)
    // from from `source` to `target`
    // TODO: Make sure the marmoset test cases cover every single one of these rules.
    fn check_casting_conversion(&self,
                                target: &Type<'a, 'ast>,
                                source: &Type<'a, 'ast>) -> bool {
        match self.check_widening(target, source) {
            Ok(_) => { return true }
            _ => {}
        };

        if self.check_narrowing_conversion(target, source) {
            return true
        }

        match (source, target) {
            (&Type::SimpleType(SimpleType::Other(source_tydef)),
             &Type::SimpleType(SimpleType::Other(target_tydef))) => {
                match (source_tydef.kind, target_tydef.kind) {
                    (TypeKind::Class, TypeKind::Class) => {
                        self.env.is_subtype(source_tydef, target_tydef) ||
                        self.env.is_subtype(target_tydef, source_tydef)
                    }
                    (TypeKind::Class, TypeKind::Interface) => {
                        if source_tydef.has_modifier(ast::Modifier_::Final) {
                            self.env.is_subtype(source_tydef, target_tydef)
                        } else {
                            true
                        }
                    }
                    (TypeKind::Interface, TypeKind::Class) => {
                        if target_tydef.has_modifier(ast::Modifier_::Final) {
                            // TODO: Check this (not explicitely written in the spec)
                            self.env.is_subtype(target_tydef, source_tydef)
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
                        self.check_casting_conversion(&Type::SimpleType(target_array_ty.clone()),
                                                      &Type::SimpleType(source_array_ty.clone()))
                    }
                    _ => {
                        source_array_ty == target_array_ty
                    }
                }
            }
            _ => false
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
        if let Err(msg) = self.check_widening(expect, expr.ty()) {
            span_error!(expr.span, "{}", msg);
        }
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
        check_not_used(&init, v);
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
                let mut texpr = if let Some(ref expr) = *expr {
                    Some(self.expr(expr))
                } else {
                    None
                };
                if let Some(ref ret_ty) = self.ret_ty {
                    texpr = match (texpr, ret_ty) {
                        (None, &Type::Void) => None,
                        (None, _) => {
                            span_error!(stmt.span,
                                        "missing value in `return` statement");
                            None
                        }
                        (Some(texpr), &Type::Void) => {
                            span_error!(stmt.span,
                                        "`return` statement with a value in a method returning void");
                            Some(texpr)
                        }
                        (Some(texpr), ty) =>
                            Some(self.coerce_expr(ty, texpr)),
                    }
                } else {
                    span_error!(stmt.span,
                                "`return` statement found outside of a method");
                }
                TypedStatement_::Return(texpr)
            }
            Block(ref block) => TypedStatement_::Block(self.block(block)),
        })
    }

    fn expr(&mut self, expr: &'ast ast::Expression) -> TypedExpression<'a, 'ast> {
        let texpr = self.expr_(expr);

        match texpr.0 {
            TypedExpression_::ThisFieldAccess(field) => {
                // Check that we don't access non-static field in static settings.
                if self.require_static {
                    span_error!(expr.span,
                                "non-static access to static field `{}` in static setting",
                                field.fq_name);
                }
            }
            _ => {}
        }

        spanned(expr.span, texpr)
    }

    fn new_expr_(&mut self, span: Span,
                 tydef: TypeDefinitionRef<'a, 'ast>,
                 args: Vec<TypedExpression<'a, 'ast>>)
        -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {

        let arg_types: Vec<_> = args.iter()
            .map(|expr| expr.node.1.clone())
            .collect();
        if let Some(&constructor) = tydef.constructors.borrow().get(&arg_types) {
            self.env.check_constructor_access_allowed(span, constructor, tydef);
            (TypedExpression_::NewStaticClass(tydef, constructor, args), Type::object(tydef))
        } else {
            // TODO: Better error message.
            span_error!(span, "no matching constructor");
            dummy_expr_()
        }
    }

    fn expr_(&mut self, expr: &'ast ast::Expression) -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {

        use ast::Expression_::*;
        match expr.node {
            Literal(ref lit) => self.lit(lit),
            This => {
                if self.require_static {
                    span_error!(expr.span, "cannot use `this` in static context");
                }
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
                        self.new_expr_(expr.span, tydef, targs)
                    }
                } else {
                    // An error was already emitted.
                    dummy_expr_()
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
                self.env.resolve_field_access(expr.span, texpr, name)
                    .unwrap_or_else(dummy_expr_)
            }
            NamedMethodInvocation(ref _name, ref args) => {
                let _targs: Vec<_> = args.iter()
                    .map(|arg| self.expr(arg))
                    .collect();
                self.env.resolve_named_method_access(expr.span, self.require_static,
                                                     _name, _targs)
                    .unwrap_or_else(dummy_expr_)
            }
            MethodInvocation(box ref callee, ref _name, ref args) => {
                let _tcallee = self.expr(callee);
                let _targs: Vec<_> = args.iter()
                    .map(|arg| self.expr(arg))
                    .collect();
                self.env.resolve_expr_method_access(expr.span, _tcallee, _name, _targs)
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
                check_lvalue(&tlhs);
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
                            if !self.check_casting_conversion(&lty, &rty) &&
                               !self.check_casting_conversion(&rty, &lty) {
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
            Cast(ref target, box ref expr) => {
                let texpr = self.expr(expr);
                let target_ty = self.env.resolve_type(target);
                // TODO distinguish between down and upcasts, for codegen
                if !self.check_casting_conversion(&target_ty, texpr.ty()) {
                    span_error!(expr.span,
                                "type mismatch: cannot cast from `{}` to `{}`",
                                texpr.ty(), target_ty);
                }
                (TypedExpression_::Cast(target_ty.clone(), box texpr), target_ty)
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
                                 method: MethodImplRef<'a, 'ast>,
                                 lang_items: &LangItems<'a, 'ast>) {
    let mut typer = Typer {
        arena: arena,
        env: env,
        ret_ty: Some(method.ret_ty.clone()),
        lang_items: lang_items,
        require_static: method.is_static,
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
    // ($8.8.5) Check that we can make an implicit super() call.
    if let Some(parent) = env.enclosing_type.extends.borrow().first() {
        match parent.constructors.borrow().get(&vec![]) {
            None => {
                // If there is an explicit constructor, then there is no implicit
                // constructor with no arguments.
                if parent.constructors.borrow().len() > 0 {
                    span_error!(ctor.ast.span,
                                "cannot make implicit super() call, no constructor found");
                }
            }
            _ => {}
        }
    }

    let mut typer = Typer {
        arena: arena,
        env: env,
        ret_ty: Some(Type::Void), // constructors can have empty return statements
        lang_items: lang_items,
        require_static: false,
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
            ret_ty: None,
            lang_items: lang_items,
            require_static: field.is_static(),
        };
        let texpr = typer.expr(expr);
        let texpr = typer.coerce_expr(&field.ty, texpr);
        *field.initializer.borrow_mut() = Some(texpr);
    }
}
