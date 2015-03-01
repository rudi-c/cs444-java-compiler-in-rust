use ast;
use name::*;
use span::*;
use middle::*;
use arena::*;

use name_resolve::Environment;

struct Typer<'a, 'ast: 'a> {
    arena: &'a Arena<'a, 'ast>,
    env: Environment<'a, 'ast>,
}

fn unify<'a, 'ast, S: IntoSpan>(expect: &Type<'a, 'ast>, actual: &Type<'a, 'ast>, sp: S) {
    // If a type is `Unknown` then an error was already emitted -
    // don't make it worse
    if let Type::Unknown = *expect {
    } else if let Type::Unknown = *actual {
    } else if *expect != *actual {
        // TODO: since expression typing is completely broken, don't actually error here
        span_warning!(sp.into_span(),
                    "type mismatch: expected `{}`, found `{}`",
                    expect, actual);
    }
}

impl<'a, 'ast> Typer<'a, 'ast> {
    fn new_var(&mut self, var: &'ast ast::VariableDeclaration) -> VariableRef<'a, 'ast> {
        let def = self.arena.alloc(VariableDef::new(
                format!("{}.{}", self.env.ty.fq_name, var.name),
                self.env.resolve_type(&var.ty),
                var));
        self.env.add_var(var.name.node, def);
        def
    }

    fn local_variable(&mut self, var: &'ast ast::LocalVariable) -> TypedLocalVariable<'a, 'ast> {
        let v = self.new_var(&var.variable);
        let init = self.expr(&var.initializer);
        unify(&v.ty, init.ty(), &var.initializer);
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
            LocalClass(ref class) => panic!("local class"),
            Statement(ref stmt) => TypedBlockStatement_::Statement(self.stmt(stmt)),
        })
    }

    fn stmt(&mut self, stmt: &'ast ast::Statement) -> TypedStatement<'a, 'ast> {
        use ast::Statement_::*;
        spanned(stmt.span, match stmt.node {
            Expression(ref expr) => TypedStatement_::Expression(self.expr(expr)),
            If(ref test, box ref then, ref els) =>
                TypedStatement_::If(self.expr(test),
                                   box self.stmt(then),
                                   els.as_ref().map(|&box ref s| box self.stmt(s))),
            While(ref test, box ref inner) =>
                TypedStatement_::While(self.expr(test), box self.stmt(inner)),
            For(ref init, ref test, ref update, box ref inner) =>
                TypedStatement_::For(
                    init.as_ref().map(|i| self.expr(i)),
                    test.as_ref().map(|t| self.expr(t)),
                    update.as_ref().map(|u| self.expr(u)),
                    box self.stmt(inner)),
            ForDecl(ref init, ref test, ref update, box ref inner) =>
                TypedStatement_::ForDecl(
                    self.local_variable(init),
                    test.as_ref().map(|t| self.expr(t)),
                    update.as_ref().map(|u| self.expr(u)),
                    box self.stmt(inner)),
            Empty => TypedStatement_::Empty,
            Return(ref expr) => TypedStatement_::Return(self.expr(expr)),
            Block(ref block) => TypedStatement_::Block(self.block(block)),
        })
    }

    fn expr(&mut self, expr: &'ast ast::Expression) -> TypedExpression<'a, 'ast> {
        use ast::Expression_::*;
        static NULL: ast::Literal = ast::Literal::Null;
        spanned(expr.span, match expr.node {
            Literal(ref lit) => self.lit(lit),
            This => (TypedExpression_::This, Type::object(Some(self.env.ty))),
            NewStaticClass(ref id, ref args) => {
                let ty = Type::object(self.env.resolve_type_name(&*id.parts));
                // FIXME: Check constructor call
                (TypedExpression_::NewStaticClass(
                    ty.clone(),
                    args.iter().map(|arg| self.expr(arg)).collect()
                ), ty)
            }
            NewDynamicClass(box _, _, _) => panic!("dynamic class"),
            NewArray(ref tyname, box ref size) => {
                let sty = self.env.resolve_simple_type(tyname);
                let tsize = self.expr(size);
                unify(&Type::SimpleType(SimpleType::Int), tsize.ty(), &tsize);
                match sty {
                    Some(sty) => {
                        let ty = Type::ArrayType(sty.clone());
                        (TypedExpression_::NewArray(sty, box tsize), ty)
                    }
                    None => {
                        // TODO: more reasonable dummy value
                        let mut r = self.lit(&NULL);
                        r.1 = Type::Unknown;
                        r
                    }
                }
            }
            FieldAccess(box ref expr, ref _name) => {
                let _texpr = self.expr(expr);
                // TODO
                self.lit(&NULL)
            }
            MethodInvocation(ref callee, ref _name, ref args) => {
                let _tcallee = callee.as_ref().map(|&box ref c| box self.expr(c));
                let _targs: Vec<_> = args.iter()
                    .map(|arg| self.expr(arg))
                    .collect();
                // TODO
                self.lit(&NULL)
            }
            ArrayAccess(box ref array, box ref ix) => {
                let tarray = self.expr(array);
                let tix = self.expr(ix);
                unify(&Type::SimpleType(SimpleType::Int), tix.ty(), &tix);
                match tarray.ty().clone() {
                    Type::ArrayType(sty) =>
                        (TypedExpression_::ArrayAccess(box tarray, box tix),
                        Type::SimpleType(sty)),
                    Type::Unknown => {
                        // TODO: more reasonable dummy value
                        let mut r = self.lit(&NULL);
                        r.1 = Type::Unknown;
                        r
                    }
                    // TODO: how to handle null
                    ty => {
                        // TODO: as in `unify`, needs to be an error later
                        span_warning!(array.span,
                                    "type mismatch: expected array, found `{}`",
                                    ty);
                        // TODO: more reasonable dummy value
                        let mut r = self.lit(&NULL);
                        r.1 = Type::Unknown;
                        r
                    }
                }
            }
            Name(ref _ident) => {
                // TODO
                self.lit(&NULL)
            }
            Assignment(box ref lhs, box ref rhs) => {
                let tlhs = self.expr(lhs);
                let trhs = self.expr(rhs);
                unify(tlhs.ty(), trhs.ty(), trhs.span);
                let ty = tlhs.ty().clone();
                // TODO: check if this is the right type
                (TypedExpression_::Assignment(box tlhs, box trhs),
                 ty)
            }
            InstanceOf(box ref obj, ref tyname) => {
                let tobj = self.expr(obj);
                let ty = self.env.resolve_type(tyname);
                // TODO: do we need to check that `ty` is anything in particular?
                (TypedExpression_::InstanceOf(box tobj, ty),
                 Type::SimpleType(SimpleType::Boolean))
            }
            Prefix(ref _op, box ref arg) => {
                let _targ = self.expr(arg);
                // TODO
                self.lit(&NULL)
            }
            Infix(ref _op, box ref l, box ref r) => {
                let _tl = self.expr(l);
                let _tr = self.expr(r);
                // TODO
                self.lit(&NULL)
            }
            Cast(ref to, box ref expr) => {
                let e = self.expr(expr);
                let ty = self.env.resolve_type(to);
                // FIXME check valid cast
                // TODO distinguish between down and upcasts, for codegen
                (TypedExpression_::Cast(ty.clone(), box e), ty)
            }
        })
    }

    fn lit(&mut self, lit: &'ast ast::Literal) -> (TypedExpression_<'a, 'ast>, Type<'a, 'ast>) {
        use ast::Literal::*;
        (TypedExpression_::Literal(lit),
         match *lit {
            Integer(..) => Type::SimpleType(SimpleType::Int),
            Boolean(..) => Type::SimpleType(SimpleType::Boolean),
            Character(..) => Type::SimpleType(SimpleType::Char),
            String(..) => Type::Unknown, // FIXME
            Null => Type::Null,
        })
    }
}

pub fn populate_method<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                 env: Environment<'a, 'ast>,
                                 method: MethodRef<'a, 'ast>) {
    let mut typer = Typer {
        arena: arena,
        env: env,
    };
    *method.args.borrow_mut() = method.ast.params.iter().map(|decl| typer.new_var(decl)).collect();
    if let Some(ref block) = method.ast.body {
        *method.body.borrow_mut() = Some(typer.block(block));
    }
}

pub fn populate_constructor<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                      env: Environment<'a, 'ast>,
                                      ctor: ConstructorRef<'a, 'ast>) {
    let mut typer = Typer {
        arena: arena,
        env: env,
    };
    *ctor.args.borrow_mut() = ctor.ast.params.iter().map(|decl| typer.new_var(decl)).collect();
    *ctor.body.borrow_mut() = Some(typer.block(&ctor.ast.body));
}

pub fn populate_field<'a, 'ast>(arena: &'a Arena<'a, 'ast>,
                                env: Environment<'a, 'ast>,
                                field: FieldRef<'a, 'ast>) {
    if let Some(ref expr) = field.ast.initializer {
        let mut typer = Typer {
            arena: arena,
            env: env,
        };
        let texpr = typer.expr(expr);
        unify(&field.ty, texpr.ty(), &texpr);
        *field.initializer.borrow_mut() = Some(texpr);
    }
}
