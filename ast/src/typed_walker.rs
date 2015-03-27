use middle::*;
use name::*;

pub trait Walker<'a, 'ast>: StatementWalker<'a, 'ast> {
    fn walk_universe(&mut self, universe: &Universe<'a, 'ast>) { default_walk_universe(self, universe); }
    fn walk_package(&mut self, package: PackageRef<'a, 'ast>) { default_walk_package(self, package); }
    fn walk_type_definition(&mut self, tydef: TypeDefinitionRef<'a, 'ast>) { default_walk_type_definition(self, tydef); }
    fn walk_field(&mut self, name: Symbol, field: FieldRef<'a, 'ast>) { default_walk_field(self, name, field); }
    fn walk_method(&mut self, signature: &MethodSignature<'a, 'ast>, method: MethodRef<'a, 'ast>) { default_walk_method(self, signature, method); }
    fn walk_method_impl(&mut self, method: MethodImplRef<'a, 'ast>) { default_walk_method_impl(self, method); }
    fn walk_constructor(&mut self, constructor: ConstructorRef<'a, 'ast>) { default_walk_constructor(self, constructor); }
}

pub trait StatementWalker<'a, 'ast>: ExpressionWalker<'a, 'ast> {
    fn walk_variable(&mut self, var: VariableRef<'a, 'ast>) { default_walk_variable(self, var); }
    fn walk_local_variable(&mut self, var: &TypedLocalVariable<'a, 'ast>) { default_walk_local_variable(self, var); }
    fn walk_block(&mut self, block: &TypedBlock<'a, 'ast>) { default_walk_block(self, block); }
    fn walk_block_statement(&mut self, stmt: &TypedBlockStatement<'a, 'ast>) { default_walk_block_statement(self, stmt); }
    fn walk_statement(&mut self, stmt: &TypedStatement<'a, 'ast>) { default_walk_statement(self, stmt); }
}

pub trait ExpressionWalker<'a, 'ast>: Sized {
    fn walk_expression(&mut self, expr: &TypedExpression<'a, 'ast>) { default_walk_expression(self, expr); }
}

pub fn default_walk_universe<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, universe: &Universe<'a, 'ast>) {
    walker.walk_package(universe.toplevel);
    walker.walk_package(universe.default);
}

pub fn default_walk_package<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, package: PackageRef<'a, 'ast>) {
    for (_, item) in package.contents.borrow().iter() {
        match *item {
            PackageItem::Package(package) => walker.walk_package(package),
            PackageItem::TypeDefinition(tydef) => walker.walk_type_definition(tydef),
        }
    }
}
pub fn default_walk_type_definition<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, tydef: TypeDefinitionRef<'a, 'ast>) {
    for (&name, &field) in tydef.fields.iter() {
        walker.walk_field(name, field);
    }
    for (sig, &method) in tydef.methods.iter() {
        walker.walk_method(sig, method);
    }
    for &method in tydef.method_impls.iter() {
        walker.walk_method_impl(method);
    }
    for (_, &ctor) in tydef.constructors.iter() {
        walker.walk_constructor(ctor);
    }
}
pub fn default_walk_field<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, _name: Symbol, field: FieldRef<'a, 'ast>) {
    if let Some(ref init) = *field.initializer {
        walker.walk_expression(init);
    }
}
pub fn default_walk_method<'a, 'ast, W: Walker<'a, 'ast>>(_walker: &mut W, _signature: &MethodSignature<'a, 'ast>, _method: MethodRef<'a, 'ast>) {
}
pub fn default_walk_method_impl<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, method: MethodImplRef<'a, 'ast>) {
    for &arg in method.args.iter() {
        walker.walk_variable(arg);
    }
    if let Some(ref body) = *method.body {
        walker.walk_block(body);
    }
}
pub fn default_walk_constructor<'a, 'ast, W: Walker<'a, 'ast>>(walker: &mut W, constructor: ConstructorRef<'a, 'ast>) {
    for &arg in constructor.args.iter() {
        walker.walk_variable(arg);
    }
    walker.walk_block(&*constructor.body);
}
pub fn default_walk_variable<'a, 'ast, W: StatementWalker<'a, 'ast>>(_walker: &mut W, _var: VariableRef<'a, 'ast>) {
}
pub fn default_walk_local_variable<'a, 'ast, W: StatementWalker<'a, 'ast>>(walker: &mut W, var: &TypedLocalVariable<'a, 'ast>) {
    walker.walk_variable(var.variable);
    walker.walk_expression(&var.initializer);
}
pub fn default_walk_block<'a, 'ast, W: StatementWalker<'a, 'ast>>(walker: &mut W, block: &TypedBlock<'a, 'ast>) {
    for stmt in block.stmts.iter() {
        walker.walk_block_statement(stmt);
    }
}
pub fn default_walk_block_statement<'a, 'ast, W: StatementWalker<'a, 'ast>>(walker: &mut W, stmt: &TypedBlockStatement<'a, 'ast>) {
    use middle::TypedBlockStatement_::*;
    match stmt.node {
        LocalVariable(ref var) => {
            walker.walk_local_variable(var);
        }
        Statement(ref stmt) => {
            walker.walk_statement(stmt);
        }
    }
}
pub fn default_walk_statement<'a, 'ast, W: StatementWalker<'a, 'ast>>(walker: &mut W, stmt: &TypedStatement<'a, 'ast>) {
    use middle::TypedStatement_::*;
    match stmt.node {
        Expression(ref expr) => walker.walk_expression(expr),
        If(ref test, box ref ift, ref iff) => {
            walker.walk_expression(test);
            walker.walk_statement(ift);
            if let Some(box ref iff) = *iff {
                walker.walk_statement(iff);
            }
        }
        While(ref test, box ref body) => {
            walker.walk_expression(test);
            walker.walk_statement(body);
        }
        For(ref expr1, ref expr2, ref expr3, box ref stmt) => {
            if let Some(ref expr1) = *expr1 {
                walker.walk_expression(expr1);
            }
            if let Some(ref expr2) = *expr2 {
                walker.walk_expression(expr2);
            }
            if let Some(ref expr3) = *expr3 {
                walker.walk_expression(expr3);
            }
            walker.walk_statement(stmt);
        }
        ForDecl(ref var, ref expr1, ref expr2, box ref stmt) => {
            walker.walk_local_variable(var);
            if let Some(ref expr1) = *expr1 {
                walker.walk_expression(expr1);
            }
            if let Some(ref expr2) = *expr2 {
                walker.walk_expression(expr2);
            }
            walker.walk_statement(stmt);
        }
        Return(ref expr) => {
            if let Some(ref expr) = *expr {
                walker.walk_expression(expr);
            }
        }
        Block(ref block) => walker.walk_block(block),
        Empty => {}
    }
}

pub fn default_walk_expression<'a, 'ast, W: ExpressionWalker<'a, 'ast>>(walker: &mut W, expr: &TypedExpression<'a, 'ast>) {
    use middle::TypedExpression_::*;
    match expr.node.0 {
        Literal(_) => (),
        This => (),

        NewDynamicClass(box ref _expr, _, ref _exprs) => panic!(),
        MethodInvocation(ref expr, _, ref exprs) => {
            if let Some(box ref expr) = *expr {
                walker.walk_expression(expr);
            }
            for expr in exprs.iter() {
                walker.walk_expression(expr);
            }
        },

        NewStaticClass(_, _, ref exprs) => {
            for expr in exprs.iter() {
                walker.walk_expression(expr);
            }
        },

        StaticFieldAccess(_) => (),
        ThisFieldAccess(_) => (),

        Variable(_) => (),

        NewArray(_, box ref expr)
        | FieldAccess(box ref expr, _)
        | Prefix(_, box ref expr)
        | Cast(_, box ref expr)
        | Widen(box ref expr)
        | ToString(box ref expr)
        | ArrayLength(box ref expr) => {
            walker.walk_expression(expr);
        },

        ArrayAccess(box ref expr1, box ref expr2)
        | Assignment(box ref expr1, box ref expr2)
        | Infix(_, box ref expr1, box ref expr2)
        | Concat(box ref expr1, box ref expr2) => {
            walker.walk_expression(expr1);
            walker.walk_expression(expr2);
        }

        InstanceOf(box ref expr, _) => {
            walker.walk_expression(expr);
        },
    }
}
