use ast::*;

/// A trait to help walk ASTs.
/// 'a is the lifetime of the AST reference.
/// To use this trait, override methods as desired; then you usually want to add a call to
/// `default_walk_*` at the end of your method (unless you want to stop descending or use custom
/// walk logic).
pub trait Walker<'a>: Sized {
    fn walk_compilation_unit(&mut self, ast: &'a CompilationUnit) { default_walk_compilation_unit(self, ast); }
    fn walk_type_declaration(&mut self, ty_decl: &'a TypeDeclaration) { default_walk_type_declaration(self, ty_decl); }
    fn walk_interface(&mut self, interface: &'a Interface) { default_walk_interface(self, interface); }
    fn walk_interface_method(&mut self, method: &'a Method) { default_walk_interface_method(self, method); }
    fn walk_class(&mut self, class: &'a Class) { default_walk_class(self, class); }
    fn walk_class_method(&mut self, method: &'a Method) { default_walk_class_method(self, method); }
    fn walk_class_field(&mut self, field: &'a Field) { default_walk_class_field(self, field); }
    fn walk_method(&mut self, method: &'a Method) { default_walk_method(self, method); }
    fn walk_variable_declaration(&mut self, decl: &'a VariableDeclaration) { default_walk_variable_declaration(self, decl); }
    fn walk_local_variable(&mut self, var: &'a LocalVariable) { default_walk_local_variable(self, var); }
    fn walk_constructor(&mut self, ctor: &'a Constructor) { default_walk_constructor(self, ctor); }
    fn walk_block(&mut self, block: &'a Block) { default_walk_block(self, block); }
    fn walk_statement(&mut self, statement: &'a Statement) { default_walk_statement(self, statement); }
    fn walk_expression(&mut self, expr: &'a Expression) { default_walk_expression(self, expr); }
}

pub fn default_walk_compilation_unit<'a, T: Walker<'a>>(walker: &mut T, ast: &'a CompilationUnit) {
    for ty_decl in ast.types.iter() {
        walker.walk_type_declaration(ty_decl);
    }
}
pub fn default_walk_type_declaration<'a, T: Walker<'a>>(walker: &mut T, ty_decl: &'a TypeDeclaration) {
    match ty_decl.node {
        TypeDeclaration_::Class(ref class) => {
            walker.walk_class(class);
        },
        TypeDeclaration_::Interface(ref interface) => {
            walker.walk_interface(interface);
        },
    }
}
pub fn default_walk_interface<'a, T: Walker<'a>>(walker: &mut T, interface: &'a Interface) {
    for method in interface.node.body.iter() {
        walker.walk_interface_method(method);
    }
}
pub fn default_walk_interface_method<'a, T: Walker<'a>>(walker: &mut T, method: &'a Method) {
    walker.walk_method(method);
}
pub fn default_walk_class<'a, T: Walker<'a>>(walker: &mut T, class: &'a Class) {
    for body_declaration in class.node.body.iter() {
        match body_declaration.node {
            ClassBodyDeclaration_::FieldDeclaration(ref field) =>
                walker.walk_class_field(field),
            ClassBodyDeclaration_::MethodDeclaration(ref method) =>
                walker.walk_class_method(method),
            ClassBodyDeclaration_::ConstructorDeclaration(ref constructor) =>
                walker.walk_constructor(constructor),
        }
    }
}
pub fn default_walk_variable_declaration<'a, T: Walker<'a>>(_walker: &mut T, _decl: &'a VariableDeclaration) {
}
pub fn default_walk_local_variable<'a, T: Walker<'a>>(walker: &mut T, var: &'a LocalVariable) {
    walker.walk_variable_declaration(&var.node.variable);
    walker.walk_expression(&var.node.initializer);
}
pub fn default_walk_constructor<'a, T: Walker<'a>>(walker: &mut T, ctor: &'a Constructor) {
    for decl in ctor.node.params.iter() {
        walker.walk_variable_declaration(decl);
    }
    walker.walk_block(&ctor.node.body);
}
pub fn default_walk_class_method<'a, T: Walker<'a>>(walker: &mut T, method: &'a Method) {
    walker.walk_method(method);
}
pub fn default_walk_class_field<'a, T: Walker<'a>>(walker: &mut T, field: &'a Field) {
    if let Some(ref init) = field.node.initializer {
        walker.walk_expression(init);
    }
}
pub fn default_walk_method<'a, T: Walker<'a>>(walker: &mut T, method: &'a Method) {
    for decl in method.node.params.iter() {
        walker.walk_variable_declaration(decl);
    }
    if let Some(ref body) = method.node.body {
        walker.walk_block(body);
    }
}
pub fn default_walk_block<'a, T: Walker<'a>>(walker: &mut T, block: &'a Block) {
    for stmt in block.node.stmts.iter() {
        match stmt.node {
            BlockStatement_::Statement(ref statement) =>
                walker.walk_statement(statement),
            BlockStatement_::LocalVariable(ref local) =>
                walker.walk_local_variable(local),
            BlockStatement_::LocalClass(_) => {
                // XXX: What is this doing here?
                panic!("local class?")
            }
        }
    }
}
pub fn default_walk_statement<'a, T: Walker<'a>>(walker: &mut T, statement: &'a Statement) {
    match statement.node {
        Statement_::Expression(ref expr) => walker.walk_expression(expr),
        Statement_::If(ref expr, box ref stmt1, None) => {
            walker.walk_expression(expr);
            walker.walk_statement(stmt1);
        },
        Statement_::If(ref expr, box ref stmt1, Some(box ref stmt2)) => {
            walker.walk_expression(expr);
            walker.walk_statement(stmt1);
            walker.walk_statement(stmt2);
        },
        Statement_::While(ref expr, box ref stmt) => {
            walker.walk_expression(expr);
            walker.walk_statement(stmt);
        },
        Statement_::For(ref expr1, ref expr2, ref expr3, box ref stmt) => {
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
        Statement_::ForDecl(ref var, ref expr1, ref expr2, box ref stmt) => {
            walker.walk_local_variable(var);
            if let Some(ref expr1) = *expr1 {
                walker.walk_expression(expr1);
            }
            if let Some(ref expr2) = *expr2 {
                walker.walk_expression(expr2);
            }
            walker.walk_statement(stmt);
        }
        Statement_::Return(ref expr) => {
            if let Some(ref expr) = *expr {
                walker.walk_expression(expr);
            }
        }
        Statement_::Block(ref block) => walker.walk_block(block),
        Statement_::Empty => {}
    }
}
pub fn default_walk_expression<'a, T: Walker<'a>>(walker: &mut T, expression: &'a Expression) {
    match expression.node {
        Expression_::Literal(_) => (),
        Expression_::This => (),

        Expression_::NewDynamicClass(box ref expr, _, ref exprs)
        | Expression_::MethodInvocation(box ref expr, _, ref exprs) => {
            walker.walk_expression(expr);
            for expr in exprs.iter() {
                walker.walk_expression(expr);
            }
        },

        Expression_::NewStaticClass(_, ref exprs)
        | Expression_::NamedMethodInvocation(_, ref exprs) => {
            for expr in exprs.iter() {
                walker.walk_expression(expr);
            }
        },

        Expression_::NewArray(_, box ref expr)
        | Expression_::FieldAccess(box ref expr, _)
        | Expression_::Prefix(_, box ref expr)
        | Expression_::Cast(_, box ref expr) => {
            walker.walk_expression(expr);
        },

        Expression_::ArrayAccess(box ref expr1, box ref expr2)
        | Expression_::Assignment(box ref expr1, box ref expr2)
        | Expression_::Infix(_, box ref expr1, box ref expr2) => {
            walker.walk_expression(expr1);
            walker.walk_expression(expr2);
        }

        Expression_::Name(_) => (),

        Expression_::InstanceOf(box ref expr, _) => {
            walker.walk_expression(expr);
        },
    }
}
