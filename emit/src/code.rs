#![macro_use]

use middle::middle::*;
use mangle::Mangle;
use ast::name::Symbol;
use context::Context;
use stack::Stack;
use std::fmt;
use std::borrow::ToOwned;

macro_rules! emit {
    ( $($instr: expr),+ ; $($comment: expr),+ ) => (
        println!("{:<40} ; {}", format!($($instr),+), format!($($comment),+))
    );
    ( $($instr: expr),+ ) => (
        println!($($instr),+)
    );
}

pub fn emit_block<'a, 'ast>(ctx: &Context<'a, 'ast>,
                            stack: &Stack,
                            block: &TypedBlock<'a, 'ast>) {
    stack.scope(|stack| {
        use middle::middle::TypedBlockStatement_::*;
        for stmt in block.stmts.iter() {
            match stmt.node {
                LocalVariable(ref var) => emit_variable(ctx, stack, var),
                Statement(ref stmt) => emit_statement(ctx, stack, stmt),
            }
        }
    });
}

pub fn emit_variable<'a, 'ast>(ctx: &Context<'a, 'ast>,
                               stack: &mut Stack,
                               var: &TypedLocalVariable<'a, 'ast>) {
    emit_expression(ctx, stack, &var.initializer);
    emit!("push eax" ; "variable {}", var.variable.fq_name);
    stack.add_var(var.variable.fq_name);
}

pub fn emit_statement<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                stack: &Stack,
                                stmt: &TypedStatement<'a, 'ast>) {
    use middle::middle::TypedStatement_::*;
    match stmt.node {
        Expression(ref expr) => {
            emit_expression(ctx, stack, expr);
        }
        If(ref expr, box ref ift, ref iff) => {
            emit!("" ; "> begin if statement");
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            emit!("test eax, eax");
            // If it is (i.e. false), jump to `iff`.
            let false_label = ctx.label();
            emit!("jz .L{}", false_label);
            // Otherwise, execute `ift`...
            emit_statement(ctx, stack, ift);
            if let Some(box ref iff) = *iff {
                // then jump over `iff`.
                let end_label = ctx.label();
                emit!("jmp .L{}", end_label);
                emit!(".L{}:", false_label);
                emit_statement(ctx, stack, iff);
                emit!(".L{}:", end_label);
            } else {
                // and we're done.
                emit!(".L{}:", false_label);
            }
            emit!("" ; "> end if statement");
        }
        While(ref expr, box ref inner) => {
            emit!("" ; "> begin while statement");
            let top_label = ctx.label();
            emit!(".L{}:", top_label);
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            emit!("test eax, eax");
            // If it is (i.e. false), jump to the end.
            let end_label = ctx.label();
            emit!("jz .L{}", end_label);
            // Otherwise, run the body...
            emit_statement(ctx, stack, inner);
            // and go back to the top.
            emit!("jmp .L{}", top_label);
            emit!(".L{}:", end_label);
            emit!("" ; "> end while statement");
        }
        For(ref init, ref test, ref update, box ref inner) => {
            emit!("" ; "> begin for statement");
            if let Some(ref init) = *init {
                emit_expression(ctx, stack, init);
            }
            let top_label = ctx.label();
            let end_label = ctx.label();
            emit!(".L{}:", top_label);
            if let Some(ref test) = *test {
                emit_expression(ctx, stack, test);
                emit!("test eax, eax");
                emit!("jz .L{}", end_label);
            }
            emit_statement(ctx, stack, inner);
            if let Some(ref update) = *update {
                emit_expression(ctx, stack, update);
            }
            emit!("jmp .L{}", top_label);
            emit!(".L{}:", end_label);
            emit!("" ; "> end for statement");
        }
        ForDecl(ref var, ref test, ref update, box ref inner) => {
            emit!("" ; "> begin for statement");
            stack.scope(|stack| {
                emit_variable(ctx, stack, var);
                let top_label = ctx.label();
                let end_label = ctx.label();
                emit!(".L{}:", top_label);
                if let Some(ref test) = *test {
                    emit_expression(ctx, stack, test);
                    emit!("test eax, eax");
                    emit!("jz .L{}", end_label);
                }
                emit_statement(ctx, stack, inner);
                if let Some(ref update) = *update {
                    emit_expression(ctx, stack, update);
                }
                emit!("jmp .L{}", top_label);
                emit!(".L{}:", end_label);
            });
            emit!("" ; "> end for statement");
        }
        Empty => (),
        Return(ref expr) => {
            if let Some(ref expr) = *expr {
                emit_expression(ctx, stack, expr);
            }

            // Result is already in `eax`, if any.
            // Just need to unwind the stack.
            emit!("mov esp, ebp");
            emit!("pop ebp");
            emit!("ret 4*{}", stack.args ; "pop {} args off stack after return", stack.args);
        }
        Block(ref block) => emit_block(ctx, stack, block),
    }
}

// Ensure that `eax` is not null.
pub fn check_null() {
    emit!("test eax, eax" ; "check null");
    emit!("jz __exception" ; "null exception");
}

pub struct ConstantValue<'a: 'c + 'v, 'ast: 'a, 'c, 'v>(pub &'c Context<'a, 'ast>, pub &'v Value);

impl<'a, 'ast, 'c, 'v> fmt::Display for ConstantValue<'a, 'ast, 'c, 'v> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self.1 {
            Value::Int(v) => write!(f, "{}", v),
            Value::Short(v) => write!(f, "{}", v),
            Value::Char(v) => write!(f, "{}", v),
            Value::Byte(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", if v { 1 } else { 0 }),
            Value::String(ref v) => write!(f, "stringstruct#{}", self.0.string_constants.get(v).unwrap()),
        }
    }
}

pub fn sizeof_simple_ty(ty: &SimpleType) -> u32 {
    match *ty {
        SimpleType::Char | SimpleType::Short => 2,
        SimpleType::Byte | SimpleType::Boolean => 1,
        _ => 4,
    }
}
pub fn sizeof_ty(ty: &Type) -> u32 {
    match *ty {
        Type::SimpleType(ref simple_ty) => sizeof_simple_ty(simple_ty),
        _ => 4,
    }
}
pub fn sizeof_array_element(ty: &Type) -> u32 {
    match *ty {
        Type::ArrayType(ref simple_ty) => sizeof_simple_ty(simple_ty),
        _ => panic!("not an array type: {}", ty),
    }
}
pub fn size_name(size: u32) -> &'static str {
    match size {
        1 => "byte",
        2 => "word",
        4 => "dword",
        _ => panic!("bad size {}", size),
    }
}
pub fn short_size_name(size: u32) -> char {
    match size {
        1 => 'b',
        2 => 'w',
        4 => 'd',
        _ => panic!("bad size {}", size),
    }
}
// Return the appropriate `mov` instruction to load
// from a location of type `ty`, to a 32-bit register.
pub fn load_simple_ty(ty: &SimpleType) -> &'static str {
    match *ty {
        SimpleType::Byte | SimpleType::Short => "movsx",
        SimpleType::Boolean | SimpleType::Char => "movzx",
        SimpleType::Int | SimpleType::Other(_) => "mov"
    }
}
pub fn load_array_ty(ty: &Type) -> &'static str {
    match *ty {
        Type::ArrayType(ref simple_ty) => load_simple_ty(simple_ty),
        _ => panic!("non-array type")
    }
}
pub fn load_ty(ty: &Type) -> &'static str {
    match *ty {
        Type::SimpleType(ref simple_ty) => load_simple_ty(simple_ty),
        Type::ArrayType(_) => "mov",
        Type::Void | Type::Null | Type::Unknown => panic!("non-concrete type")
    }
}
// Return the sub-register of `eax` of size `size`.
pub fn eax_lo(size: u32) -> &'static str {
    match size {
        4 => "eax",
        2 => "ax",
        1 => "al",
        _ => panic!("bad size {}", size),
    }
}

pub fn desc(ty: &SimpleType) -> String {
    use middle::middle::SimpleType::*;

    match *ty {
        Boolean => format!("BOOLEANDESC"),
        Int => format!("INTDESC"),
        Short => format!("SHORTDESC"),
        Char => format!("CHARDESC"),
        Byte => format!("BYTEDESC"),
        Other(ref tydef) => format!("DESC{}", tydef.mangle()),
    }
}

pub fn emit_expression<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 stack: &Stack,
                                 expr: &TypedExpression<'a, 'ast>) {
    use middle::middle::TypedExpression_::*;
    match expr.node {
        Constant(ref val) => emit!("mov eax, {}", ConstantValue(ctx, val)),
        Null => emit!("xor eax, eax"), // eax = 0
        This => emit!("mov eax, [ebp+4*{}]", stack.this_index()),
        NewStaticClass(tydef, ref constructor, ref args) => {
            emit!("" ; "Begin allocate {}", tydef.fq_name);

            emit!("push dword 0" ; "reserve space to store `this`");

            // Generate argument code.
            for arg in args.iter() {
                emit_expression(ctx, stack, arg);
                emit!("push eax");
            }

            emit!("call ALLOC{}", tydef.mangle());

            emit!("mov [esp+4*{}], eax", args.len() ; "store `this` into reserved space");
            emit!("call {}", constructor.mangle());
            emit!("pop eax" ; "recover `this`");

            emit!("" ; "End allocate {}", tydef.fq_name);
        }
        NewArray(ref ty, box ref expr) => {
            emit!(""; "Begin allocate array of type {}[]", ty);
            emit_expression(ctx, stack, expr);

            emit!("push eax" ; "save the length of the register");

            emit!("lea eax, [{}*eax + ARRAYLAYOUT.elements]", sizeof_simple_ty(ty));

            emit!("call __malloc");

            emit!("mov dword [eax+VPTR], ARRAYDESC");
            emit!("mov dword [eax+ARRAYLAYOUT.tydesc], {}", desc(ty));

            emit!("pop ebx");
            emit!("mov [eax+ARRAYLAYOUT.len], ebx" ; "store length of array");

            emit!(""; "End allocate array of type {}[]", ty);
        }
        Variable(var) => emit!("mov eax, [ebp+4*{}]", stack.var_index(var.fq_name)
                               ; "variable {}", var.fq_name),
        StaticFieldAccess(field) => {
            emit!("{} eax, {} [{}]",
                  load_ty(&field.ty),
                  size_name(sizeof_ty(&field.ty)), field.mangle());
        }
        FieldAccess(box ref expr, field) => {
            emit_expression(ctx, stack, expr);
            check_null();

            emit!("{} eax, {} [eax+{}]",
                  load_ty(&field.ty),
                  size_name(sizeof_ty(&field.ty)), field.mangle()
                  ; "access field {}", field.fq_name);
        }
        ThisFieldAccess(field) => {
            emit!("mov eax, [ebp+4*{}]", stack.this_index() ; "this");

            emit!("{} eax, {} [eax+{}]",
                  load_ty(&field.ty),
                  size_name(sizeof_ty(&field.ty)), field.mangle()
                  ; "access field {}", field.fq_name);
        }
        Assignment(box expr!(Variable(var)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            emit!("mov [ebp+4*{}], eax", stack.var_index(var.fq_name));
        }
        Assignment(box expr!(StaticFieldAccess(field)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            let field_size = sizeof_ty(&field.ty);
            emit!("mov {} [{}], {}",
                  size_name(field_size), field.mangle(),
                  eax_lo(field_size));
        }
        Assignment(box expr!(FieldAccess(box ref expr, field)), box ref rhs) => {
            emit_expression(ctx, stack, expr);
            // conceptually, the field reference is evaluated here
            // (in reality, we do it later)
            check_null(); // null check before evaluating RHS
            emit!("push eax");
            emit_expression(ctx, stack, rhs);
            emit!("pop ebx");

            let field_size = sizeof_ty(&field.ty);
            emit!("mov {} [ebx + {}], {}",
                  size_name(field_size), field.mangle(),
                  eax_lo(field_size)
                  ; "set field {}", field.fq_name);
        }
        Assignment(box expr!(ThisFieldAccess(field)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            emit!("mov ebx, [ebp+4*{}]", stack.this_index() ; "emit");

            let field_size = sizeof_ty(&field.ty);
            emit!("mov {} [ebx + {}], {}",
                  size_name(field_size), field.mangle(),
                  eax_lo(field_size)
                  ; "set field {}", field.fq_name);
        }
        Assignment(box expr!(ArrayAccess(box ref array_expr, box ref index_expr)), box ref rhs) => {
            // NOTE: This is perhaps a bit surprisng. The JLS specifies special handling
            // for assignment of arrays. In particular, the RHS must be evaluated before
            // the null check and out of bounds access check.

            emit_expression(ctx, stack, array_expr);
            emit!("push eax");
            emit_expression(ctx, stack, index_expr);
            emit!("push eax");
            emit_expression(ctx, stack, rhs);
            emit!("pop edi"); // array index
            emit!("pop ecx"); // array location

            emit!("test ecx, ecx" ; "check null");
            emit!("jz __exception");
            emit!("cmp edi, [ecx+ARRAYLAYOUT.len]" ; "check for array out of bounds");
            // UNSIGNED compare (if eax is negative, then it will also fail)
            emit!("jae __exception");

            // check type compatibility
            match array_expr.ty {
                Type::ArrayType(SimpleType::Other(_)) => {
                    // save a reference to the object
                    emit!("test eax, eax");
                    let skip = ctx.label();
                    emit!("jz .L{}", skip ; "null is ok");
                    emit!("mov edx, eax");
                    emit!("mov ebx, [ecx+ARRAYLAYOUT.tydesc]" ; "get array's runtime type");
                    emit!("call __instanceof");
                    emit!("test eax, eax");
                    emit!("jz __exception");
                    emit!("mov eax, edx");
                    emit!(".L{}:", skip);
                }
                Type::ArrayType(_) => {
                    // primitive type: no compatibility check required
                }
                _ => panic!("type of array is not array type"),
            }

            let size = sizeof_array_element(&array_expr.ty);
            emit!("mov [ecx + ARRAYLAYOUT.elements + {} * edi], {}",
                  size,
                  eax_lo(size));
        }
        Assignment(..) => panic!("non-lvalue in assignment"),
        ArrayLength(box ref expr) => {
            emit_expression(ctx, stack, expr);
            check_null();
            emit!("mov eax, [eax+ARRAYLAYOUT.len]");
        }
        MethodInvocation(ref receiver, ref sig, method, ref args) => {
            if method.is_static {
                assert!(receiver.is_none());
            } else {
               if let Some(box ref expr) = *receiver {
                    emit_expression(ctx, stack, expr);
                    check_null();
                } else {
                    // implicitly `this`
                    emit!("mov eax, [ebp+4*{}]", stack.this_index());
                }
                emit!("push eax");
            }
            for arg in args.iter() {
                emit_expression(ctx, stack, arg);
                emit!("push eax");
            }
            if method.is_static {
                // No dynamic dispatch: just call the impl.
                if let Concrete(method_impl) = method.impled {
                    emit!("call {}", method_impl.mangle());
                } else {
                    panic!("no impl for static method");
                }
            } else {
                // Grab the reference to the receiver...
                // (`args.len()` slots up the stack)
                emit!("mov eax, [esp+4*{}]", args.len());
                // Look up the type descriptor (first slot).
                emit!("mov eax, [eax+VPTR]");
                // Now call the method.
                // Skip three slots, then look up by method index
                emit!("call [eax+TYDESC.methods+4*{}]", ctx.method_index(sig)
                      ; "method {}", sig);
            }
            // Callee pops the stack, nothing to do here.
        }
        ArrayAccess(box ref array, box ref ix) => {
            emit_expression(ctx, stack, array);
            emit!("push eax");
            emit_expression(ctx, stack, ix);
            emit!("pop ebx");
            emit!("test ebx, ebx" ; "check null");
            emit!("jz __exception");

            // array (not null) in `ebx`
            // check index in bounds?
            emit!("cmp eax, [ebx+ARRAYLAYOUT.len]");
            // UNSIGNED compare (if eax is negative, then it will also fail)
            emit!("jae __exception");
            // index OK, look up element
            let size = sizeof_array_element(&array.ty);
            emit!("{} eax, {} [ebx+ARRAYLAYOUT.elements+{}*eax]",
                  load_array_ty(&array.ty),
                  size_name(size), size);
        }
        Prefix(op, box ref expr) => {
            use ast::PrefixOperator::*;
            emit_expression(ctx, stack, expr);
            match op {
                Not => {
                    // always a boolean
                    emit!("xor eax, 1");
                }
                Minus => {
                    emit!("neg eax");
                }
            }
        }
        Infix(op, box ref l, box ref r) => {
            use ast::InfixOperator::*;
            match op {
                LazyOr | LazyAnd => {
                    emit_expression(ctx, stack, l);
                    emit!("test eax, eax");
                    let skip = ctx.label();
                    match op {
                        LazyOr => emit!("jnz .L{}", skip),
                        LazyAnd => emit!("jz .L{}", skip),
                        _ => unreachable!(),
                    }
                    emit_expression(ctx, stack, r);
                    emit!(".L{}:", skip);
                }
                _ => {
                    emit_expression(ctx, stack, l);
                    emit!("push eax");
                    emit_expression(ctx, stack, r);
                    emit!("pop ebx");
                    match op {
                        LazyOr | LazyAnd => unreachable!(),
                        Xor => emit!("xor eax, ebx"),
                        EagerOr => emit!("or eax, ebx"),
                        EagerAnd => emit!("and eax, ebx"),
                        Equals | NotEquals
                        | LessThan | GreaterThan
                        | LessEqual | GreaterEqual => {
                            emit!("cmp ebx, eax");
                            emit!("set{} al", match op {
                                // Equality is also fine for pointers
                                Equals => "e",
                                NotEquals => "ne",
                                // Numeric comparisons only happen for numbers.
                                // Use signed comparison.
                                LessThan => "l",
                                GreaterThan => "g",
                                LessEqual => "le",
                                GreaterEqual => "ge",
                                _ => unreachable!(),
                            });
                            emit!("movzx eax, al");
                        }
                        // These operations are commutative.
                        Plus => emit!("add eax, ebx"),
                        Mult => emit!("imul ebx"),
                        // These are not. `eax` and `ebx` are in the wrong order
                        Minus | Div | Modulo => {
                            emit!("xchg eax, ebx");
                            match op {
                                Minus => emit!("sub eax, ebx"),
                                Div | Modulo => {
                                    emit!("test ebx, ebx");
                                    emit!("jz __exception" ; "division by zero");
                                    // Special case: (-2^31) / (-1) produces a division error,
                                    // but should instead return (-2^31).
                                    // Meanwhile, (-2^31) % (-1) should return 0.
                                    let skip = ctx.label();
                                    emit!("lea ecx, [2*eax]" ; "ecx = 0 iff eax = -2^31 or 0");
                                    emit!("lea edx, [ebx+1]" ; "edx = 0 iff ebx = -1");
                                    emit!("or ecx, edx" ; "ecx = 0 iff both the above hold");
                                    emit!("jz .L{}", skip ; "in this case, skip the division");
                                    // If the division is skipped, then -eax = eax, while ebx = -1.
                                    // Hence `eax` is the correct result of eax / ebx, while edx =
                                    // 0 is the correct result of eax % ebx.
                                    // Otherwise, do the division properly.
                                    emit!("cdq"); // clear out edx
                                    emit!("idiv ebx");
                                    emit!(".L{}:", skip);
                                    if let Modulo = op {
                                        // remainder in edx
                                        emit!("mov eax, edx");
                                    } // otherwise, quotient in eax
                                }
                                _ => unreachable!(),
                            }
                        }
                    }
                }
            }
        }
        Concat(box ref expr1, box ref expr2) => {
            emit!("" ; "> begin string concat operation");
            emit_expression(ctx, stack, expr1);
            // null -> "null"
            let null = ctx.string_constants["null"];
            emit!("test eax, eax");
            emit!("mov ebx, stringstruct#{}", null);
            emit!("cmovz eax, ebx");
            emit!("push eax");

            emit_expression(ctx, stack, expr2);
            emit!("test eax, eax");
            emit!("mov ebx, stringstruct#{}", null);
            emit!("cmovz eax, ebx");
            emit!("push eax");

            // TODO: Fragile if we change naming scheme. Consider revising.
            emit!("call METHODjava.lang.String.concat#java.lang.String");

            emit!("" ; "> end string concat operation");
        }
        InstanceOf(box ref expr, ref ty) => {
            emit_expression(ctx, stack, expr);
            match *ty {
                Type::SimpleType(ref ty @ SimpleType::Other(&TypeDefinition {
                                     kind: TypeKind::Interface, ..
                                 })) => {
                    emit!("mov ebx, {}", desc(ty));
                    emit!("call __instanceof_interface");
                }
                Type::SimpleType(ref ty) => {
                    emit!("mov ebx, {}", desc(ty));
                    emit!("call __instanceof");
                }
                Type::ArrayType(ref ty @ SimpleType::Other(&TypeDefinition {
                                     kind: TypeKind::Interface, ..
                                 })) => {
                    emit!("mov ebx, {}", desc(ty));
                    emit!("call __instanceof_array_interface");
                }
                Type::ArrayType(ref ty) => {
                    emit!("mov ebx, {}", desc(ty));
                    emit!("call __instanceof_array");
                }
                _ => panic!("bad type in instanceof")
            }
        }
        RefDowncast(box ref inner_expr) => {
            emit_expression(ctx, stack, inner_expr);
            emit!("" ; "check reference downcast");
            let end = ctx.label();
            emit!("test eax, eax");
            emit!("jz .L{}", end ; "null is always fine");
            match expr.ty {
                Type::SimpleType(SimpleType::Other(tydef)) => {
                    // object must be a subtype of `tydef`
                    emit!("push eax");
                    emit!("mov ebx, {}", desc(&SimpleType::Other(tydef)));
                    match tydef.kind {
                        TypeKind::Class => emit!("call __instanceof"),
                        TypeKind::Interface => emit!("call __instanceof_interface"),
                    }
                    emit!("test eax, eax");
                    emit!("jz __exception");
                    emit!("pop eax");
                }
                Type::ArrayType(SimpleType::Other(tydef)) => {
                    emit!("cmp [eax+VPTR], dword ARRAYDESC" ; "check array");
                    emit!("jne __exception");
                    emit!("push eax");
                    emit!("mov eax, [eax+ARRAYLAYOUT.tydesc]");
                    emit!("mov ebx, {}", desc(&SimpleType::Other(tydef)));
                    // array element type must be a subtype of `tydef`
                    match tydef.kind {
                        TypeKind::Class => emit!("call __instanceof_tydesc"),
                        TypeKind::Interface => emit!("call __instanceof_tydesc_interface"),
                    }
                    emit!("test eax, eax");
                    emit!("jz __exception");
                    emit!("pop eax");
                    // TODO: Handle arrays of interface types :(
                    // (Need to make interface descriptors recognizable,
                    // generalize __instanceof)
                }
                Type::ArrayType(ref elem_ty) => {
                    // Cast to a primitive array type.
                    emit!("cmp [eax+VPTR], dword ARRAYDESC" ; "check array");
                    emit!("jne __exception");
                    emit!("mov ebx, [eax+ARRAYLAYOUT.tydesc]");
                    emit!("cmp ebx, {}", desc(elem_ty) ; "primitive array type: check exact match");
                    emit!("jne __exception");
                }
                _ => panic!("bad RefDowncast to type {}", expr.ty),
            }
            emit!(".L{}:", end ; "cast OK");
        }
        PrimDowncast(box ref inner_expr) => {
            emit_expression(ctx, stack, inner_expr);
            match expr.ty {
                Type::SimpleType(SimpleType::Byte) => emit!("movsx eax, al" ; "cast to byte"),
                Type::SimpleType(SimpleType::Char) => emit!("movzx eax, ax" ; "cast to char"),
                Type::SimpleType(SimpleType::Short) => emit!("movsx eax, ax" ; "cast to short"),
                Type::SimpleType(SimpleType::Int) => emit!("" ; "(cast to int)"),
                _ => panic!("bad PrimDowncast to type {}", expr.ty),
            }
        }
        Widen(box ref expr) => {
            emit_expression(ctx, stack, expr);
            // no operation: reference types all have the same representation,
            // while primitive types are already extended to 32 bits
        }
        ToString(box ref expr) => {
            use middle::middle::SimpleType::*;

            let tostring_signature = MethodSignature {
                name: Symbol::from_str("toString"),
                args: vec![],
            };

            emit!(""; "Begin conversion to string");

            match expr.ty {
                // reference type
                Type::ArrayType(_) | Type::SimpleType(Other(_)) => {
                    emit_expression(ctx, stack, expr);

                    // eax contains reference type
                    emit!("test eax, eax" ; "check null");

                    let not_null_label = ctx.label();
                    let end_label = ctx.label();
                    emit!("jnz .L{}", not_null_label
                          ; "use string \"null\" if reference type is null");
                    emit!("mov eax, {}",
                          ConstantValue(ctx, &Value::String("null".to_owned())));
                    emit!("jmp .L{}", end_label);

                    emit!(".L{}:", not_null_label);

                    emit!("push eax");
                    // Look up the type descriptor (first slot).
                    emit!("mov eax, [eax+VPTR]");
                    // Now call the method.
                    emit!("call [eax+TYDESC.methods+4*{}]", ctx.method_index(&tostring_signature)
                          ; "call toString");

                    emit!(".L{}:", end_label);
                }
                // primitive type
                Type::SimpleType(ref ty) => {
                    emit!("" ; " create reference type for primitive conversion to string");
                    emit!("push dword 0" ; "reserve space to store `this`");

                    emit_expression(ctx, stack, expr);
                    emit!("push eax");

                    let (boxed_type, use_type) = match *ty {
                        Boolean => (ctx.lang_items.boolean, Boolean),
                        Byte | Short | Int => (ctx.lang_items.integer, Int),
                        Char => (ctx.lang_items.character, Char),
                        Other(..) => panic!("case should have been previously covered"),
                    };
                    let arg_type = Type::SimpleType(use_type);
                    let constructor = boxed_type.constructors.get(&vec![arg_type]).unwrap();

                    emit!("call ALLOC{}", boxed_type.mangle());
                    emit!("mov [esp+4], eax" ; "store `this` into reserved space");
                    emit!("call {}", constructor.mangle());
                    emit!("pop eax" ; "recover `this`");
                    check_null();

                    emit!("push eax");
                    // Look up the type descriptor (first slot).
                    emit!("mov eax, [eax+VPTR]");
                    // Now call the method.
                    emit!("call [eax+TYDESC.methods+4*{}]", ctx.method_index(&tostring_signature)
                          ; "call toString");
                }
                Type::Null => {
                    emit!("mov eax, {}",
                          ConstantValue(ctx, &Value::String("null".to_owned())));
                }
                Type::Void | Type:: Unknown =>
                    panic!("should not be able to print Void or Unknown"),
            }

            emit!(""; "End conversion to string");
        }
    }
}
