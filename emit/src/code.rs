#![macro_use]

use middle::middle::*;
use mangle::Mangle;
use context::Context;
use stack::Stack;
use std::fmt;

macro_rules! emit {
    ( $($instr: expr),+ ; $($comment: expr),+ ) => (
        println!("{:<40} ; {}", format!($($instr),+), format!($($comment),+))
    );
    ( $($instr: expr),+ ) => (
        println!($($instr),+)
    );
}

pub fn emit_block<'a, 'ast>(ctx: &Context<'a, 'ast>,
                            entry: bool,
                            stack: &Stack,
                            block: &TypedBlock<'a, 'ast>) {
    stack.scope(|stack| {
        use middle::middle::TypedBlockStatement_::*;
        for stmt in block.stmts.iter() {
            match stmt.node {
                LocalVariable(ref var) => emit_variable(ctx, stack, var),
                Statement(ref stmt) => emit_statement(ctx, entry, stack, stmt),
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
                                entry: bool,
                                stack: &Stack,
                                stmt: &TypedStatement<'a, 'ast>) {
    use middle::middle::TypedStatement_::*;
    match stmt.node {
        Expression(ref expr) => {
            emit_expression(ctx, stack, expr);
        }
        If(ref expr, box ref ift, ref iff) => {
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            emit!("test eax, eax");
            // If it is (i.e. false), jump to `iff`.
            let false_label = ctx.label();
            emit!("jz L{}", false_label);
            // Otherwise, execute `ift`...
            emit_statement(ctx, entry, stack, ift);
            if let Some(box ref iff) = *iff {
                // then jump over `iff`.
                let end_label = ctx.label();
                emit!("jmp L{}", end_label);
                emit!("L{}:", false_label);
                emit_statement(ctx, entry, stack, iff);
                emit!("L{}:", end_label);
            } else {
                // and we're done.
                emit!("L{}:", false_label);
            }
        }
        While(ref expr, box ref inner) => {
            let top_label = ctx.label();
            emit!("L{}:", top_label);
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            emit!("test eax, eax");
            // If it is (i.e. false), jump to the end.
            let end_label = ctx.label();
            emit!("jz L{}", end_label);
            // Otherwise, run the body...
            emit_statement(ctx, entry, stack, inner);
            // and go back to the top.
            emit!("jmp L{}", top_label);
            emit!("L{}:", end_label);
        }
        For(ref init, ref test, ref update, box ref inner) => {
            if let Some(ref init) = *init {
                emit_expression(ctx, stack, init);
            }
            let top_label = ctx.label();
            let end_label = ctx.label();
            emit!("L{}:", top_label);
            if let Some(ref test) = *test {
                emit_expression(ctx, stack, test);
                emit!("test eax, eax");
                emit!("jz L{}", end_label);
            }
            emit_statement(ctx, entry, stack, inner);
            if let Some(ref update) = *update {
                emit_expression(ctx, stack, update);
            }
            emit!("jmp L{}", top_label);
            emit!("L{}:", end_label);
        }
        ForDecl(ref var, ref test, ref update, box ref inner) => {
            stack.scope(|stack| {
                emit_variable(ctx, stack, var);
                let top_label = ctx.label();
                let end_label = ctx.label();
                emit!("L{}:", top_label);
                if let Some(ref test) = *test {
                    emit_expression(ctx, stack, test);
                    emit!("test eax, eax");
                    emit!("jz L{}", end_label);
                }
                emit_statement(ctx, entry, stack, inner);
                if let Some(ref update) = *update {
                    emit_expression(ctx, stack, update);
                }
                emit!("jmp L{}", top_label);
                emit!("L{}:", end_label);
            });
        }
        Empty => (),
        Return(ref expr) => {
            if let Some(ref expr) = *expr {
                emit_expression(ctx, stack, expr);
            }

            // Result is already in `eax`, if any.
            if entry {
                emit!("mov ebx, eax");
                emit!("mov eax, 1");
                emit!("int 0x80" ; "sys_exit");
            } else {
                // Just need to unwind the stack.
                emit!("mov esp, ebp");
                emit!("pop ebp");
                // pop arguments off the stack after return
                emit!("ret {}", 4 * stack.args);
            }
        }
        Block(ref block) => emit_block(ctx, entry, stack, block),
    }
}

// Ensure that `eax` is not null.
pub fn check_null() {
    emit!("test eax, eax" ; "check null");
    emit!("jz __exception" ; "null exception");
}

pub struct ConstantValue<'a: 'c + 'v, 'ast: 'a, 'c, 'v>(pub &'c Context<'a, 'ast>, pub &'v Value);

impl<'a, 'ast, 'c, 'v> fmt::String for ConstantValue<'a, 'ast, 'c, 'v> {
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

pub fn sizeof_simple_ty<'a, 'ast>(ty: &SimpleType<'a, 'ast>) -> u32 {
    match *ty {
        SimpleType::Char | SimpleType::Short => 2,
        SimpleType::Byte | SimpleType::Boolean => 1,
        _ => 4,
    }
}
pub fn sizeof_ty<'a, 'ast>(ty: &Type<'a, 'ast>) -> u32 {
    match *ty {
        Type::SimpleType(ref simple_ty) => sizeof_simple_ty(simple_ty),
        _ => 4,
    }
}
pub fn sizeof_array_element<'a, 'ast>(ty: &Type<'a, 'ast>) -> u32 {
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
// Return the appropriate `mov` instruction to load
// from a location of size `size`, to a 32-bit register.
pub fn mov_extend(size: u32) -> &'static str {
    match size {
        4 => "mov",
        2 | 1 => "movsx",
        _ => panic!("bad size {}", size),
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

pub fn emit_expression<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 stack: &Stack,
                                 expr: &TypedExpression<'a, 'ast>) {
    use middle::middle::TypedExpression_::*;
    match expr.node {
        Constant(ref val) => emit!("mov eax, {}", ConstantValue(ctx, val)),
        Null => emit!("xor eax, eax"), // eax = 0
        This => emit!("mov eax, [ebp+{}]", stack.this_index() * 4),
        NewStaticClass(tydef, ref constructor, ref args) => {
            emit!("" ; "Begin allocate {}", tydef.fq_name);

            emit!("push dword 0" ; "reserve space to store `this`");

            // Generate argument code.
            for arg in args.iter() {
                emit_expression(ctx, stack, arg);
                emit!("push eax");
            }

            emit!("call ALLOC{}", tydef.mangle());

            emit!("mov [esp+{}], eax", args.len() * 4 ; "store `this` into reserved space");

            emit!("call {}", constructor.mangle());

            emit!("" ; "End allocate {}", tydef.fq_name);
        }
        NewArray(ref ty, box ref expr) => {
            use middle::middle::SimpleType::*;

            emit!(""; "Begin allocate array of type {}[]", ty);
            emit_expression(ctx, stack, expr);

            emit!("push eax" ; "save the length of the register");

            emit!("lea eax, [{}*eax + 12]", sizeof_simple_ty(ty));

            emit!("call __malloc");

            emit!("mov dword [eax], ARRAYDESC");

            match *ty {
                Boolean => emit!("mov dword [eax+4], BOOLEANDESC"),
                Int => emit!("mov dword [eax+4], INTDESC"),
                Short => emit!("mov dword [eax+4], SHORTDESC"),
                Char => emit!("mov dword [eax+4], CHARDESC"),
                Byte => emit!("mov dword [eax+4], BYTEDESC"),
                Other(ref tydef) => emit!("mov [eax+4], DESC{}", tydef.mangle()),
            }

            emit!("pop ebx");
            emit!("mov [eax+8], ebx" ; "store length of array");

            emit!(""; "End allocate array of type {}[]", ty);
        }
        Variable(var) => emit!("mov eax, [ebp+{}]", stack.var_index(var.fq_name) * 4
                               ; "variable {}", var.fq_name),
        StaticFieldAccess(field) => emit!("mov eax, dword [{}]", field.mangle()),
        FieldAccess(box ref expr, field) => {
            emit_expression(ctx, stack, expr);
            check_null();

            let offset = ctx.field_offsets.get(&field).unwrap();
            let field_size = sizeof_ty(&field.ty);
            emit!("{} eax, {} [eax+{}]",
                  mov_extend(field_size),
                  size_name(field_size), offset
                  ; "access field {}", field.fq_name);
        }
        ThisFieldAccess(field) => {
            emit!("mov eax, [ebp+{}]", stack.this_index() * 4 ; "this");

            let offset = ctx.field_offsets.get(&field).unwrap();
            let field_size = sizeof_ty(&field.ty);
            emit!("{} eax, {} [eax+{}]",
                  mov_extend(field_size),
                  size_name(field_size), offset
                  ; "access field {}", field.fq_name);
        }
        Assignment(box expr!(Variable(var)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            emit!("mov [ebp+{}], eax", stack.var_index(var.fq_name) * 4);
        }
        Assignment(box expr!(StaticFieldAccess(field)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            emit!("mov [{}], eax", field.mangle());
        }
        Assignment(box expr!(FieldAccess(box ref expr, field)), box ref rhs) => {
            emit_expression(ctx, stack, expr);
            emit!("push eax");
            emit_expression(ctx, stack, rhs);
            emit!("pop ebx");

            let offset = ctx.field_offsets.get(&field).unwrap();
            let field_size = sizeof_ty(&field.ty);
            emit!("mov {} [ebx + {}], {}",
                  size_name(field_size), offset,
                  eax_lo(field_size)
                  ; "set field {}", field.fq_name);
        }
        Assignment(box expr!(ThisFieldAccess(field)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            emit!("mov ebx, [ebp+{}]", stack.this_index() * 4 ; "emit");

            let offset = ctx.field_offsets.get(&field).unwrap();
            let field_size = sizeof_ty(&field.ty);
            emit!("mov {} [ebx + {}], {}",
                  size_name(field_size), offset,
                  eax_lo(field_size)
                  ; "set field {}", field.fq_name);
        }
        Assignment(box expr!(ArrayAccess(box ref array_expr, box ref index_expr)), box ref rhs) => {
            emit_expression(ctx, stack, array_expr);
            check_null();
            emit!("push eax");
            emit_expression(ctx, stack, index_expr);

            emit!("mov ebx, [esp]");
            // array (not null) in `ebx`
            // check index in bounds?
            // length of array is [ebx+8]
            emit!("cmp eax, [ebx+8]" ; "check for array out of bounds");
            // UNSIGNED compare (if eax is negative, then it will also fail)
            emit!("jae __exception");

            emit!("push eax");
            emit_expression(ctx, stack, rhs);

            emit!("pop ebx"); // array index
            emit!("pop ecx"); // array location
            let size = sizeof_array_element(&array_expr.ty);
            emit!("mov [ecx + 12 + {} * ebx], {}",
                  size,
                  eax_lo(size));
        }
        Assignment(..) => panic!("non-lvalue in assignment"),
        ArrayLength(box ref expr) => {
            emit_expression(ctx, stack, expr);
            check_null();
            // length is the third field
            emit!("mov eax, [eax+8]");
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
                    emit!("mov eax, [ebp+{}]", stack.this_index() * 4);
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
                emit!("mov eax, [esp+{}]", 4 * args.len());
                // Look up the type descriptor (first slot).
                emit!("mov eax, [eax]");
                // Now call the method.
                // Skip three slots, then look up by method index
                emit!("call [eax+12+{}]", 4 * ctx.method_index(sig));
            }
            // Callee pops the stack, nothing to do here.
        }
        ArrayAccess(box ref array, box ref ix) => {
            emit_expression(ctx, stack, array);
            check_null();
            emit!("push eax");
            emit_expression(ctx, stack, ix);
            emit!("pop ebx");

            // array (not null) in `ebx`
            // check index in bounds?
            // length of array is [ebx+8]
            emit!("cmp eax, [ebx+8]");
            // UNSIGNED compare (if eax is negative, then it will also fail)
            emit!("jae __exception");
            // index OK, look up element
            let size = sizeof_array_element(&array.ty);
            emit!("{} eax, {} [ebx+12+{}*eax]",
                  mov_extend(size),
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
                        LazyOr => emit!("jnz L{}", skip),
                        LazyAnd => emit!("jz L{}", skip),
                        _ => unreachable!(),
                    }
                    emit_expression(ctx, stack, r);
                    emit!("L{}:", skip);
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
                                    emit!("cdq"); // clear out edx
                                    emit!("idiv ebx");
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
            emit!("test eax, eax");
            emit!("mov ebx, stringstruct#{}", ctx.string_constants[*"null"]);
            emit!("cmovz eax, ebx");
            emit!("push eax");

            emit_expression(ctx, stack, expr2);
            emit!("test eax, eax");
            emit!("mov ebx, stringstruct#{}", ctx.string_constants[*"null"]);
            emit!("cmovz eax, ebx");
            emit!("push eax");

            // TODO: Fragile if we change naming scheme. Consider revising.
            emit!("call METHODjava.lang.String.concat#java.lang.String");

            emit!("" ; "> end string concat operation");
        }
        _ => emit!("; TODO: expression goes here"),
    }
}
