use middle::middle::*;
use mangle::Mangle;
use context::Context;
use stack::Stack;

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
    println!("push eax ; variable {}", var.variable.fq_name);
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
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            println!("test eax, eax");
            // If it is (i.e. false), jump to `iff`.
            let false_label = ctx.label();
            println!("jz L{}", false_label);
            // Otherwise, execute `ift`...
            emit_statement(ctx, stack, ift);
            if let Some(box ref iff) = *iff {
                // then jump over `iff`.
                let end_label = ctx.label();
                println!("jmp L{}", end_label);
                println!("L{}:", false_label);
                emit_statement(ctx, stack, iff);
                println!("L{}:", end_label);
            } else {
                // and we're done.
                println!("L{}:", false_label);
            }
        }
        While(ref expr, box ref inner) => {
            let top_label = ctx.label();
            println!("L{}:", top_label);
            emit_expression(ctx, stack, expr);
            // Is the result zero?
            println!("test eax, eax");
            // If it is (i.e. false), jump to the end.
            let end_label = ctx.label();
            println!("jz L{}", end_label);
            // Otherwise, run the body...
            emit_statement(ctx, stack, inner);
            // and go back to the top.
            println!("jmp L{}", top_label);
            println!("L{}:", end_label);
        }
        For(ref init, ref test, ref update, box ref inner) => {
            if let Some(ref init) = *init {
                emit_expression(ctx, stack, init);
            }
            let top_label = ctx.label();
            let end_label = ctx.label();
            println!("L{}:", top_label);
            if let Some(ref test) = *test {
                emit_expression(ctx, stack, test);
                println!("test eax, eax");
                println!("jz L{}", end_label);
            }
            emit_statement(ctx, stack, inner);
            if let Some(ref update) = *update {
                emit_expression(ctx, stack, update);
            }
            println!("jmp L{}", top_label);
            println!("L{}:", end_label);
        }
        ForDecl(ref var, ref test, ref update, box ref inner) => {
            stack.scope(|stack| {
                emit_variable(ctx, stack, var);
                let top_label = ctx.label();
                let end_label = ctx.label();
                println!("L{}:", top_label);
                if let Some(ref test) = *test {
                    emit_expression(ctx, stack, test);
                    println!("test eax, eax");
                    println!("jz L{}", end_label);
                }
                emit_statement(ctx, stack, inner);
                if let Some(ref update) = *update {
                    emit_expression(ctx, stack, update);
                }
                println!("jmp L{}", top_label);
                println!("L{}:", end_label);
            });
        }
        Empty => (),
        Return(ref expr) => {
            if let Some(ref expr) = *expr {
                emit_expression(ctx, stack, expr);
            }
            // Result is already in `eax`, if any.
            // Just need to unwind the stack.
            println!("mov esp, ebp");
            println!("pop ebp");
            // pop arguments off the stack after return
            println!("ret {}", 4 * stack.args);
        }
        Block(ref block) => emit_block(ctx, stack, block),
    }
}

// Ensure that `eax` is not null.
pub fn check_null() {
    println!("test eax, eax");
    println!("jz __exception");
}

pub fn emit_expression<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 stack: &Stack,
                                 expr: &TypedExpression<'a, 'ast>) {
    use middle::middle::TypedExpression_::*;
    match expr.node {
        Constant(ref val) => match *val {
            Value::Int(v) => println!("mov eax, {}", v),
            Value::Short(v) => println!("mov eax, {}", v),
            Value::Char(v) => println!("mov eax, {}", v),
            Value::Byte(v) => println!("mov eax, {}", v),
            Value::Bool(v) => println!("mov eax, {}", if v { 1 } else { 0 }),
            Value::String(ref v) => println!("; TODO: string literal \"{}\"", v),
        },
        Null => println!("xor eax, eax"), // eax = 0
        This => println!("mov eax, [ebp+{}]", stack.this_index() * 4),
        Variable(var) => println!("mov eax, [ebp+{}]", stack.var_index(var.fq_name) * 4),
        StaticFieldAccess(field) => println!("mov eax, [{}]", field.mangle()),
        // FieldAccess(box ref expr, field)
        // ThisFieldAccess(field),
        Assignment(box expr!(Variable(var)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            println!("mov [ebp+{}], eax", stack.var_index(var.fq_name) * 4);
        }
        Assignment(box expr!(StaticFieldAccess(field)), box ref rhs) => {
            emit_expression(ctx, stack, rhs);
            println!("mov [{}], eax", field.mangle());
        }
        // Assignment(box expr!(FieldAccess(box ref expr, field)), box ref rhs) =>
        // Assignment(box expr!(ThisFieldAccess(field)), box ref rhs) =>
        // Assignment(..) => panic!("non-lvalue in assignment"),
        ArrayLength(box ref expr) => {
            emit_expression(ctx, stack, expr);
            check_null();
            // length is the third field
            println!("mov eax, [eax+8]");
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
                    println!("mov eax, [ebp+{}]", stack.this_index() * 4);
                }
                println!("push eax");
            }
            for arg in args.iter() {
                emit_expression(ctx, stack, arg);
                println!("push eax");
            }
            if method.is_static {
                // No dynamic dispatch: just call the impl.
                if let Concrete(method_impl) = method.impled {
                    println!("call {}", method_impl.mangle());
                } else {
                    panic!("no impl for static method");
                }
            } else {
                // Grab the reference to the receiver...
                // (`args.len()` slots up the stack)
                println!("mov eax, [esp+{}]", 4 * args.len());
                // Look up the type descriptor (first slot).
                println!("mov eax, [eax]");
                // Now call the method.
                // Skip three slots, then look up by method index
                println!("call [eax+12+{}]", 4 * ctx.method_index(sig));
            }
            // Callee pops the stack, nothing to do here.
        }
        ArrayAccess(box ref array, box ref ix) => {
            emit_expression(ctx, stack, array);
            check_null();
            println!("push eax");
            emit_expression(ctx, stack, ix);
            println!("pop ebx");

            // array (not null) in `ebx`
            // check index in bounds?
            // length of array is [ebx+8]
            println!("cmp eax, [ebx+8]");
            // UNSIGNED compare (if eax is negative, then it will also fail)
            println!("jae __exception");
            // index OK, look up element
            println!("mov eax, [ebx+12+4*eax]");
        }
        _ => println!("; TODO: expression goes here"),
    }
}
