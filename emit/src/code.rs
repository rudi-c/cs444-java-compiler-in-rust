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
    println!("push eax");
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
            println!("ret");
        }
        Block(ref block) => emit_block(ctx, stack, block),
    }
}

pub fn emit_expression<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 stack: &Stack,
                                 expr: &TypedExpression<'a, 'ast>) {
    use middle::middle::TypedExpression_::*;
    match expr.node {
        _ => println!("; TODO: expression goes here"),
    }
}
