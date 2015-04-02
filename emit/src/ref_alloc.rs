use middle::middle::*;
use mangle::Mangle;

use context::Context;
use stack::Stack;
use code::emit_expression;

pub fn emit_class_allocator<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                      tydef: TypeDefinitionRef<'a, 'ast>) {
    emit!("ALLOC{}:", tydef.mangle());

    // Prologue
    emit!("push ebp");
    emit!("mov ebp, esp");
    let stack = Stack::new(&[], false);

    emit!("mov eax, {}", ctx.class_sizes.get(&tydef).unwrap());
    emit!("call __malloc");
    emit!("push eax");

    // Set pointer to type descriptor.
    emit!("mov [eax], DESC{}", tydef.mangle());

    emit!("; Field initializers, first pass");
    for field_name in tydef.ordered_fields.iter() {
        let field = tydef.fields.get(field_name).unwrap();
        let offset = ctx.field_offsets.get(field).unwrap();
        if let Some(ref initializer) = *field.initializer {
            if let TypedExpression_::Constant(ref val) = initializer.node {
                match *val {
                    Value::Int(v) => emit!("mov [eax+{}], {}", offset, v),
                    Value::Short(v) => emit!("mov [eax+{}], {}", offset, v),
                    Value::Char(v) => emit!("mov [eax+{}], {}", offset, v),
                    Value::Byte(v) => emit!("mov [eax+{}], {}", offset, v),
                    Value::Bool(v) => emit!("mov [eax+{}], {}", offset, if v { 1 } else { 0 }),
                    Value::String(ref v) => emit!("; TODO: string literal \"{}\"", v),
                }
            }
        } else {
            emit!("mov [eax+{}], 0", offset);
        }
    }

    // Initialize fields (second pass).
    for field_name in tydef.ordered_fields.iter() {
        let field = tydef.fields.get(field_name).unwrap();
        let offset = ctx.field_offsets.get(field).unwrap();
        if let Some(ref initializer) = *field.initializer {
            if let TypedExpression_::Constant(..) = initializer.node {
            } else {
                emit!("" ; "emit field initializer {}", field.fq_name);
                emit_expression(ctx, &stack, initializer);

                // A pointer to the new object should still be at the top of the stack.
                emit!("mov [esp+{}], eax", offset);
            }
        }
    }

    // Leave the result in eax.
    emit!("pop eax");

    emit!("mov esp, ebp");
    emit!("pop ebp");

    // No arguments to pop off the stack.
    emit!("ret");

    emit!("; end class allocator\n");
}
