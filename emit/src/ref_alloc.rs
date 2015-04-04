use middle::middle::*;
use mangle::Mangle;

use context::Context;
use stack::Stack;
use code::{emit_block, emit_expression, ConstantValue, sizeof_ty, size_name, eax_lo};

fn emit_field_initializers<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                    tydef: TypeDefinitionRef<'a, 'ast>) {
    emit!("section .text" ; "begin allocator");
    emit!("ALLOC{}:", tydef.mangle());

    // Prologue
    emit!("push ebp");
    emit!("mov ebp, esp");
    let stack = Stack::new(&[], false);

    emit!("mov eax, {}", ctx.class_sizes.get(&tydef).unwrap());
    emit!("call __malloc");
    emit!("push eax");

    // Set pointer to type descriptor.
    emit!("mov dword [eax], DESC{}", tydef.mangle());

    emit!("; Field initializers, first pass");
    for field in tydef.nonstatic_fields().iter() {
        let offset = ctx.field_offsets.get(field).unwrap();
        let field_size = sizeof_ty(&field.ty);
        if let Some(ref initializer) = *field.initializer {
            if let TypedExpression_::Constant(ref val) = initializer.node {
                emit!("mov {} [eax+{}], {}",
                      size_name(field_size), offset,
                      ConstantValue(ctx, val));
            } else {
                emit!("mov {} [eax+{}], 0", size_name(field_size), offset ; "non-constant");
            }
        } else {
            emit!("mov {} [eax+{}], 0", size_name(field_size), offset ; "no initializer");
        }
    }

    emit!("; Field initializers, second pass");
    for field in tydef.nonstatic_fields().iter() {
        let offset = ctx.field_offsets.get(field).unwrap();
        let field_size = sizeof_ty(&field.ty);
        if let Some(ref initializer) = *field.initializer {
            if let TypedExpression_::Constant(..) = initializer.node {
            } else {
                emit!("" ; "emit field initializer {}", field.fq_name);
                emit_expression(ctx, &stack, initializer);

                // A pointer to the new object should still be at the top of the stack.
                emit!("mov ebx, [esp]");
                emit!("mov {} [ebx+{}], {}", size_name(field_size), offset, eax_lo(field_size));
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

fn emit_constructor<'a, 'ast>(ctx: &Context<'a, 'ast>,
                              constructor: ConstructorRef<'a, 'ast>) {
    emit!("section .text" ; "begin constructor");
    emit!("{}:", constructor.mangle());

    // Prologue
    emit!("push ebp");
    emit!("mov ebp, esp");

    let stack = Stack::new(&**constructor.args, false);
    emit_block(ctx, &stack, &*constructor.body);

    emit!("mov esp, ebp");
    emit!("pop ebp");
    emit!("ret");
    emit!("; end class constructor\n");
}

pub fn emit_class_allocator<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                      tydef: TypeDefinitionRef<'a, 'ast>) {
    emit_field_initializers(ctx, tydef);
    for (_, constructor) in tydef.constructors.iter() {
        emit_constructor(ctx, *constructor);
    }
}
