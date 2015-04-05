use middle::middle::*;
use mangle::Mangle;

use context::Context;
use stack::Stack;
use code::{emit_block, emit_expression, sizeof_ty, size_name, eax_lo};

fn emit_allocator<'a, 'ast>(ctx: &Context<'a, 'ast>,
                            tydef: TypeDefinitionRef<'a, 'ast>) {
    emit!("section .text" ; "begin allocator");
    emit!("ALLOC{}:", tydef.mangle());

    // Prologue
    emit!("push ebp");
    emit!("mov ebp, esp");

    emit!("mov eax, LAYOUT{}#_size", tydef.mangle());
    emit!("call __malloc");

    // Set pointer to type descriptor.
    emit!("mov dword [eax], DESC{}", tydef.mangle());
    // The rest of the fields are zero.

    // Leave the result in eax.

    emit!("mov esp, ebp");
    emit!("pop ebp");

    // No arguments to pop off the stack.
    emit!("ret");

    emit!("; end class allocator\n");
}

fn emit_constructor<'a, 'ast>(ctx: &Context<'a, 'ast>,
                              tydef: TypeDefinitionRef<'a, 'ast>,
                              super_constructor: Option<ConstructorRef<'a, 'ast>>,
                              constructor: ConstructorRef<'a, 'ast>) {
    emit!("section .text" ; "begin constructor");
    emit!("{}:", constructor.mangle());

    // Prologue
    emit!("push ebp");
    emit!("mov ebp, esp");

    let mut stack = Stack::new(&**constructor.args, false /* not static */);
    // FIXME: hack: leave `this` on the stack
    stack.args -= 1;

    if let Some(ctor) = super_constructor {
        emit!("push dword [ebp+4*{}]", stack.this_index() ; "push `this`");
        emit!("call {}", ctor.mangle() ; "call super-constructor");
        emit!("add esp, 4"); // hack: ctor does not remove `this`
    }

    emit!("; field initializers");
    for field in tydef.nonstatic_fields().iter() {
        if field.origin == tydef {
            let field_size = sizeof_ty(&field.ty);
            if let Some(ref initializer) = *field.initializer {
                emit!("" ; "emit field initializer {}", field.fq_name);
                emit_expression(ctx, &stack, initializer);

                emit!("mov ebx, [ebp+4*{}]", stack.this_index() ; "get `this`");
                emit!("mov {} [ebx+{}], {}", size_name(field_size), field.mangle(), eax_lo(field_size));
            }
        }
    }

    emit_block(ctx, &stack, &*constructor.body);

    emit!("mov esp, ebp");
    emit!("pop ebp");
    emit!("ret 4*{}", stack.args ; "pop {} args off stack after return", stack.args);
    emit!("; end class constructor\n");
}

pub fn emit_class_allocator<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                      tydef: TypeDefinitionRef<'a, 'ast>) {
    emit_allocator(ctx, tydef);
    let empty: &[Type] = &[];
    let super_constructor = match &**tydef.extends {
        [] if tydef == ctx.lang_items.object => None,
        [] => Some(ctx.lang_items.object),
        [tydef] => Some(tydef),
        _ => panic!("class has multiple parents"),
    }.map(|parent| *parent.constructors.get(empty).expect("missing parent constructor with no arguments"));
    for (_, constructor) in tydef.constructors.iter() {
        emit_constructor(ctx, tydef, super_constructor, *constructor);
    }
}
