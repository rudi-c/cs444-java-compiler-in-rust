use middle::middle::*;
use mangle::Mangle;
use context::Context;
use stack::Stack;
use code::emit_block;

pub fn emit_method<'a, 'ast>(ctx: &Context<'a, 'ast>,
                             method: MethodImplRef<'a, 'ast>) {
    if method.is_native {
        return;
    }

    if let Some(ref body) = *method.body {
        emit!("section .text" ; "begin method");
        emit!("{}:", method.mangle());
        // prologue
        emit!("push ebp");
        emit!("mov ebp, esp");
        let stack = Stack::new(&**method.args, method.is_static);
        emit_block(ctx, false, &stack, body);
        if let Type::Void = method.ret_ty {
            emit!("mov esp, ebp");
            emit!("pop ebp");
        }
        emit!("; end method\n");
    }
}
