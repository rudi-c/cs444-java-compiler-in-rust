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
        println!("{}:", method.mangle());
        // prologue
        println!("push ebp");
        println!("mov ebp, esp");
        let stack = Stack::new(&**method.args, method.is_static);
        emit_block(ctx, false, &stack, body);
        println!("; end method\n");
    }
}

pub fn emit_entry_method<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                   method: MethodImplRef<'a, 'ast>) {
    if let Some(ref body) = *method.body {
        println!("; begin entry method\n");
        let stack = Stack::new(&**method.args, method.is_static);
        emit_block(ctx, true, &stack, body);
        println!("; end entry method\n");
    }
}
