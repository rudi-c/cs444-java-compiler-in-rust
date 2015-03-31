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
        let stack = Stack::new(&**method.args);
        emit_block(ctx, &stack, body);
        println!("; end method\n");
    }
}
