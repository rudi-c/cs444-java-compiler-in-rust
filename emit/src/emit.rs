use middle::middle::*;

use context::Context;
use descriptors::{emit_descriptor, emit_primitive_descriptors};
use method::{emit_method, emit_entry_method};
use ref_alloc::emit_class_allocator;
use strings::output_string_constants;

fn is_entry_method<'a, 'ast>(method: &MethodImplRef<'a, 'ast>) -> bool {
    method.arg_types.len() == 0 &&
    method.ast.name.node.as_slice() == "test" &&
    method.ret_ty == Type::SimpleType(SimpleType::Int) &&
    method.is_static
}

fn emit_entry_point<'a, 'ast>(ctx: &Context<'a, 'ast>,
                              method: MethodImplRef<'a, 'ast>) {
    emit!("; begin entry point");
    emit!("_start:");
    emit_entry_method(ctx, method);
}

fn emit_type<'a, 'ast>(ctx: &Context<'a, 'ast>,
                       universe: &Universe<'a, 'ast>,
                       tydef: TypeDefinitionRef<'a, 'ast>) {
    // Emit type descriptors.
    emit_descriptor(ctx, tydef);
    if tydef.kind == TypeKind::Class {
        emit_class_allocator(ctx, tydef);
    }

    if tydef == universe.main {
        let mut found_entry = false;
        for method in tydef.method_impls.iter() {
            if tydef == universe.main && is_entry_method(method) {
                emit_entry_point(ctx, *method);
                found_entry = true;
            } else {
                emit_method(ctx, *method);
            }
        }

        if !found_entry {
            // TODO: Do we need this to return an error code?
            panic!("No entry point static int test() found!");
        }
    } else {
        for method in tydef.method_impls.iter() {
            emit_method(ctx, *method);
        }
    }
}

pub fn emit(universe: &Universe) {
    let emit_ctx = Context::create(universe);

    emit!("global _start");
    emit!("extern __exception");
    emit!("extern __malloc");
    emit!("extern NATIVEjava.io.OutputStream.nativeWrite");

    emit_primitive_descriptors(&emit_ctx);

    universe.each_type(|tydef| {
        emit_type(&emit_ctx, universe, tydef);
    });

    output_string_constants(&emit_ctx.string_constants);
}
