use middle::middle::*;
use mangle::Mangle;

use code::emit_expression;
use context::Context;
use descriptors::{emit_descriptor, emit_primitive_descriptors};
use method::{emit_method, emit_entry_method};
use ref_alloc::emit_class_allocator;
use stack::Stack;
use strings::output_string_constants;

fn is_entry_method<'a, 'ast>(method: &MethodImplRef<'a, 'ast>) -> bool {
    method.arg_types.len() == 0 &&
    method.ast.name.node.as_slice() == "test" &&
    method.ret_ty == Type::SimpleType(SimpleType::Int) &&
    method.is_static
}

fn emit_field_initializer<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                    stack: &Stack,
                                    field: FieldRef<'a, 'ast>) {
    if let Some(ref initializer) = *field.initializer {
        let target = field.mangle();
        if let TypedExpression_::Constant(ref val) = initializer.node {
            match *val {
                Value::Int(v) => emit!("mov dword [{}], {}", target, v),
                Value::Short(v) => emit!("mov word [{}], {}", target, v),
                Value::Char(v) => emit!("mov word [{}], {}", target, v),
                Value::Byte(v) => emit!("mov byte [{}], {}", target, v),
                Value::Bool(v) => emit!("mov byte [{}], {}", target, if v { 1 } else { 0 }),
                Value::String(ref v) =>
                    emit!("mov dword [{}], stringstruct#{}", target,
                          ctx.string_constants.get(v).unwrap()),
            }
        } else {
            emit!("" ; "emit field initializer {}", field.fq_name);
            emit_expression(ctx, stack, initializer);

            // A pointer to the new object should still be at the top of the stack.
            emit!("mov [{}], eax", target);
        }
    }
}

fn emit_entry_point<'a, 'ast>(ctx: &Context<'a, 'ast>,
                              universe: &Universe<'a, 'ast>,
                              method: MethodImplRef<'a, 'ast>) {
    emit!("; begin entry point");
    emit!("_start:");

    emit!("; emit static field initializers");
    let stack = Stack::new(&[], false);
    universe.each_type(|tydef| {
        for field in tydef.static_fields().iter() {
            emit_field_initializer(ctx, &stack, *field);
        }
    });
    emit!("; end emit static field initializers\n");

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

    // Emit slots for static fields.
    emit!("; begin static field slots");
    for field in tydef.static_fields().iter() {
        emit!("{}:", field.mangle());
        emit!("dd 0");
    }
    emit!("; end static field slots\n");

    if tydef == universe.main {
        let mut found_entry = false;
        for method in tydef.method_impls.iter() {
            if tydef == universe.main && is_entry_method(method) {
                emit_entry_point(ctx, universe, *method);
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
