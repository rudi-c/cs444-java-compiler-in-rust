use middle::middle::*;
use mangle::Mangle;

use ast::name::Symbol;
use code::{emit_expression, ConstantValue, sizeof_ty, size_name, eax_lo};
use context::Context;
use descriptors::{emit_descriptor, emit_primitive_descriptors};
use method::emit_method;
use ref_alloc::emit_class_allocator;
use stack::Stack;
use strings::output_string_constants;

fn emit_static_field_initializer<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                    stack: &Stack,
                                    field: FieldRef<'a, 'ast>) {
    assert!(field.is_static());
    if let Some(ref initializer) = *field.initializer {
        let target = field.mangle();
        let field_size = sizeof_ty(&field.ty);
        if let TypedExpression_::Constant(..) = initializer.node {
            emit!("" ; "initializer omitted for {}", target);
        } else {
            emit!("" ; "emit field initializer {}", field.fq_name);
            emit_expression(ctx, stack, initializer);
            emit!("mov {} [{}], {}",
                  size_name(field_size), target,
                  eax_lo(field_size));
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
            emit_static_field_initializer(ctx, &stack, *field);
        }
    });
    emit!("; end emit static field initializers\n");

    emit!("call {}", method.mangle() ; "begin program");
    emit!("jmp __debexit" ; "exit code in eax");
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
        if let Some(expr!(TypedExpression_::Constant(ref initializer))) = *field.initializer {
            emit!("dd {}", ConstantValue(ctx, initializer));
        } else {
            emit!("dd 0" ; "non-constant");
        }
    }
    emit!("; end static field slots\n");

    for method in tydef.method_impls.iter() {
        emit_method(ctx, *method);
    }
}

pub fn emit(universe: &Universe) {
    let entry_fn = if let Some(&method) = universe.main.methods.get(&MethodSignature {
        name: Symbol::from_str("test"),
        args: vec![],
    }) {
        let span = if let Some(ast) = method.ast {
            ast.span
        } else {
            universe.main.ast.span
        };
        if !method.is_static {
            span_fatal!(span, "method `test()` must be static");
        }
        if let Type::SimpleType(SimpleType::Int) = method.ret_ty {
        } else {
            span_fatal!(span, "method `test()` must return `int`");
        }
        if let Concrete(method_impl) = method.impled {
            method_impl
        } else {
            panic!("abstract static method?")
        }
    } else {
        span_fatal!(universe.main.ast.span, "missing `static int test()`");
    };
    let emit_ctx = Context::create(universe);

    emit!("global _start");
    emit!("extern __exception");
    emit!("extern __malloc");
    emit!("extern __debexit");
    emit!("extern NATIVEjava.io.OutputStream.nativeWrite");

    emit_primitive_descriptors(&emit_ctx);

    universe.each_type(|tydef| {
        emit_type(&emit_ctx, universe, tydef);
    });

    emit_entry_point(&emit_ctx, universe, entry_fn);

    output_string_constants(&emit_ctx.string_constants);
}
