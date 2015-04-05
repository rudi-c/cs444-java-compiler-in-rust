use middle::middle::*;
use mangle::Mangle;

use ast::name::Symbol;
use code::{emit_expression, sizeof_ty, size_name, short_size_name, eax_lo};
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
        emit!("" ; "emit field initializer {}", field.fq_name);
        emit_expression(ctx, stack, initializer);
        emit!("mov {} [{}], {}",
              size_name(field_size), target,
              eax_lo(field_size));
    }
}

fn emit_entry_point<'a, 'ast>(ctx: &Context<'a, 'ast>,
                              universe: &Universe<'a, 'ast>,
                              method: MethodImplRef<'a, 'ast>) {
    emit!("section .text" ; "begin entry point");
    emit!("global _start");
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
    emit!("mov ebx, eax" ; "exit code in eax");
    emit!("mov eax, 1" ; "sys_exit");
    emit!("int 80h" ; "syscall");
    emit!("");
}

fn emit_type<'a, 'ast>(ctx: &Context<'a, 'ast>,
                       tydef: TypeDefinitionRef<'a, 'ast>) {
    // Emit type descriptors.
    emit_descriptor(ctx, tydef);
    if tydef.kind == TypeKind::Class {
        emit_class_allocator(ctx, tydef);
    }

    // Emit slots for static fields.
    emit!("section .data" ; "begin static field slots");
    for field in tydef.static_fields().iter() {
        let field_size = sizeof_ty(&field.ty);
        emit!("{}: d{} 0", field.mangle(), short_size_name(field_size));
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

    emit!("; vim: ft=nasm");
    emit!("extern __exception");
    emit!("extern __malloc");
    emit!("");

    emit!(r"section .text
__true:
mov eax, 1
ret

__false:
mov eax, 0
ret

; expression in `eax`, type descriptor in `ebx`
; returns a bool in `eax`
__instanceof:
test eax, eax
jz __false ; null is never instanceof anything
mov eax, [eax+VPTR] ; look up type descriptor
; fall into __instanceof_tydesc...

; tydesc in `eax` and `ebx`.
; is `eax` a subtype of `ebx`?
__instanceof_tydesc:
test ebx, 04h ; is it actually an interface descriptor?
jnz __instanceof_tydesc_interface
.loop:
test eax, eax
jz __false
cmp eax, ebx
je __true
mov eax, [eax+TYDESC.parent] ; parent tydesc
jmp .loop

; array expression in `eax`, type descriptor in `ebx`
__instanceof_array:
test eax, eax
jz __false ; null is never instanceof anything
cmp dword [eax+VPTR], ARRAYDESC ; check array type descriptor
jne __false
mov eax, [eax+ARRAYLAYOUT.tydesc] ; look up element type descriptor
jmp __instanceof_tydesc

; array expression in `eax`, interface type descriptor in `ebx`
__instanceof_array_interface:
test eax, eax
jz __false ; null is never instanceof anything
cmp dword [eax+VPTR], ARRAYDESC ; check array type descriptor
jne __false
mov eax, [eax+ARRAYLAYOUT.tydesc] ; look up element type descriptor
jmp __instanceof_tydesc_interface

; object in `eax`, interface descriptor in `ebx`
__instanceof_interface:
test eax, eax
jz __false ; null is never instanceof anything
mov eax, [eax+VPTR] ; look up type descriptor
; fall into __instanceof_tydesc_interface

__instanceof_tydesc_interface:
mov eax, [eax+TYDESC.intfs] ; look up interface list
test eax, eax ; list might be null (in the case of a primitive type)
jz __false
.loop:
cmp [eax], dword 0
jz __false
cmp [eax], ebx
je __true
add eax, 4
jmp .loop
");

    emit_primitive_descriptors(&emit_ctx);

    universe.each_type(|tydef| {
        emit_type(&emit_ctx, tydef);
    });

    emit_entry_point(&emit_ctx, universe, entry_fn);

    output_string_constants(&emit_ctx.string_constants);
}
