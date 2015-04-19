use middle::middle::*;
use mangle::Mangle;
use context::Context;
use code::{sizeof_ty, short_size_name};

use std::collections::{HashSet, VecDeque};

fn emit_methods<'a, 'ast>(ctx: &Context<'a, 'ast>,
                          tydef: TypeDefinitionRef<'a, 'ast>) {
    for sig in ctx.all_methods.iter() {
        if let Some(decl) = tydef.methods.get(*sig) {
            if let Concrete(method_impl) = decl.impled {
                emit!("dd {}", method_impl.mangle() ; "{}", sig);
            } else {
                emit!("dd 0" ; "{}", sig);
            }
        } else {
            emit!("dd 0" ; "{}", sig);
        }
    }
}

fn emit_interface_list(tydef: TypeDefinitionRef) {
    let mut set = HashSet::new();
    let mut deque = VecDeque::new();
    set.insert(tydef.fq_name);
    deque.push_back(tydef);
    while let Some(next) = deque.pop_front() {
        if next.kind == TypeKind::Interface {
            emit!("dd DESC{}", next.mangle());
        }
        for intf in next.implements.iter().chain(next.extends.iter()) {
            if set.insert(intf.fq_name) {
                deque.push_back(*intf);
            }
        }
    }
    emit!("dd 0");
}

pub fn emit_descriptor<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 tydef: TypeDefinitionRef<'a, 'ast>) {
    emit!("section .rodata" ; "begin descriptor");
    emit!("align 8,db 0");
    match tydef.kind {
        TypeKind::Interface => {
            emit!("; interface");
            emit!("dd 0" ; "align interface descriptors to 4 bytes instead of 8");
            emit!("DESC{}:", tydef.mangle());
            emit!("istruc TYDESC");
            emit!("at TYDESC.name, dd .?name");
            // Interface types can only be cast to `Object`, or other interface types.
            emit!("at TYDESC.parent, dd DESC{}", ctx.lang_items.object.mangle());
            emit!("at TYDESC.intfs, dd .?intfs");
            emit!("iend");
            emit!(".?name: db '{}', 0", tydef.fq_name);
            emit!("align 4,db 0");
            emit!(".?intfs:");
            emit_interface_list(tydef);
        }
        TypeKind::Class => {
            emit!("; class");
            emit!("DESC{}:", tydef.mangle());
            emit!("istruc TYDESC");
            emit!("at TYDESC.name, dd .?name");
            let superclass = if tydef.fq_name == ctx.lang_items.object.fq_name {
                emit!("at TYDESC.parent, dd 0" ; "Object: no superclass");
                None
            } else {
                let superclass = match &**tydef.extends {
                    [] => ctx.lang_items.object,
                    [parent] => parent,
                    _ => panic!("class extends multiple types?")
                };
                emit!("at TYDESC.parent, dd DESC{}", superclass.mangle() ; "superclass");
                Some(superclass)
            };
            emit!("at TYDESC.intfs, dd .?intfs");
            emit!("at TYDESC.methods");
            emit_methods(ctx, tydef);
            emit!("iend");
            emit!(".?name:");
            emit!("db '{}', 0", tydef.fq_name);
            emit!("align 4,db 0");
            emit!(".?intfs:");
            emit_interface_list(tydef);
            emit!("");
            emit!("struc LAYOUT{}#", tydef.mangle());
            if let Some(superclass) = superclass {
                emit!("resb LAYOUT{}#_size", superclass.mangle() ; "parent");
            } else {
                emit!("VPTR: resd 1");
            }
            for field in tydef.nonstatic_fields().iter() {
                if field.origin == tydef {
                    let size = sizeof_ty(&field.ty);
                    emit!("alignb {}", size);
                    emit!("{}: res{} 1", field.mangle(), short_size_name(size));
                }
            }
            emit!("endstruc");
        }
    }
    emit!("; end type descriptor\n");
}

pub fn emit_primitive_descriptors<'a, 'ast>(ctx: &Context<'a, 'ast>) {
    emit!("struc TYDESC" ; "type descriptor layout");
    emit!(".name: resd 1");
    emit!(".parent: resd 1");
    emit!(".intfs: resd 1");
    emit!(".methods: resd {}", ctx.all_methods.len());
    emit!("endstruc\n");
    emit!("section .rodata\n");

    emit!("struc ARRAYLAYOUT");
    emit!("resb LAYOUT{}#_size", ctx.lang_items.object.mangle() ; "inherits from Object");
    emit!(".tydesc: resd 1");
    emit!(".len: resd 1");
    emit!(".elements:");
    emit!("endstruc\n");

    emit!("align 8,db 0");
    emit!("ARRAYDESC:");
    emit!("istruc TYDESC");
    emit!("at TYDESC.name, dd .?name");
    emit!("at TYDESC.parent, dd DESC{}", ctx.lang_items.object.mangle());
    emit!("at TYDESC.intfs, dd .?intfs");
    emit!("at TYDESC.methods");
    // arrays just have Object's methods
    emit_methods(ctx, ctx.lang_items.object);
    emit!("iend");
    emit!(".?name: db 'array', 0");
    emit!("align 4,db 0");
    emit!(".?intfs:");
    emit!("dd DESC{}", ctx.lang_items.cloneable.mangle());
    emit!("dd DESC{}", ctx.lang_items.serializable.mangle());
    emit!("dd 0");

    macro_rules! prim {
        ($name: ident) => ({
            emit!("align 8,db 0");
            emit!("{}DESC:", stringify!($name));
            emit!("istruc TYDESC");
            emit!("at TYDESC.name, dd .?name");
            emit!("at TYDESC.parent, dd 0");
            emit!("at TYDESC.intfs, dd 0");
            emit!("iend");
            emit!(".?name: db '{}', 0", stringify!($name));
        });
    }
    prim!(BOOLEAN);
    prim!(INT);
    prim!(SHORT);
    prim!(CHAR);
    prim!(BYTE);
    emit!("; end primitive descriptors\n");
}
