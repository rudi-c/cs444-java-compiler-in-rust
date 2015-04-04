use middle::middle::*;
use mangle::Mangle;
use context::Context;

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

pub fn emit_descriptor<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 tydef: TypeDefinitionRef<'a, 'ast>) {
    emit!("section .rodata" ; "begin descriptor");
    emit!("align 4,db 0");
    emit!("DESC{}:", tydef.mangle());
    match tydef.kind {
        TypeKind::Interface => {
            emit!("; interface");
            emit!("db '{}', 0", tydef.fq_name);
        }
        TypeKind::Class => {
            emit!("; class");
            emit!("dd .?name");
            if tydef.fq_name == ctx.lang_items.object.fq_name {
                emit!("dd 0" ; "Object: no superclass");
            } else {
                let superclass = match &**tydef.extends {
                    [] => ctx.lang_items.object,
                    [parent] => parent,
                    _ => panic!("class extends multiple types?")
                };
                emit!("dd DESC{}", superclass.mangle() ; "superclass");
            }
            emit!("dd .?intfs");
            emit_methods(ctx, tydef);
            emit!(".?name:");
            emit!("db '{}', 0", tydef.fq_name);
            emit!("align 4,db 0");
            emit!(".?intfs:");
            for intf in tydef.implements.iter() {
                emit!("dd DESC{}", intf.mangle());
            }
            emit!("dd 0");
        }
    }
    emit!("; end type descriptor\n");
}

pub fn emit_primitive_descriptors<'a, 'ast>(ctx: &Context<'a, 'ast>) {
    emit!("section .rodata\n");

    emit!("align 4,db 0");
    emit!("ARRAYDESC:");
    emit!("dd .?name");
    emit!("dd DESC{}", ctx.lang_items.object.mangle());
    emit!("dd .?intfs");
    emit!(".?name: db 'array', 0");
    emit!("align 4,db 0");
    emit!(".?intfs:");
    emit!("dd DESC{}", ctx.lang_items.cloneable.mangle());
    emit!("dd DESC{}", ctx.lang_items.serializable.mangle());
    emit!("dd 0");
    // arrays just have Object's methods
    emit_methods(ctx, ctx.lang_items.object);

    macro_rules! prim {
        ($name: ident) => ({
            emit!("align 4,db 0");
            emit!("{}DESC:", stringify!($name));
            emit!("dd .?name");
            emit!("dd 0");
            emit!("dd 0");
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
