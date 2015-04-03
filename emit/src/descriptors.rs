use middle::middle::*;
use mangle::Mangle;
use context::Context;

pub fn emit_descriptor<'a, 'ast>(ctx: &Context<'a, 'ast>,
                                 tydef: TypeDefinitionRef<'a, 'ast>) {
    println!("align 4,db 0");
    println!("DESC{}:", tydef.mangle());
    match tydef.kind {
        TypeKind::Interface => {
            println!("; interface");
            println!("db '{}', 0", tydef.fq_name);
        }
        TypeKind::Class => {
            println!("; class");
            println!("dd .?name");
            if tydef.fq_name == ctx.lang_items.object.fq_name {
                println!("dd 0");
            } else {
                let superclass = match &**tydef.extends {
                    [] => ctx.lang_items.object,
                    [parent] => parent,
                    _ => panic!("class extends multiple types?")
                };
                println!("dd DESC{}", superclass.mangle());
            }
            println!("dd .?intfs");
            for sig in ctx.all_methods.iter() {
                if let Some(decl) = tydef.methods.get(*sig) {
                    if let Concrete(method_impl) = decl.impled {
                        print!("dd {}", method_impl.mangle());
                    } else {
                        print!("dd 0");
                    }
                } else {
                    print!("dd 0");
                }
                println!(" ; {}", sig);
            }
            println!(".?name:");
            println!("db '{}', 0", tydef.fq_name);
            println!("align 4,db 0");
            println!(".?intfs:");
            for intf in tydef.implements.iter() {
                println!("dd DESC{}", intf.mangle());
            }
            println!("dd 0");
        }
    }
    println!("; end type descriptor\n");
}

pub fn emit_primitive_descriptors<'a, 'ast>(ctx: &Context<'a, 'ast>) {
    // TODO
    emit!("ARRAYDESC:");
    emit!("BOOLEANDESC:");
    emit!("INTDESC:");
    emit!("SHORTDESC:");
    emit!("CHARDESC:");
    emit!("BYTEDESC:");
}
