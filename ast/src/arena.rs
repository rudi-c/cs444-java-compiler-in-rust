extern crate arena;

use name_resolve_structs::*;
use self::arena::TypedArena;

macro_rules! make_arena {
    ($($name: ident: $ty: ident),+) => {
        pub struct Arena<'a, 'ast: 'a> {
            $($name: TypedArena<$ty<'a, 'ast>>,)+
        }
        pub trait Alloc<'a, 'ast> {
            fn alloc(self, arena: &'a Arena<'a, 'ast>) -> &'a Self;
        }
        $(impl<'a, 'ast> Alloc<'a, 'ast> for $ty<'a, 'ast> {
            fn alloc(self, arena: &'a Arena<'a, 'ast>) -> &'a Self {
                arena.$name.alloc(self)
            }
        })+
        impl<'a, 'ast> Arena<'a, 'ast> {
            pub fn alloc<T: Alloc<'a, 'ast>>(&'a self, t: T) -> &'a T {
                t.alloc(self)
            }
            pub fn new() -> Self {
                Arena {
                    $($name: TypedArena::new(),)+
                }
            }
        }
    };
}

make_arena! {
    package: Package,
    field: Field,
    method: Method,
    tydef: TypeDefinition
}
