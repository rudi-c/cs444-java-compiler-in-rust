use middle::middle::*;

use std::fmt::Writer;

pub trait Mangle {
    fn mangle(&self) -> String;
}

impl<'a, 'ast> Mangle for TypeDefinition<'a, 'ast> {
    fn mangle(&self) -> String {
        format!("{}", self.fq_name)
    }
}

impl<'a, 'ast> Mangle for SimpleType<'a, 'ast> {
    fn mangle(&self) -> String {
        use middle::middle::SimpleType::*;
        match *self {
            Boolean => format!("bool"),
            Int => format!("int"),
            Short => format!("short"),
            Char => format!("char"),
            Byte => format!("byte"),
            Other(tydef) => tydef.mangle(),
        }
    }
}

impl<'a, 'ast> Mangle for Type<'a, 'ast> {
    fn mangle(&self) -> String {
        use middle::middle::Type::*;
        match *self {
            SimpleType(ref ty) => ty.mangle(),
            ArrayType(ref ty) => format!("a@{}", ty.mangle()),

            _ => panic!("tried to mangle a bad type {}", *self),
        }
    }
}

impl<'a, 'ast> Mangle for Field<'a, 'ast> {
    fn mangle(&self) -> String {
        assert!(self.is_static());
        format!("FIELD{}", self.fq_name)
    }
}

impl<'a, 'ast> Mangle for MethodImpl<'a, 'ast> {
    fn mangle(&self) -> String {
        if self.is_native {
            format!("NATIVE{}", self.fq_name)
        } else {
            let mut r = format!("METHOD");
            write!(&mut r, "{}", self.fq_name).unwrap();
            for ty in self.arg_types.iter() {
                write!(&mut r, "#{}", ty.mangle()).unwrap();
            }
            r
        }
    }
}

impl<'a, 'ast> Mangle for Constructor<'a, 'ast> {
    fn mangle(&self) -> String {
        let mut r = format!("NEW");
        write!(&mut r, "{}", self.origin.fq_name).unwrap();
        for ty in self.arg_types.iter() {
            write!(&mut r, "#{}", ty.mangle()).unwrap();
        }
        r
    }
}
