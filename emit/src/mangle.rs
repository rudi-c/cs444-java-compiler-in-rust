use middle::middle::*;

use std::fmt;

pub struct Mangled<T>(pub T);

pub trait Mangle {
    fn mangle(&self) -> Mangled<&Self> {
        Mangled(self)
    }
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error>;
}

impl<'a, T: Mangle> fmt::Display for Mangled<&'a T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        self.0.mangle_into(f)
    }
}

impl<'a, 'ast> Mangle for TypeDefinition<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}", self.fq_name)
    }
}

impl<'a, 'ast> Mangle for SimpleType<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use middle::middle::SimpleType::*;
        match *self {
            Boolean => write!(f, "boolean"),
            Int => write!(f, "int"),
            Short => write!(f, "short"),
            Char => write!(f, "char"),
            Byte => write!(f, "byte"),
            Other(tydef) => tydef.mangle_into(f),
        }
    }
}

impl<'a, 'ast> Mangle for Type<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        use middle::middle::Type::*;
        match *self {
            SimpleType(ref ty) => ty.mangle_into(f),
            ArrayType(ref ty) => write!(f, "a@{}", ty.mangle()),

            _ => panic!("tried to mangle a bad type {}", *self),
        }
    }
}

impl<'a, 'ast> Mangle for Field<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "FIELD{}", self.fq_name)
    }
}

impl<'a, 'ast> Mangle for MethodImpl<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if self.is_native {
            write!(f, "NATIVE{}", self.fq_name)
        } else {
            try!(write!(f, "METHOD"));
            try!(write!(f, "{}", self.fq_name));
            for ty in self.arg_types.iter() {
                try!(write!(f, "#{}", ty.mangle()));
            }
            Ok(())
        }
    }
}

impl<'a, 'ast> Mangle for Constructor<'a, 'ast> {
    fn mangle_into(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        try!(write!(f, "NEW"));
        try!(write!(f, "{}", self.origin.fq_name));
        for ty in self.arg_types.iter() {
            try!(write!(f, "#{}", ty.mangle()));
        }
        Ok(())
    }
}
