use middle::*;
use ast;

#[derive(Show, Clone)]
pub enum Value {
    Int(i32),
    Short(i16),
    Char(i16), // a UTF-16 code unit
    Byte(i8),
    Bool(bool),
    String(String),
}

macro_rules! try_opt {
    ($e: expr) => (match $e { Some(v) => v, None => return None });
}

pub fn eval_const<'a, 'ast>(expr: &TypedExpression<'a, 'ast>) -> Option<Value> {
    use middle::TypedExpression_::*;
    use self::Value::*;
    let r = match expr.0 {
        Literal(lit) => {
            match *lit {
                ast::Literal::Integer(v) => Int(v as i32),
                ast::Literal::Boolean(v) => Bool(v),
                ast::Literal::Character(v) => Char(v as u32 as i16),
                ast::Literal::String(ref v) => String(v.clone()),
                _ => return None,
            }
        }
        Prefix(op, box ref inner) => {
            use ast::PrefixOperator::*;
            match (op, try_opt!(eval_const(inner))) {
                (Not, Bool(v)) => Bool(!v),
                (Not, _) => panic!("bad Not"),
                (Minus, Int(v)) => Int(-v),
                (Minus, Short(_)) |
                (Minus, Byte(_)) => panic!("unpromoted Minus operand"),
                (Minus, _) => panic!("bad Minus"),
            }
        }
        Infix(op, box ref left, box ref right) => {
            use ast::InfixOperator::*;
            match (op, try_opt!(eval_const(left)), try_opt!(eval_const(right))) {
                (Xor, Bool(l), Bool(r)) => Bool(l ^ r),
                (Xor, _, _) => panic!("bad Xor"),
                (EagerOr, Bool(l), Bool(r)) |
                (LazyOr, Bool(l), Bool(r)) => Bool(l || r),
                (EagerOr, _, _) |
                (LazyOr, _, _) => panic!("bad Or"),
                (EagerAnd, Bool(l), Bool(r)) |
                (LazyAnd, Bool(l), Bool(r)) => Bool(l && r),
                (EagerAnd, _, _) |
                (LazyAnd, _, _) => panic!("bad And"),

                (Equals, Bool(l), Bool(r)) => Bool(l == r),
                (Equals, Int(l), Int(r)) => Bool(l == r),
                (Equals, String(l), String(r)) => Bool(l == r),
                (Equals, _, _) => panic!("bad Equals"),
                (NotEquals, Bool(l), Bool(r)) => Bool(l != r),
                (NotEquals, Int(l), Int(r)) => Bool(l != r),
                (NotEquals, String(l), String(r)) => Bool(l != r),
                (NotEquals, _, _) => panic!("bad NotEquals"),

                (LessThan, Int(l), Int(r)) => Bool(l < r),
                (LessThan, _, _) => panic!("bad LessThan"),
                (GreaterThan, Int(l), Int(r)) => Bool(l > r),
                (GreaterThan, _, _) => panic!("bad GreaterThan"),
                (LessEqual, Int(l), Int(r)) => Bool(l <= r),
                (LessEqual, _, _) => panic!("bad LessEqual"),
                (GreaterEqual, Int(l), Int(r)) => Bool(l >= r),
                (GreaterEqual, _, _) => panic!("bad GreaterEqual"),

                (Plus, Int(l), Int(r)) => Int(l + r),
                (Plus, String(l), String(r)) => String(l + &*r),
                (Plus, _, _) => panic!("bad Plus"),
                (Minus, Int(l), Int(r)) => Int(l - r),
                (Minus, _, _) => panic!("bad Minus"),
                (Mult, Int(l), Int(r)) => Int(l * r),
                (Mult, _, _) => panic!("bad Mult"),

                (Div, Int(_), Int(0)) |
                (Modulo, Int(_), Int(0)) => {
                    span_warning!(right.span, "division by zero");
                    return None
                }
                (Div, Int(l), Int(r)) => {
                    // Explicitly use 2's-complement overflow.
                    let v = l as i64 / r as i64;
                    Int(v as i32)
                }
                (Div, _, _) => panic!("bad Div"),
                (Modulo, Int(l), Int(r)) => {
                    let v = l as i64 % r as i64;
                    Int(v as i32)
                }
                (Modulo, _, _) => panic!("bad Modulo"),
            }
        }
        Cast(_, box ref inner) |
        Widen(box ref inner) => {
            let val = try_opt!(eval_const(inner));
            // To avoid combinatorial blowup, always convert numberic types to Int
            let val = match val {
                Int(v) => Int(v),
                Short(v) => Int(v as i32),
                Char(v) => Int(v as i32),
                Byte(v) => Int(v as i32),
                v => v,
            };
            match (expr.ty(), val) {
                (&Type::SimpleType(SimpleType::Int), Int(v)) => Int(v),
                (&Type::SimpleType(SimpleType::Int), _) => panic!("bad cast to int"),
                (&Type::SimpleType(SimpleType::Short), Int(v)) => Short(v as i16),
                (&Type::SimpleType(SimpleType::Short), _) => panic!("bad cast to short"),
                (&Type::SimpleType(SimpleType::Char), Int(v)) => Char(v as i16),
                (&Type::SimpleType(SimpleType::Char), _) => panic!("bad cast to char"),
                (&Type::SimpleType(SimpleType::Byte), Int(v)) => Byte(v as i8),
                (&Type::SimpleType(SimpleType::Byte), _) => panic!("bad cast to byte"),
                (&Type::SimpleType(SimpleType::Boolean), Bool(v)) => Bool(v),
                (&Type::SimpleType(SimpleType::Boolean), _) => panic!("bad cast to bool"),
                (_, _) => return None
            }
        }
        ToString(box ref inner) => {
            String(match try_opt!(eval_const(inner)) {
                Int(v) => format!("{}", v),
                Short(v) => format!("{}", v),
                Char(v) => format!("{}", try_opt!(::std::char::from_u32(v as u32))),
                Byte(v) => format!("{}", v),
                Bool(v) => format!("{}", v),
                String(v) => v,
            })
        }
        _ => return None,
    };
    // sanity check
    match (&r, expr.ty()) {
        (&Int(_), &Type::SimpleType(SimpleType::Int)) |
        (&Short(_), &Type::SimpleType(SimpleType::Short)) |
        (&Char(_), &Type::SimpleType(SimpleType::Char)) |
        (&Byte(_), &Type::SimpleType(SimpleType::Byte)) |
        (&Bool(_), &Type::SimpleType(SimpleType::Boolean)) => (),
        (&String(_), &Type::SimpleType(SimpleType::Other(tydef))) => {
            // hack alert
            assert_eq!(tydef.fq_name.to_string(), "java.lang.String")
        }
        (r, ty) => {
            panic!("sanity check failed: constant expression evaluated to {:?}, but had type {}",
                   r, ty)
        }
    }
    Some(r)
}

pub fn eval_const_bool<'a, 'ast>(expr: &TypedExpression<'a, 'ast>) -> Option<bool> {
    if let Some(Value::Bool(v)) = eval_const(expr) {
        Some(v)
    } else {
        None
    }
}
