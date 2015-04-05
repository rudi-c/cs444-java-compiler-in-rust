use middle::*;
use std::mem::replace;

// Destructively move the String out of a &mut String
fn grab(s: &mut String) -> String {
    replace(s, String::new())
}

macro_rules! constant {
    ($i: pat) => (box TypedExpression { node: TypedExpression_::Constant($i), .. });
    ($i: pat, $s: pat) => (box TypedExpression { node: TypedExpression_::Constant($i), span: $s, .. });
}

fn reduce_toplevel<'a, 'ast>(expr: &mut TypedExpression<'a, 'ast>) {
    use middle::TypedExpression_::*;
    use middle::Value::*;
    let r = match expr.node {
        Prefix(op, constant!(ref inner)) => {
            use ast::PrefixOperator::*;
            match (op, inner) {
                (Not, &Bool(v)) => Bool(!v),
                (Minus, &Int(v)) => Int(-v),
                (Minus, &Short(_)) |
                (Minus, &Byte(_)) => panic!("unpromoted Minus operand"),
                _ => return,
            }
        }
        Infix(op, constant!(ref left), constant!(ref right, ref rspan)) => {
            use ast::InfixOperator::*;
            match (op, left, right) {
                (Xor, &Bool(l), &Bool(r)) => Bool(l ^ r),
                (EagerOr, &Bool(l), &Bool(r)) |
                (LazyOr, &Bool(l), &Bool(r)) => Bool(l || r),
                (EagerAnd, &Bool(l), &Bool(r)) |
                (LazyAnd, &Bool(l), &Bool(r)) => Bool(l && r),

                (Equals, &Bool(l), &Bool(r)) => Bool(l == r),
                (Equals, &Int(l), &Int(r)) => Bool(l == r),
                (Equals, &String(ref l), &String(ref r)) => Bool(l == r),
                (NotEquals, &Bool(l), &Bool(r)) => Bool(l != r),
                (NotEquals, &Int(l), &Int(r)) => Bool(l != r),
                (NotEquals, &String(ref l), &String(ref r)) => Bool(l != r),

                (LessThan, &Int(l), &Int(r)) => Bool(l < r),
                (GreaterThan, &Int(l), &Int(r)) => Bool(l > r),
                (LessEqual, &Int(l), &Int(r)) => Bool(l <= r),
                (GreaterEqual, &Int(l), &Int(r)) => Bool(l >= r),

                (Plus, &Int(l), &Int(r)) => Int(l + r),
                (Minus, &Int(l), &Int(r)) => Int(l - r),
                (Mult, &Int(l), &Int(r)) => Int(l * r),

                (Div, &Int(_), &Int(0)) |
                (Modulo, &Int(_), &Int(0)) => {
                    span_warning!(*rspan, "division by zero");
                    return
                }
                (Div, &Int(l), &Int(r)) => {
                    // Explicitly use 2's-complement overflow.
                    let v = l as i64 / r as i64;
                    Int(v as i32)
                }
                (Modulo, &Int(l), &Int(r)) => {
                    let v = l as i64 % r as i64;
                    Int(v as i32)
                }
                _ => return,
            }
        }
        Concat(constant!(String(ref mut l)), constant!(String(ref mut r))) => {
            String(grab(l) + &**r)
        }
        // Because of a compiler bug (rust#23891), we can't use `constant!` here
        PrimDowncast(box TypedExpression { node: Constant(ref val), .. }) |
        Widen(box TypedExpression { node: Constant(ref val), .. }) => {
            // To avoid combinatorial blowup, always convert numberic types to Int
            let val = match val {
                &Int(v) => Int(v),
                &Short(v) => Int(v as i32),
                &Char(v) => Int(v as i32),
                &Byte(v) => Int(v as i32),
                &Bool(v) => Bool(v),
                _ => return,
            };
            match (&expr.ty, val) {
                (&Type::SimpleType(SimpleType::Int), Int(v)) => Int(v),
                (&Type::SimpleType(SimpleType::Short), Int(v)) => Short(v as i16),
                (&Type::SimpleType(SimpleType::Char), Int(v)) => Char(v as u16),
                (&Type::SimpleType(SimpleType::Byte), Int(v)) => Byte(v as i8),
                (&Type::SimpleType(SimpleType::Boolean), Bool(v)) => Bool(v),
                (_, _) => return
            }
        }
        ToString(constant!(ref mut inner)) => {
            String(match *inner {
                Int(v) => format!("{}", v),
                Short(v) => format!("{}", v),
                Char(v) => format!("{}", match ::std::char::from_u32(v as u32) {
                    Some(v) => v,
                    None => return
                }),
                Byte(v) => format!("{}", v),
                Bool(v) => format!("{}", v),
                String(ref mut v) => grab(v),
            })
        }
        _ => return,
    };
    // sanity check
    match (&r, &expr.ty) {
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
    expr.node = Constant(r);
}

// Replace constants inside the expression in-place.
pub fn reduce_const_expr<'a, 'ast>(expr: &mut TypedExpression<'a, 'ast>) {
    use middle::TypedExpression_::*;
    // First reduce inner expressions.
    match expr.node {
        Constant(_) | This | Null => (),

        MethodInvocation(ref mut expr, _, _, ref mut exprs) => {
            if let Some(box ref mut expr) = *expr {
                reduce_const_expr(expr);
            }
            for expr in exprs.iter_mut() {
                reduce_const_expr(expr);
            }
        },

        NewStaticClass(_, _, ref mut exprs) => {
            for expr in exprs.iter_mut() {
                reduce_const_expr(expr);
            }
        },

        StaticFieldAccess(_) => (),
        ThisFieldAccess(_) => (),

        Variable(_) => (),

        NewArray(_, box ref mut expr)
        | FieldAccess(box ref mut expr, _)
        | Prefix(_, box ref mut expr)
        | RefDowncast(box ref mut expr)
        | PrimDowncast(box ref mut expr)
        | Widen(box ref mut expr)
        | ToString(box ref mut expr)
        | ArrayLength(box ref mut expr) => {
            reduce_const_expr(expr);
        },

        ArrayAccess(box ref mut expr1, box ref mut expr2)
        | Assignment(box ref mut expr1, box ref mut expr2)
        | Infix(_, box ref mut expr1, box ref mut expr2)
        | Concat(box ref mut expr1, box ref mut expr2) => {
            reduce_const_expr(expr1);
            reduce_const_expr(expr2);
        }

        InstanceOf(box ref mut expr, _) => {
            reduce_const_expr(expr);
        },
    }
    // Then reduce the top-level expression.
    reduce_toplevel(expr);
}
