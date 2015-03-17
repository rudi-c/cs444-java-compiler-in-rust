use middle::*;
use ast;

pub fn eval_const_bool<'a, 'ast>(expr: &TypedExpression<'a, 'ast>) -> Option<bool> {
    use middle::TypedExpression_::*;
    if !matches!(Type::SimpleType(SimpleType::Boolean), *expr.ty()) {
        return None;
    }
    match expr.0 {
        Literal(lit) => {
            match *lit {
                ast::Literal::Boolean(v) => Some(v),
                _ => panic!("non-boolean literal given a boolean type"),
            }
        }
        Prefix(op, box ref inner) => {
            use ast::PrefixOperator::*;
            eval_const_bool(inner).map(|v| {
                match op {
                    Not => !v,
                    _ => panic!("invalid boolean operator"),
                }
            })
        }
        Infix(op, box ref left, box ref right) => {
            use ast::InfixOperator::*;
            eval_const_bool(left).and_then(|l| {
                eval_const_bool(right).and_then(|r| {
                    match op {
                        Xor => Some(l ^ r),
                        EagerOr | LazyOr => Some(l || r),
                        EagerAnd | LazyAnd => Some(l && r),
                        // FIXME: Handle more constant expressions
                        _ => None,
                    }
                })
            })
        }
        _ => None
    }
}
