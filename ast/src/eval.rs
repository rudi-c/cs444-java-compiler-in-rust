use middle::*;
use ast;
use ast::InfixOperator::*;
use ast::Prefix::*;

pub fn eval_const_bool<'a, 'ast>(expr: &TypedExpression<'a, 'ast>) -> Option<bool> {
    use middle::TypedExpression_::*;
    if !matches!(Type::SimpleType(SimpleType::Boolean), *expr.ty()) {
        return None;
    }
    match expr.node {
        Literal(lit) => {
            match lit.node {
                ast::Literal_::Boolean(v) => Some(v),
                _ => panic!("non-boolean literal given a boolean type"),
            }
        }
        Prefix(op, box ref inner) => {
            eval_const_bool(inner).map(|v| {
                match op {
                    Not => !v,
                    _ => panic!("invalid boolean operator"),
                }
            })
        }
        Infix(op, box ref left, box ref right) => {
            eval_const_bool(left).and_then(|l| {
                eval_const_bool(right).map(|r| {
                    match op {
                        Xor => l ^ r,
                        EagerOr | LazyOr => l || r,
                        EagerAnd | LazyAnd => l && r,
                        // FIXME: Handle more constant expressions
                        _ => None,
                    }
                })
            })
        }
        _ => None
    }
}
