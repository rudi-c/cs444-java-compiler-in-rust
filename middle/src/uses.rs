use middle::*;
use typed_walker::*;

struct Uses<'a, 'ast: 'a> {
    var: VariableRef<'a, 'ast>,
}

impl<'a, 'ast> ExpressionWalker<'a, 'ast> for Uses<'a, 'ast> {
    fn walk_expression(&mut self, expr: &TypedExpression<'a, 'ast>) {
        use middle::TypedExpression_::*;
        match expr.node {
            Variable(var) if var.fq_name == self.var.fq_name => {
                span_error!(expr.span,
                            "illegal use of variable `{}`",
                            var.fq_name);
            }
            // Special case: LHS of an assignment is ok.
            Assignment(box expr!(Variable(..)), box ref rhs) => {
                self.walk_expression(rhs);
                return;
            }
            _ => (),
        }
        default_walk_expression(self, expr);
    }
}

pub fn check_not_used<'a, 'ast>(expr: &TypedExpression<'a, 'ast>, v: VariableRef<'a, 'ast>) {
    Uses { var: v }.walk_expression(expr);
}
