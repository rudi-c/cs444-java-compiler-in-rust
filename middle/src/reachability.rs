use middle::*;
use typed_walker::*;

struct Reachability;

impl<'a, 'ast> Walker<'a, 'ast> for Reachability {
    fn walk_method_impl(&mut self, method: MethodImplRef<'a, 'ast>) {
        if let Some(ref body) = *method.body {
            let out_maybe = check_block(body);
            if out_maybe && !matches!(Type::Void, method.ret_ty) {
                span_error!(body.span, "missing return statement");
            }
        }
    }

    fn walk_constructor(&mut self, constructor: ConstructorRef<'a, 'ast>) {
        check_block(&*constructor.body);
    }
}

impl<'a, 'ast> StatementWalker<'a, 'ast> for Reachability {
}

impl<'a, 'ast> ExpressionWalker<'a, 'ast> for Reachability {
}

fn check_block<'a, 'ast>(block: &TypedBlock<'a, 'ast>) -> bool {
    let mut out_maybe = true;

    for stmt in block.node.stmts.iter() {
        out_maybe = check_block_statement(out_maybe, stmt);
    }
    out_maybe
}

fn check_block_statement<'a, 'ast>(in_maybe: bool, stmt: &TypedBlockStatement<'a, 'ast>) -> bool {
    use middle::TypedBlockStatement_::*;

    if !in_maybe {
        span_error!(stmt.span, "unreachable statement");
        return false
    }

    match stmt.node {
        LocalVariable(_) => true,
        Statement(ref stmt) => check_statement(in_maybe, stmt),
    }
}

fn check_statement<'a, 'ast>(in_maybe: bool, stmt: &TypedStatement<'a, 'ast>) -> bool {
    use middle::TypedStatement_::*;

    if !in_maybe {
        span_error!(stmt.span, "unreachable statement");
        return false
    }

    match stmt.node {
        If(_, box ref tstmt, None) => {
            check_statement(in_maybe, tstmt);
            in_maybe
        }
        If(_, box ref tstmt, Some(box ref fstmt)) => {
            check_statement(in_maybe, tstmt) ||
            check_statement(in_maybe, fstmt)
        }
        For(_, None, _, box ref stmt) |
        ForDecl(_, None, _, box ref stmt) => {
            check_statement(in_maybe, stmt); false
        }
        For(_, Some(ref test), _, box ref stmt) |
        ForDecl(_, Some(ref test), _, box ref stmt) |
        While(ref test, box ref stmt) => {
            match test.node {
                TypedExpression_::Constant(Value::Bool(true)) => { check_statement(in_maybe, stmt); false }
                TypedExpression_::Constant(Value::Bool(false)) => { check_statement(false, stmt); in_maybe }
                _ => { check_statement(in_maybe, stmt); in_maybe }
            }
        }
        Return(_) => false,
        Block(ref block) => check_block(block),
        Expression(_) | Empty => in_maybe,
    }
}

pub fn check_reachability<'a, 'ast>(universe: &Universe<'a, 'ast>) {
    Reachability.walk_universe(universe);
}

