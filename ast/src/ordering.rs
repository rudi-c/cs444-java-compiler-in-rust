use name::*;
use middle::*;
use typed_walker::*;

use std::collections::HashSet;

// Check that field initializers do not use the values of later fields.

struct Order {
    seen: HashSet<Name>,
}

impl<'a, 'ast> Walker<'a, 'ast> for Order {
    fn walk_type_definition(&mut self, tydef: TypeDefinitionRef<'a, 'ast>) {
        for &name in tydef.ordered_fields.borrow().iter() {
            self.walk_field(name, *tydef.fields.borrow().get(&name).unwrap());
        }
    }

    fn walk_field(&mut self, _name: Symbol, field: FieldRef<'a, 'ast>) {
        if !field.is_static() {
            if let Some(ref expr) = *field.initializer.borrow() {
                self.walk_expression(expr);
            }
            self.seen.insert(field.fq_name);
        }
    }

    fn walk_expression(&mut self, expr: &TypedExpression<'a, 'ast>) {
        use middle::TypedExpression_::*;
        match expr.0 {
            ThisFieldAccess(field) => {
                if !self.seen.contains(&field.fq_name) {
                    span_error!(expr.span,
                                "reference to field `{}` in an initializer before it is declared",
                                field.fq_name);
                }
            }

            // Special case: LHS of an assignment is ok.
            Assignment(box node!((ThisFieldAccess(..), _)), box ref rhs) => {
                self.walk_expression(rhs);
                // Don't walk normally!
                return;
            }

            _ => (),
        }

        default_walk_expression(self, expr);
    }
}

pub fn check_ordering<'a, 'ast>(package: PackageRef<'a, 'ast>) {
    Order { seen: HashSet::new() }.walk_package(package);
}
