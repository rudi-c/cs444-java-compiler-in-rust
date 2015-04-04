use middle::middle::*;
use middle::typed_walker::*;

use std::collections::HashMap;

struct StringCollector<'a, 'ast: 'a> {
    // Map of string constants to their associated label.
    strings: HashMap<String, u32>,
    string_count: u32,
}

impl<'a, 'ast> Walker<'a, 'ast> for StringCollector<'a, 'ast> {
}

impl<'a, 'ast> StatementWalker<'a, 'ast> for StringCollector<'a, 'ast> {
}

impl<'a, 'ast> ExpressionWalker<'a, 'ast> for StringCollector<'a, 'ast> {
    fn walk_expression(&mut self, expr: &TypedExpression<'a, 'ast>) {
        match expr.node {
            TypedExpression_::Constant(Value::String(ref string)) => {
                if !self.strings.contains_key(string) {
                    self.strings.insert(string.clone(), self.string_count);
                    self.string_count += 1
                }
            }
            _ => default_walk_expression(self, expr),
        }
    }
}

pub fn output_string_constants(strings: &HashMap<String, u32>) {
    for (string, label) in strings.iter() {
        emit!("stringstruct#{}:", label);
        emit!("dd DESCjava.lang.String");
        emit!("dd .string");
        emit!(".string:");
        emit!("dd ARRAYDESC");
        emit!("dd CHARDESC");
        emit!("dd {}", string.chars().count());
        for c in string.chars() {
            if c.is_alphanumeric() || c == ' ' {
                emit!("dw '{}'", c);
            } else {
                emit!("dw {}", c as u32 as i16);
            }
        }
        emit!("");
    }
}

pub fn collect_constant_strings(universe: &Universe) -> HashMap<String, u32> {
    let mut collector = StringCollector {
        strings: HashMap::new(),
        string_count: 0
    };

    collector.walk_universe(universe);

    collector.strings
}
