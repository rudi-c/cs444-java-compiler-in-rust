use ast::middle::*;
use ast::lang_items::*;

use std::collections::{BTreeSet, HashMap};

pub struct Context<'a, 'ast: 'a> {
    pub lang_items: LangItems<'a, 'ast>,
    pub all_methods: Vec<&'a MethodSignature<'a, 'ast>>,
    pub by_method: HashMap<&'a MethodSignature<'a, 'ast>, usize>,
}

impl<'a, 'ast> Context<'a, 'ast> {
    pub fn create(universe: &Universe<'a, 'ast>) -> Self {
        let mut methods = BTreeSet::new();
        universe.each_type(|tydef| {
            for (sig, method) in tydef.methods.iter() {
                if !method.is_static {
                    methods.insert(sig);
                }
            }
        });
        Context {
            lang_items: find_lang_items(universe.toplevel),
            all_methods: methods.iter().cloned().collect(),
            by_method: methods.into_iter().enumerate().map(|(x, y)| (y, x)).collect(),
        }
    }
}
