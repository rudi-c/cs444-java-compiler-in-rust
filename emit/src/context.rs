use middle::middle::*;
use middle::lang_items::*;

use std::collections::{BTreeSet, HashMap};
use std::cell::Cell;

pub struct Context<'a, 'ast: 'a> {
    pub lang_items: LangItems<'a, 'ast>,
    pub all_methods: Vec<&'a MethodSignature<'a, 'ast>>,
    pub by_method: HashMap<&'a MethodSignature<'a, 'ast>, u32>,
    pub next_label: Cell<u32>,
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
            by_method: methods.into_iter().enumerate().map(|(x, y)| (y, x as u32)).collect(),
            next_label: Cell::new(0),
        }
    }

    pub fn label(&self) -> u32 {
        let r = self.next_label.get();
        self.next_label.set(r+1);
        r
    }

    pub fn method_index(&self, sig: &MethodSignature<'a, 'ast>) -> u32 {
        *self.by_method.get(sig).unwrap()
    }
}
