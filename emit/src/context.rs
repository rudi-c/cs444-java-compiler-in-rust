use middle::middle::*;
use middle::lang_items::*;

use strings::collect_constant_strings;

use std::collections::{BTreeSet, HashMap};
use std::cell::Cell;

pub struct Context<'a, 'ast: 'a> {
    pub lang_items: LangItems<'a, 'ast>,
    pub all_methods: Vec<&'a MethodSignature<'a, 'ast>>,
    pub by_method: HashMap<&'a MethodSignature<'a, 'ast>, u32>,
    pub next_label: Cell<u32>,

    pub class_sizes: HashMap<TypeDefinitionRef<'a, 'ast>, u32>,
    pub field_offsets: HashMap<FieldRef<'a, 'ast>, u32>,

    pub string_constants: HashMap<String, u32>,
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

        let mut context = Context {
            lang_items: find_lang_items(universe.toplevel),
            all_methods: methods.iter().cloned().collect(),
            by_method: methods.into_iter().enumerate().map(|(x, y)| (y, x as u32)).collect(),
            next_label: Cell::new(0),
            class_sizes: HashMap::new(),
            field_offsets: HashMap::new(),
            string_constants: collect_constant_strings(universe),
        };

        context.compute_field_offsets(universe);

        context
    }

    fn compute_field_offsets(&mut self, universe: &Universe<'a, 'ast>) {
        universe.each_type(|tydef| {
            self.compute_field_offsets_for_type(universe, tydef);
        });
    }

    fn compute_field_offsets_for_type(&mut self, universe: &Universe<'a, 'ast>,
                                      tydef: TypeDefinitionRef<'a, 'ast>) {
        // Avoid processing multiple times.
        if self.class_sizes.contains_key(&tydef) {
            return;
        }

        if tydef.kind == TypeKind::Class {
            // Need to know the size of the parent, since the object layout of the
            // superclass is a prefix of the object layout of the class.
            let mut class_size = match &**tydef.extends {
                // Classes that inherit from object start with a pointer to the class
                // descriptor instead of the object layout of the parent.
                [] => 4,
                [parent] => {
                    self.compute_field_offsets_for_type(universe, parent);
                    match self.class_sizes.get(&parent) {
                        Some(parent_size) => *parent_size,
                        None => {
                            panic!("should have processed parent before child")
                        }
                    }
                }
                _ => panic!("class extends multiple types?")
            };

            for field in tydef.nonstatic_fields().iter() {
                // For now, give every field 4 words.
                self.field_offsets.insert(*field, class_size);
                class_size += 4;
            }

            self.class_sizes.insert(tydef, class_size);
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
