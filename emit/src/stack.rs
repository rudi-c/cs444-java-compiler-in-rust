use middle::middle::*;
use ast::name::Name;
use rbtree::RbMap;
use std::borrow::ToOwned;

#[derive(Show, Clone)]
pub struct Stack {
    // The number of variables stored in the local stack frame.
    // This is the number of dwords between `esp` and `ebp`.
    pub size: u32,
    // The index of each variable, relative to `ebp`.
    // Local variables are at -1, -2, ...; parameters are at +1, +2, ....
    pub vars: RbMap<Name, i32>,
}

thread_local!(static THIS: Name = Name::fresh("this".to_owned()));

fn this() -> Name {
    THIS.with(|this| *this)
}

impl Stack {
    // Initialize the stack frame with the locations of this function's parameters.
    pub fn new(args: &[VariableRef]) -> Self {
        let mut s = Stack { size: 0, vars: RbMap::new() };
        s.add_args(args);
        s
    }
    pub fn add_args(&mut self, args: &[VariableRef]) {
        // Arguments are in reverse order; `this` is at the end.
        for (ix, name) in args.iter().rev().map(|var| var.fq_name)
            .chain(Some(this()).into_iter())
            .enumerate() {
            self.vars.insert_in_place(name, ix as i32 + 1);
        }
    }
    // Record that a new variable was pushed onto the stack.
    // (Doesn't emit a push instruction.)
    pub fn add_var(&mut self, name: Name) -> i32 {
        self.size += 1;
        let ix = -(self.size as i32);
        self.vars.insert_in_place(name, ix);
        ix
    }

    pub fn var_index(&self, name: Name) -> i32 {
        *self.vars.get(&name).unwrap()
    }

    // The index of `this`. We don't check that the current method is
    // non-static; this should have been handled during type-checking.
    pub fn this_index(&self) -> i32 {
        *self.vars.get(&this()).unwrap()
    }

    // Open a new scope which can admit new variables.
    // At the end, automatically emit the corresponding stack adjustment
    // to return to the parent scope.
    pub fn scope<F: FnOnce(&mut Stack)>(&self, f: F) {
        let mut new = self.clone();
        println!("; begin new scope");
        f(&mut new);
        assert!(new.size >= self.size);
        if new.size > self.size {
            println!("add esp, {} ; end scope", 4*(new.size - self.size));
        }
    }
}
