use std::cell::UnsafeCell;
use std::ops::Deref;
use std::{ptr, fmt};

/// A cell that can be assigned just once.
pub struct Ivar<T> {
    inner: UnsafeCell<Option<T>>,
}

impl<T> Ivar<T> {
    /// Creates a new, unassigned Ivar.
    pub fn new() -> Self {
        Ivar { inner: UnsafeCell::new(None) }
    }

    /// Sets the value of the Ivar.
    /// Panics if it has already been set.
    pub fn set(&self, val: T) {
        let old = unsafe { ptr::replace(self.inner.get(), Some(val)) };
        if let Some(..) = old {
            panic!("ivar assigned twice");
        }
    }
}

impl<T> Deref for Ivar<T> {
    type Target = T;
    fn deref<'a>(&'a self) -> &'a T {
        unsafe { &*self.inner.get() }.as_ref().expect("tried to read unassigned ivar")
    }
}

impl<T: fmt::Debug> fmt::Debug for Ivar<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        if let &Some(ref inner) = unsafe { &*self.inner.get() } {
            inner.fmt(f)
        } else {
            write!(f, "<uninit>")
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic() {
        let var: Ivar<i32> = Ivar::new();
        var.set(123);
        assert!(*var == 123);
    }

    #[test] #[should_panic]
    fn test_set_twice() {
        let var: Ivar<i32> = Ivar::new();
        var.set(123);
        var.set(345);
    }

    #[test] #[should_panic]
    fn test_read_uninit() {
        let var: Ivar<i32> = Ivar::new();
        *var;
    }
}
