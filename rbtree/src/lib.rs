#![allow(unstable)]

use std::borrow::BorrowFrom;
use std::fmt::{Show, Formatter, Error};
use std::rc::Rc;
use std::cmp::Ordering::*;
use Node::*;
use Color::*;

/// A persistent, immutable red-black tree.
pub struct RbMap<K, V> {
    root: Node<K, V>,
}

impl<K, V> RbMap<K, V> {
    /// Creates a new, empty map.
    pub fn new() -> RbMap<K, V> {
        RbMap { root: Leaf }
    }
    /// Inserts a key-value pair into the map.
    /// Returns the updated map and the previous element with the given key, if any.
    pub fn insert<'a>(&'a self, k: K, v: V) -> (RbMap<K, V>, Option<&'a (K, V)>) where K: Ord {
        let (node, prev) = self.root.insert(k, v);
        (RbMap { root: node }, prev.map(|x| &**x))
    }
    /// Inserts a key-value pair destructively.
    /// Returns the previous element with the given key, if any.
    pub fn insert_in_place<'a>(&'a mut self, k: K, v: V) -> Option<Rc<(K, V)>> where K: Ord {
        let (node, prev) = {
            let (node, prev) = self.root.insert(k, v);
            (node, prev.cloned())
        };
        self.root = node;
        prev
    }
    /// Looks up the given key in the map.
    pub fn get<'a, Q: ?Sized>(&'a self, k: &Q) -> Option<&'a V> where Q: BorrowFrom<K> + Ord {
        self.root.lookup(k)
    }
    /// Removes a key from the map, if it exists.
    /// Returns the new map and the removed element.
    ///
    /// FIXME: Actually implement this.
    pub fn remove<'a, Q: ?Sized>(&'a self, k: &Q) -> (RbMap<K, V>, Option<&'a (K, V)>) where Q: BorrowFrom<K> + Ord {
        if let Some((node, prev)) = self.root.remove(k) {
            (RbMap { root: node }, Some(prev))
        } else {
            (self.clone(), None)
        }
    }
    /// Iterates by reference over all the elements in the map.
    pub fn iter<'a>(&'a self) -> RbMapIter<'a, K, V> {
        RbMapIter { nodes: vec![IterNode::Node(&self.root)] }
    }
}

impl<K, V> Clone for RbMap<K, V> {
    fn clone(&self) -> RbMap<K, V> {
        RbMap { root: self.root.clone() }
    }
}

impl<K: Show, V: Show> Show for RbMap<K, V> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        try!(write!(f, "{{"));
        for &(ref key, ref value) in self.iter() {
            // TODO: For extra prettiness, get rid of last comma.
            try!(write!(f, "{:?}: {:?}, ", key, value));
        }
        try!(write!(f, "}}"));
        Ok(())
    }
}

enum IterNode<'a, K: 'a, V: 'a> {
    Node(&'a Node<K, V>),
    Item(&'a (K, V)),
}

/// An iterator over the entries of an RbMap.
pub struct RbMapIter<'a, K: 'a, V: 'a> {
    nodes: Vec<IterNode<'a K, V>>,
}

impl<'a, K, V> Iterator for RbMapIter<'a, K, V> {
    type Item = &'a (K, V);
    fn next(&mut self) -> Option<&'a (K, V)> {
        loop {
            let n = self.nodes.pop();
            match n {
                Some(IterNode::Node(&Branch(_, ref l, ref m, ref r))) => {
                    self.nodes.push(IterNode::Node(&**r));
                    self.nodes.push(IterNode::Item(&**m));
                    self.nodes.push(IterNode::Node(&**l));
                }
                Some(IterNode::Node(&Leaf)) => (),
                Some(IterNode::Item(x)) => return Some(x),
                None => return None,
            }
        }
    }
}

#[derive(Show, Copy, PartialEq, Eq)]
enum Color {
    Red,
    Black,
}

#[derive(Show)]
enum Node<K, V> {
    Branch(Color, Rc<Node<K, V>>, Rc<(K, V)>, Rc<Node<K, V>>),
    Leaf
}

impl<K, V> Clone for Node<K, V> {
    fn clone(&self) -> Node<K, V> {
        match *self {
            Branch(c, ref l, ref m, ref r) => Branch(c, l.clone(), m.clone(), r.clone()),
            Leaf => Leaf
        }
    }
}


/*
impl<K, V> Node<K, V> {
    fn into_quasi(self) -> Quasi<K, V> {
        match self {
            Node::Red(a, b, c) => Quasi::Red(a, b, c),
            Node::Black(a, b, c) => Quasi::Black(a, b, c),
            Node::Leaf => Quasi::BLeaf,
        }
    }
}

enum Quasi<K, V> {
    NBlack(Rc<Node<K, V>>, (K, V), Rc<Node<K, V>>),
    Red(Rc<Node<K, V>>, (K, V), Rc<Node<K, V>>),
    Black(Rc<Node<K, V>>, (K, V), Rc<Node<K, V>>),
    BBlack(Rc<Node<K, V>>, (K, V), Rc<Node<K, V>>),
    BLeaf,
    BBLeaf,
}
fn red<K, V>(l: Rc<Node<K, V>>, m: Rc<(K, V)>, r: Rc<Node<K, V>>) -> Rc<Node<K, V>> {
    Rc::new(Branch(Red, l, m, r))
}
*/

fn black<K, V>(l: Rc<Node<K, V>>, m: Rc<(K, V)>, r: Rc<Node<K, V>>) -> Rc<Node<K, V>> {
    Rc::new(Branch(Black, l, m, r))
}
// XXX
fn leaf<K, V>() -> Rc<Node<K, V>> {
    Rc::new(Leaf)
}

fn balance<K, V>(t: Node<K, V>) -> Node<K, V> {
    // With manual derefs, because Rc.
    match t {
        Branch(_, ref l, ref m, ref r) => {
            if let Branch(Red, ref l, ref lm, ref lr) = **l {
                if let Branch(Red, ref a, ref x, ref b) = **l {
                    return Branch(Red, black(a.clone(), x.clone(), b.clone()), lm.clone(), black(lr.clone(), m.clone(), r.clone()));
                }
                if let Branch(Red, ref b, ref y, ref c) = **lr {
                    return Branch(Red, black(l.clone(), lm.clone(), b.clone()), y.clone(), black(c.clone(), m.clone(), r.clone()));
                }
            }
            if let Branch(Red, ref rl, ref rm, ref r) = **r {
                if let Branch(Red, ref b, ref y, ref c) = **rl {
                    return Branch(Red, black(l.clone(), m.clone(), b.clone()), y.clone(), black(c.clone(), rm.clone(), r.clone()));
                }
                if let Branch(Red, ref c, ref z, ref d) = **r {
                    return Branch(Red, black(l.clone(), m.clone(), rl.clone()), rm.clone(), black(c.clone(), z.clone(), d.clone()));
                }
            }
        }
        _ => ()
    }
    t
}

impl<K, V> Node<K, V> {
    fn insert(&self, k: K, v: V) -> (Node<K, V>, Option<&Rc<(K, V)>>) where K: Ord {
        match *self {
            Branch(c, ref l, ref m, ref r) => match k.cmp(&m.0) {
                Less => {
                    let (node, prev) = l.insert(k, v);
                    (balance(Branch(c, Rc::new(node), m.clone(), r.clone())), prev)
                }
                Greater => {
                    let (node, prev) = r.insert(k, v);
                    (balance(Branch(c, l.clone(), m.clone(), Rc::new(node))), prev)
                }
                Equal => {
                    (Branch(c, l.clone(), Rc::new((k, v)), r.clone()), Some(m))
                }
            },
            Leaf => (Branch(Red, leaf(), Rc::new((k, v)), leaf()), None)
        }
    }
    fn lookup<'a, Q: ?Sized>(&'a self, k: &Q) -> Option<&'a V> where Q: BorrowFrom<K> + Ord {
        match *self {
            Branch(_, ref l, ref m, ref r) => match k.cmp(BorrowFrom::borrow_from(&m.0)) {
                Less => l.lookup(k),
                Greater => r.lookup(k),
                Equal => Some(&m.1),
            },
            Leaf => None,
        }
    }
    fn remove<'a, Q: ?Sized + BorrowFrom<K>>(&'a self, _k: &Q) -> Option<(Node<K, V>, &'a (K, V))> {
        panic!()
    }
}

#[cfg(test)]
impl<K, V> Node<K, V> {
    fn is_black(&self) -> bool {
        match *self {
            Branch(Black, _, _, _) | Leaf => true,
            _ => false,
        }
    }
    fn check_depth(&self, x: usize) -> usize {
        match *self {
            Branch(c, ref l, _, ref r) => {
                let diff = if c == Black { 1 } else { 0 };
                let ld = l.check_depth(x+diff);
                let rd = r.check_depth(x+diff);
                assert_eq!(ld, rd);
                if c == Red && x > 0 {
                    assert!(l.is_black());
                    assert!(r.is_black());
                }
                ld
            }
            Leaf => x
        }
    }
}


#[cfg(test)]
mod test {
    use super::*;
    macro_rules! assert_matches {
        ($e: expr, $p: pat) => (assert!(if let $p = $e { true } else { false }));
    }
    #[test]
    fn basic() {
        let m = RbMap::new();
        assert_eq!(m.root.check_depth(0), 0);
        let (m, x) = m.insert(1, 2);
        assert_eq!(x, None);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(1, 2)]);
        m.root.check_depth(0);
        let (m, x) = m.insert(2, 3);
        assert_eq!(x, None);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(1, 2), (2, 3)]);
        m.root.check_depth(0);
        let (m, x) = m.insert(4, 2);
        assert_eq!(x, None);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(1, 2), (2, 3), (4, 2)]);
        m.root.check_depth(0);
        let (m, x) = m.insert(3, 10);
        assert_eq!(x, None);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(1, 2), (2, 3), (3, 10), (4, 2)]);
        assert_matches!(m.get(&1), Some(&2));
        assert_matches!(m.get(&3), Some(&10));
        assert_matches!(m.get(&30), None);
        m.root.check_depth(0);
        let (m, x) = m.insert(1, 12);
        assert!(if let Some(&(1, 2)) = x { true } else { false });
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(1, 12), (2, 3), (3, 10), (4, 2)]);
        m.root.check_depth(0);
        let (m, x) = m.insert(-100, 0);
        assert_eq!(x, None);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), vec![(-100, 0), (1, 12), (2, 3), (3, 10), (4, 2)]);
        m.root.check_depth(0);
        assert_matches!(m.get(&-100), Some(&0));
        assert_matches!(m.get(&4), Some(&2));
        assert_matches!(m.get(&5), None);
    }

    #[test]
    fn range() {
        let m = (0..100).fold(RbMap::new(), |x, v| x.insert(v, v*2).0);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), (0..100).map(|v| (v, v*2)).collect::<Vec<_>>());
        assert_matches!(m.get(&3), Some(&6));
        assert_matches!(m.get(&30), Some(&60));
        assert_matches!(m.get(&300), None);
    }

    #[test]
    fn rev_range() {
        let m = (0..100).rev().fold(RbMap::new(), |x, v| x.insert(v, v*2).0);
        assert_eq!(m.iter().cloned().collect::<Vec<_>>(), (0..100).map(|v| (v, v*2)).collect::<Vec<_>>());
    }
}
