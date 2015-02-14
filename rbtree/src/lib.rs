#![allow(unstable)]

use std::borrow::BorrowFrom;
use std::rc::Rc;
use std::cmp::Ordering::*;
use Node::*;
use Color::*;

/// A persistent, immutable red-black tree.
pub struct RbMap<T, U> {
    root: Node<T, U>,
}

impl<T, U> RbMap<T, U> {
    pub fn new() -> RbMap<T, U> {
        RbMap { root: Leaf }
    }
    pub fn insert(&self, k: T, v: U) -> (RbMap<T, U>, Option<&(T, U)>) where T: Ord {
        let (node, prev) = self.root.insert(k, v);
        (RbMap { root: node }, prev)
    }
    pub fn get<'a, Q: ?Sized + BorrowFrom<T> + Ord>(&'a self, k: &Q) -> Option<&'a U> {
        self.root.lookup(k)
    }
    pub fn iter<'a>(&'a self) -> RbMapIter<'a, T, U> {
        RbMapIter { nodes: vec![IterNode::Node(&self.root)] }
    }
}

enum IterNode<'a, T: 'a, U: 'a> {
    Node(&'a Node<T, U>),
    Item(&'a (T, U)),
}

pub struct RbMapIter<'a, T: 'a, U: 'a> {
    nodes: Vec<IterNode<'a T, U>>,
}

impl<'a, T, U> Iterator for RbMapIter<'a, T, U> {
    type Item = &'a (T, U);
    fn next(&mut self) -> Option<&'a (T, U)> {
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
enum Node<T, U> {
    Branch(Color, Rc<Node<T, U>>, Rc<(T, U)>, Rc<Node<T, U>>),
    Leaf
}

/*
impl<T, U> Node<T, U> {
    fn into_quasi(self) -> Quasi<T, U> {
        match self {
            Node::Red(a, b, c) => Quasi::Red(a, b, c),
            Node::Black(a, b, c) => Quasi::Black(a, b, c),
            Node::Leaf => Quasi::BLeaf,
        }
    }
}

enum Quasi<T, U> {
    NBlack(Rc<Node<T, U>>, (T, U), Rc<Node<T, U>>),
    Red(Rc<Node<T, U>>, (T, U), Rc<Node<T, U>>),
    Black(Rc<Node<T, U>>, (T, U), Rc<Node<T, U>>),
    BBlack(Rc<Node<T, U>>, (T, U), Rc<Node<T, U>>),
    BLeaf,
    BBLeaf,
}
fn red<T, U>(l: Rc<Node<T, U>>, m: Rc<(T, U)>, r: Rc<Node<T, U>>) -> Rc<Node<T, U>> {
    Rc::new(Branch(Red, l, m, r))
}
*/

fn black<T, U>(l: Rc<Node<T, U>>, m: Rc<(T, U)>, r: Rc<Node<T, U>>) -> Rc<Node<T, U>> {
    Rc::new(Branch(Black, l, m, r))
}
// XXX
fn leaf<T, U>() -> Rc<Node<T, U>> {
    Rc::new(Leaf)
}

fn balance<T, U>(t: Node<T, U>) -> Node<T, U> {
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

impl<T, U> Node<T, U> {
    fn insert(&self, k: T, v: U) -> (Node<T, U>, Option<&(T, U)>) where T: Ord {
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
                    (Branch(c, l.clone(), Rc::new((k, v)), r.clone()), Some(&**m))
                }
            },
            Leaf => (Branch(Red, leaf(), Rc::new((k, v)), leaf()), None)
        }
    }
    fn lookup<'a, Q: ?Sized + BorrowFrom<T> + Ord>(&'a self, k: &Q) -> Option<&'a U> {
        match *self {
            Branch(_, ref l, ref m, ref r) => match k.cmp(BorrowFrom::borrow_from(&m.0)) {
                Less => l.lookup(k),
                Greater => r.lookup(k),
                Equal => Some(&m.1),
            },
            Leaf => None,
        }
    }
    /*
    fn remove<Q: ?Sized + BorrowFrom<T>>(&self, k: &Q) -> Node<T, U> {
        panic!()
    }
    */
}

#[cfg(test)]
impl<T, U> Node<T, U> {
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
