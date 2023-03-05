use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::ops::*;
use std::slice::*;

use crate::Order;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vertex(usize);

#[derive(Clone, Debug)]
pub struct VertexVec<T: Debug + Clone> {
    vec: Vec<T>
}

pub struct VertexPair {
    curr: (Vertex, Vertex),
    n: Order,
}

impl Vertex {
    pub const ZERO: Vertex = Vertex(0);

    pub fn of_string(text: &str) -> Vertex {
        Vertex(text.trim().parse().unwrap())
    }

    pub fn of_usize(x: usize) -> Vertex {
        Vertex(x)
    }

    pub fn incr(&self, n: Order) -> Vertex {
        Vertex((self.0 + 1) % n.to_usize())
    }

    pub fn incr_inplace(&mut self, n: Order) {
        self.0 = (self.0 + 1) % n.to_usize();
    }

    pub fn decr(&self, n: Order) -> Vertex {
        Vertex((self.0 + n.to_usize() - 1) % n.to_usize())
    }

    pub fn div(&self, denom: usize) -> Vertex {
        Vertex(self.0 / denom)
    }

    pub fn div_inplace(&mut self, denom: usize) {
        self.0 /= denom
    }

    pub fn rem(&self, denom: usize) -> Vertex {
        Vertex(self.0 % denom)
    }

    pub fn xor(&self, other: Vertex) -> Vertex {
        Vertex(self.0 ^ other.0)
    }

    pub fn encode_with(&self, other: Vertex) -> usize {
        fn tri(x: usize) -> usize { (x * (x - 1)) / 2 }
        if self.0 > other.0 {
            tri(self.0) + other.0
        } else {
            tri(other.0) + self.0
        }
    }

    pub fn is_n(&self, n: Order) -> bool {
        n.to_usize() == self.0
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn is_max_less_one(&self, n: Order) -> bool {
        n.to_usize() == self.0 + 1
    }
}

impl VertexPair {
    pub fn new(n: Order) -> VertexPair {
        VertexPair { curr: (Vertex(0), Vertex(1)), n }
    }
}

impl Iterator for VertexPair {
    type Item = (Vertex, Vertex);

    fn next(&mut self) -> Option<Self::Item> {
        let i = self.curr.0;
        let j = self.curr.1;

        if j.is_n(self.n) {
            if i.is_n(self.n) {
                None
            } else {
                let k = i.incr(self.n);
                self.curr = (k, k.incr(self.n));
                Some((i, j))
            }
        } else {
            self.curr = (i, j.incr(self.n));
            Some((i, j))
        }
    }
}

impl <T: Debug + Clone> VertexVec<T> {
    pub fn new_fn(n: Order, f: fn(Vertex) -> T) -> VertexVec<T> {
        VertexVec { vec: n.iter_verts().map(|x| f(x)).collect() }
    }

    pub fn new(n: Order, t: &T) -> VertexVec<T> {
        VertexVec { vec: vec![t.to_owned(); n.to_usize()] }
    }

    pub fn len(&self) -> Order {
        Order::of_usize(self.vec.len())
    }

    pub fn set(&mut self, v: Vertex, value: T) {
        self.vec[v.0] = value;
    }

    pub fn get(&self, v: Vertex) -> &T {
        &self.vec[v.0]
    }

    pub fn arg_max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<Vertex> {
        let mut best_vert = None;
        let mut max = min;

        for (i, val) in self.vec.iter().enumerate() {
            if cmp(val, &max) == Ordering::Greater {
                max = val;
                best_vert = Some(Vertex(i));
            }
        }

        best_vert
    }

    pub fn max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<&T> {
        self.arg_max(min, cmp).map(|x| &self[x])
    }

    pub fn iter(&self) -> Iter<T> {
        self.vec.iter()
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (Vertex, &T)> {
        self.vec.iter().enumerate().map(|(i, v)| (Vertex::of_usize(i), v))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.vec.iter_mut()
    }

    pub fn iter_mut_enum(&mut self) -> impl Iterator<Item = (Vertex, &mut T)>{
        self.vec.iter_mut().enumerate().map(|(i, t)| (Vertex::of_usize(i), t))
    }

    pub fn println(&self) {
        println!("{:?}", self.vec);
    }
}

impl fmt::Display for Vertex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Debug + Clone> Index<Vertex> for VertexVec<T> {
    type Output = T;

    fn index(&self, index: Vertex) -> &Self::Output {
        &self.vec[index.0]
    }
}

impl<T: Debug + Clone> IndexMut<Vertex> for VertexVec<T> {
    fn index_mut(&mut self, index: Vertex) -> &mut Self::Output {
        &mut self.vec[index.0]
    }
}