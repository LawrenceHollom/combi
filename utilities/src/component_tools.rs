use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::*;
use std::slice::*;

use rand::rngs::ThreadRng;

use crate::*;
use crate::vertex_tools::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Component(Vertex);

#[derive(Clone, Debug)]
pub struct ComponentVec<T: Debug + Clone> {
    vec: VertexVec<T>
}

pub struct UnionFind {
    vec: VertexVec<Vertex>
}

impl Component {
    pub const ZERO: Component = Component(Vertex::ZERO);

    pub fn of_vertex(v: Vertex) -> Component {
        Component(v)
    }

    pub fn to_vertex(&self) -> Vertex {
        self.0
    }

    pub fn is_vertex(&self, v: Vertex) -> bool {
        self.0 == v
    }
}

impl <T: Debug + Clone> ComponentVec<T> {
    pub fn new_fn(n: Order, f: fn(Vertex) -> T) -> ComponentVec<T> {
        ComponentVec { vec: n.iter_verts().map(|x| f(x)).collect() }
    }

    pub fn new(n: Order, t: &T) -> ComponentVec<T> {
        ComponentVec { vec: VertexVec::new(n, t) }
    }

    pub fn len(&self) -> Order {
        self.vec.len()
    }

    pub fn set(&mut self, c: Component, value: T) {
        self.vec[c.0] = value;
    }

    pub fn get(&self, c: Component) -> &T {
        &self.vec[c.0]
    }

    pub fn arg_max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<Component> {
        let mut best_vert = None;
        let mut max = min;

        for (i, val) in self.vec.iter_enum() {
            if cmp(val, &max) == Ordering::Greater {
                max = val;
                best_vert = Some(Component(i));
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

    pub fn iter_enum(&self) -> impl Iterator<Item = (Component, &T)> {
        self.vec.iter_enum().map(|(i, v)| (Component(i), v))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.vec.iter_mut()
    }

    pub fn iter_mut_enum(&mut self) -> impl Iterator<Item = (Component, &mut T)>{
        self.vec.iter_mut_enum().map(|(i, t)| (Component(i), t))
    }

    pub fn shuffle(&mut self, rng: &mut ThreadRng) {
        self.vec.shuffle(rng)
    }

    pub fn sort(&mut self, compare: fn(&T, &T) -> Ordering) {
        self.vec.sort(compare);
    }

    pub fn contains(&self, other: &T, eq: fn(&T, &T) -> bool) -> bool {
        for t in self.vec.iter() {
            if eq(t, other) {
                return true;
            }
        }
        false
    }

    pub fn println(&self) {
        println!("{:?}", self.vec);
    }
}

impl UnionFind {
    pub fn new(n: Order) -> UnionFind {
        UnionFind { vec: VertexVec::new_fn(n, |x| x) }
    }

    fn flatten_to(&mut self, x: Vertex, root: Vertex) {
        let mut z = x;
        while self.vec[z] != root {
            let sta = self.vec[z];
            self.vec[z] = root;
            z = sta;
        }
    }

    pub fn merge(&mut self, x: Vertex, y: Vertex) {
        let mut root = x;
        while root != self.vec[root] {
            root = self.vec[root];
        }
        self.flatten_to(x, root);
        self.flatten_to(y, root);
    }

    pub fn get_component(&self, v: Vertex) -> Component {
        let mut x = self.vec[v];
        while x != self.vec[x] {
            x = self.vec[x];
        }
        Component::of_vertex(x)
    }

    pub fn to_component_vec(self) -> VertexVec<Component> {
        VertexVec::new_fn(self.vec.len(), |v| self.get_component(v))
    }
}

impl<T: Debug + Clone> Index<Component> for ComponentVec<T> {
    type Output = T;

    fn index(&self, index: Component) -> &Self::Output {
        &self.vec[index.0]
    }
}

impl<T: Debug + Clone> IndexMut<Component> for ComponentVec<T> {
    fn index_mut(&mut self, index: Component) -> &mut Self::Output {
        &mut self.vec[index.0]
    }
}