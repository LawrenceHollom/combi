use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::*;
use std::slice::*;

use rand::rngs::ThreadRng;

use crate::*;
use crate::edge_tools::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Component(Vertex);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct EdgeComponent(Edge);

#[derive(Clone, Debug)]
pub struct ComponentVec<T: Debug + Clone> {
    vec: VertexVec<T>
}

#[derive(Clone)]
pub struct EdgeComponentVec<T: Debug + Copy>(EdgeVec<T>);

/**
 * Currently refers to VertexSet, so requires there to be at most 128 vertices.
 * (Which is silly because this whole thing's aim is to be fast for large n)
 */
#[derive(Clone, Debug)]
pub struct UnionFind {
    vec: VertexVec<Vertex>
}

/**
 * Uses BigEdgeSet, so has no size limits, but does need an EdgeIndexer, so has
 * some overheads.
 * vec: The parent edge in the UnionFind tree
 * component_sizes: Stores and updates the sizes of componenets, indexed by the root nodes.
 */
#[derive(Clone)]
pub struct EdgeUnionFind {
    vec: EdgeVec<Edge>,
    component_sizes: EdgeComponentVec<usize>,
}

impl Component {
    pub const ZERO: Self = Self(Vertex::ZERO);

    pub fn of_vertex(v: Vertex) -> Self {
        Self(v)
    }

    pub fn to_vertex(&self) -> Vertex {
        self.0
    }

    pub fn is_vertex(&self, v: Vertex) -> bool {
        self.0 == v
    }

    pub fn incr_inplace(&mut self) {
        self.0.incr_inplace()
    }
}

impl EdgeComponent {
    pub fn of_edge(e: Edge) -> Self {
        Self(e)
    }

    pub fn to_edge(&self) -> Edge {
        self.0
    }

    pub fn is_edge(&self, e: Edge) -> bool {
        self.0 == e
    }
}

impl <T: Debug + Clone> ComponentVec<T> {
    pub fn new_fn(n: Order, f: fn(Vertex) -> T) -> Self {
        Self { vec: n.iter_verts().map(|x| f(x)).collect() }
    }

    pub fn new(n: Order, t: &T) -> Self {
        Self { vec: VertexVec::new(n, t) }
    }

    pub fn new_sizes(components: &VertexVec<Component>) -> ComponentVec<usize> {
        let n = components.len();
        let mut out = ComponentVec::new(n, &0_usize);
        for c in components.iter() {
            out[*c] += 1;
        }
        out
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
        self.vec.iter().any(|t| eq(t, other))
    }

    pub fn println(&self) {
        println!("{:?}", self.vec);
    }
}

impl <T: Debug + Copy> EdgeComponentVec<T> {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>, value: T) -> Self {
        Self(EdgeVec::new(adj_list, value))
    }
    pub fn new_with_indexer(indexer: &EdgeIndexer, value: T) -> Self {
        Self(EdgeVec::new_with_indexer(indexer, value))
    }
}

impl UnionFind {
    pub fn new(n: Order) -> Self {
        Self { vec: VertexVec::new_fn(n, |x| x) }
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

    pub fn to_canonical_component_vec(self) -> VertexVec<Component> {
        let n = self.vec.len();
        let mut reps = ComponentVec::new(n, &None);
        for (v, comp_v) in self.vec.iter_enum() {
            let comp = Component::of_vertex(*comp_v);
            if reps[comp].is_none() {
                reps[comp] = Some(v)
            }
        }
        VertexVec::new_fn(n, |v| Component::of_vertex(reps[self.get_component(v)].unwrap()))
    }

    pub fn get_representatives(&self) -> VertexSet {
        let n = self.vec.len();
        let mut found = ComponentVec::new(n, &false);
        let mut reps = VertexSet::new(n);
        for (v, comp_v) in self.vec.iter_enum() {
            let comp = self.get_component(*comp_v);
            if !found[comp] {
                found[comp] = true;
                reps.add_vert(v);
            }
        }
        reps
    }
}

impl EdgeUnionFind {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> Self {
        Self { 
            vec: EdgeVec::new_fn(adj_list, |e| e),
            component_sizes: EdgeComponentVec::new(adj_list, 1),
        }
    }

    fn flatten_to(&mut self, x: Edge, root: Edge) {
        let mut z = x;
        while self.vec[z] != root {
            let sta = self.vec[z];
            self.vec[z] = root;
            z = sta;
        }
    }

    /**
     * Combine the components of the edges x and y
     */
    pub fn merge(&mut self, x: Edge, y: Edge) {
        let mut root = x;
        while root != self.vec[root] {
            root = self.vec[root];
        }
        let mut other_root = y;
        while other_root != self.vec[other_root] {
            other_root = self.vec[other_root];
        }
        // root is the new parent of the joint component.
        self.flatten_to(x, root);
        self.flatten_to(y, root);
        if root != other_root {
            // The components are different and so we need to combine sizes.
            let other_size = self.component_sizes[EdgeComponent::of_edge(other_root)];
            self.component_sizes[EdgeComponent::of_edge(root)] += other_size;
        }
    }

    /**
     * Returns the component of the edge e.
     */
    pub fn get_component(&self, e: Edge) -> EdgeComponent {
        let mut x = self.vec[e];
        while x != self.vec[x] {
            x = self.vec[x];
        }
        EdgeComponent::of_edge(x)
    }

    pub fn get_component_size(&self, comp: EdgeComponent) -> usize {
        self.component_sizes[comp]
    }

    pub fn get_component_size_from_edge(&self, e: Edge) -> usize {
        self.component_sizes[self.get_component(e)]
    }

    /**
     * Returns a BigEdgeSet containing precisely one edge from each component.
     */
    pub fn get_representatives(&self) -> BigEdgeSet {
        let indexer = self.vec.get_indexer();
        let mut found = EdgeComponentVec::new_with_indexer(indexer, false);
        let mut representatives = BigEdgeSet::new(indexer);
        for (e, component_edge) in self.vec.iter_enum() {
            let comp = self.get_component(*component_edge);
            if !found[comp] {
                found[comp] = true;
                representatives.add_edge(e);
            }
        }
        representatives
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

impl<T: Debug + Copy> Index<EdgeComponent> for EdgeComponentVec<T> {
    type Output = T;

    fn index(&self, index: EdgeComponent) -> &Self::Output {
        &self.0[index.0]
    }
}

impl<T: Debug + Copy> IndexMut<EdgeComponent> for EdgeComponentVec<T> {
    fn index_mut(&mut self, index: EdgeComponent) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}