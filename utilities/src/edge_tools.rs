use std::collections::hash_map::DefaultHasher;
use std::hash::Hasher;
use std::{cmp::Ordering, hash::Hash};
use std::fmt;
use std::fmt::Debug;
use std::ops::*;

use crate::{vertex_tools::*, Order};

#[derive(Copy, Clone, Debug, Hash)]
pub struct Edge(Vertex, Vertex);

#[derive(Hash, Clone, Copy, PartialEq, Eq)]
pub struct EdgeSet {
    edges: u128,
    indexer_hash: u64,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct EdgeIndexer {
    indexer: Vec<Option<usize>>,
    indexer_inv: Vec<Edge>,
    num_edges: usize,
    pub hash: u64,
}

#[derive(Clone, PartialEq, Eq)]
pub struct EdgeSetIterator {
    indexer: EdgeIndexer,
    edges: u128,
}


#[derive(Clone)]
pub struct EdgeVec<T: Debug + Copy> {
    indexer: EdgeIndexer,
    vec: Vec<T>,
}

pub fn detriangle(x: usize) -> usize {
    (1 + ((1 + 8 * x) as f64).sqrt() as usize) / 2
}

impl Edge {
    pub const FILLER: Edge = Edge(Vertex::ZERO, Vertex::ZERO);

    pub fn of_string(text: &str) -> Edge {
        let pars: Vec<&str> = text.split("~").map(|x| x.trim()).collect();
        Edge(Vertex::of_string(pars[0]), Vertex::of_string(pars[1]))
    }

    pub fn of_pair(x: Vertex, y: Vertex) -> Edge {
        Edge(x, y)
    }

    pub fn to_pair(&self) -> (Vertex, Vertex) {
        (self.0, self.1)
    }

    pub fn encode(&self) -> usize {
        self.0.encode_with(self.1)
    }

    pub fn decode(i: usize) -> Edge {
        let x = detriangle(i);
        let y = i - x;
        Edge::of_pair(Vertex::of_usize(x), Vertex::of_usize(y))
    }

    pub fn fst(&self) -> Vertex {
        self.0
    }

    pub fn snd(&self) -> Vertex {
        self.1
    }

    pub fn incr_by_order(&self, n: Order) -> Edge {
        Edge::of_pair(self.0.incr_by_order(n), self.1.incr_by_order(n))
    }

    pub fn contains(&self, v: Vertex) -> bool {
        self.0 == v || self.1 == v
    }

    pub fn incidents(&self, d: Vertex) -> bool {
        self.0 == d
    }    
}

impl PartialEq for Edge {
    fn eq(&self, other: &Edge) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.1 == other.0)
    }
}

impl Eq for Edge { }

fn make_indexer(adj_list: &VertexVec<Vec<Vertex>>) -> (Vec<Option<usize>>, Vec<Edge>, usize) {
    let n = adj_list.len();
    let mut indexer: Vec<Option<usize>> = vec![None; n.triangle()];
    let mut indexer_inv: Vec<Edge> = vec![];
    let mut i = 0;
    for (u, adj) in adj_list.iter_enum() {
        for v in adj.iter() {
            if *v > u {
                let e = Edge::of_pair(u, *v);
                indexer[e.encode()] = Some(i);
                indexer_inv.push(e);
                i += 1;
            }
        }
    }
    (indexer, indexer_inv, i)
}

impl EdgeIndexer {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> EdgeIndexer {
        let (indexer, indexer_inv, num_edges) = make_indexer(adj_list);
        let hash = Self::default_hash(&indexer);
        EdgeIndexer { indexer, indexer_inv, num_edges, hash }
    }

    pub fn new_complete(order: Order) -> EdgeIndexer {
        let mut indexer: Vec<Option<usize>> = vec![None; order.triangle()];
        let mut indexer_inv: Vec<Edge> = vec![];
        let mut i = 0;
        for (u, v) in order.iter_pairs() {
            let e = Edge::of_pair(u, v);
            indexer[e.encode()] = Some(i);
            indexer_inv.push(e);
            i += 1;
        }
        let hash = Self::default_hash(&indexer);
        EdgeIndexer { indexer, indexer_inv, num_edges: i, hash }
    }

    pub fn index(&self, e: Edge) -> Option<usize> {
        self.indexer[e.encode()]
    }

    pub fn index_unwrap(&self, e: Edge) -> usize {
        self.indexer[e.encode()].unwrap()
    }

    pub fn invert(&self, i: usize) -> Edge {
        self.indexer_inv[i]
    }

    fn default_hash(indexer: &Vec<Option<usize>>) -> u64 {
        let mut s = DefaultHasher::new();
        indexer.hash(&mut s);
        s.finish()
    }

    pub fn iter_edges(&self) -> impl Iterator<Item = &Edge> {
        self.indexer_inv.iter()
    }
}

impl EdgeSetIterator {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> EdgeSetIterator {
        EdgeSetIterator {
            indexer: EdgeIndexer::new(adj_list),
            edges: 0,
        }
    }
}

impl EdgeSet {
    pub fn new(indexer: &EdgeIndexer) -> EdgeSet {
        EdgeSet { 
            edges: 0,
            indexer_hash: indexer.hash,
        }
    }

    fn of_int(code: u128, indexer: &EdgeIndexer) -> EdgeSet {
        EdgeSet { 
            edges: code,
            indexer_hash: indexer.hash,
        }
    }

    fn check_indexer(&self, indexer: &EdgeIndexer) {
        if self.indexer_hash != indexer.hash {
            panic!("Indexer and EdgeSet hash values do not agree!")
        }
    }

    pub fn add_edge(&mut self, e: Edge, indexer: &EdgeIndexer) {
        self.check_indexer(indexer);
        self.edges |= 1 << indexer[e].unwrap();
    }

    pub fn remove_edge(&mut self, e: Edge, indexer: &EdgeIndexer) {
        self.check_indexer(indexer);
        self.edges &= !(1 << indexer[e].unwrap());
    }

    pub fn flip_edge(&mut self, e: Edge, indexer: &EdgeIndexer) {
        self.check_indexer(indexer);
        self.edges ^= 1 << indexer[e].unwrap();
    }

    pub fn inverse(&self, indexer: &EdgeIndexer) -> EdgeSet {
        self.check_indexer(indexer);
        let mut set = self.to_owned();
        set.edges ^= (1 << indexer.num_edges) - 1;
        set
    }

    pub fn has_edge(&self, e: Edge, indexer: &EdgeIndexer) -> bool {
        self.check_indexer(indexer);
        (self.edges >> indexer[e].unwrap()) % 2 == 1
    }

    pub fn is_empty(&self) -> bool {
        self.edges == 0
    }

    pub fn size(&self) -> usize {
        let mut size = 0;
        let mut sta = self.edges;
        while sta > 0 {
            size += sta % 2;
            sta /= 2;
        }
        size as usize
    }

    // No safety net; perhaps there should be.
    pub fn inter(&self, other: &EdgeSet) -> EdgeSet {
        if self.indexer_hash == other.indexer_hash {
            EdgeSet {
                edges: self.edges & other.edges,
                indexer_hash: self.indexer_hash,
            }
        } else {
            panic!("Cannot intersect EdgeSets corresponding to different graphs!")
        }
    }
}

impl<T: Debug + Copy> EdgeVec<T> {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>, value: T) -> EdgeVec<T> {
        let indexer = EdgeIndexer::new(adj_list);
        let num_edges = indexer.num_edges;
        EdgeVec {
            indexer,
            vec: vec![value; num_edges],
        }
    }

    pub fn new_fn(adj_list: &VertexVec<Vec<Vertex>>, f: fn(Edge) -> T) -> EdgeVec<T> {
        let indexer = EdgeIndexer::new(adj_list);
        let mut vec = vec![f(Edge::FILLER); indexer.num_edges];

        for (u, adj) in adj_list.iter_enum() {
            for v in adj.iter() {
                if *v > u {
                    let e = Edge::of_pair(u, *v); 
                    vec[indexer.index_unwrap(e)] = f(e);
                }
            }
        }

        EdgeVec { indexer, vec }
    }

    pub fn set(&mut self, e: Edge, value: T) {
        self.vec[self.indexer.index_unwrap(e)] = value;
    }

    pub fn get(&self, e: Edge) -> T {
        self.vec[self.indexer.index_unwrap(e)]
    }

    pub fn find_max(&self, min: T, cmp: fn(&T, &T) -> Ordering) -> Option<Edge> {
        let mut best_edge = None;
        let mut max = min;

        for (i, val) in self.vec.iter().enumerate() {
            if cmp(val, &max) == Ordering::Greater {
                max = *val;
                best_edge = Some(self.indexer.invert(i));
            }
        }

        best_edge
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.vec.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.vec.iter_mut()
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (Edge, &T)> {
        self.vec.iter().enumerate().map(|(i, t)| (self.indexer.invert(i), t))
    }

    pub fn iter_mut_enum(&mut self) -> impl Iterator<Item = (Edge, &mut T)> {
        self.vec.iter_mut().enumerate().map(|(i, t)| (self.indexer.invert(i), t))
    }

    fn n(&self) -> usize {
        detriangle(self.vec.len())
    }

    pub fn print(&self) {
        let n = Order::of_usize(self.n());
        for (u, v) in n.iter_pairs() {
            let e = Edge::of_pair(u, v);
            match self.indexer.index(e) {
                Some(i) => {
                    println!("{} ~ {}: {:?}", u, v, self.vec[i]);
                }
                None => (),
            }
        }
    }
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ~ {}", self.0, self.1)
    }
}

impl fmt::Display for EdgeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:b}", self.edges)
    }
}

impl<T: Debug + Copy> fmt::Display for EdgeVec<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.vec)
    }
}

impl Index<Edge> for EdgeIndexer {
    type Output = Option<usize>;

    fn index(&self, index: Edge) -> &Self::Output {
        // Something tells me this isn't the intended solution.
        &self.indexer[index.encode()]
    }
}

impl<T: Debug + Copy> Index<Edge> for EdgeVec<T> {
    type Output = T;

    fn index(&self, index: Edge) -> &Self::Output {
        &self.vec[self.indexer.index_unwrap(index)]
    }
}

impl<T: Debug + Copy> IndexMut<Edge> for EdgeVec<T> {
    fn index_mut(&mut self, index: Edge) -> &mut Self::Output {
        &mut self.vec[self.indexer.index_unwrap(index)]
    }
}

impl Iterator for EdgeSetIterator {
    type Item = EdgeSet;

    fn next(&mut self) -> Option<Self::Item> {
        let edges = self.edges;

        if edges == 2_u128.pow(self.indexer.num_edges as u32) {
            None
        } else {
            self.edges += 1;
            Some(EdgeSet::of_int(edges, &self.indexer))
        }
    }
}