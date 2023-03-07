use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::ops::*;

use crate::{vertex_tools::*, Order};

#[derive(Copy, Clone, Debug, Hash)]
pub struct Edge(Vertex, Vertex);

#[derive(Hash, Clone, PartialEq, Eq)]
pub struct EdgeSet {
    indexer: Vec<Option<usize>>,
    edges: u128,
}

#[derive(Clone, PartialEq, Eq)]
pub struct EdgeSetIndexer {
    indexer: Vec<Option<usize>>,
    num_edges: usize,
}

#[derive(Clone, PartialEq, Eq)]
pub struct EdgeSetIterator {
    indexer: EdgeSetIndexer,
    edges: u128,
}


#[derive(Clone)]
pub struct EdgeVec<T: Debug + Copy> {
    indexer: Vec<Option<usize>>,
    indexer_inv: Vec<Edge>,
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

impl EdgeSetIndexer {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> EdgeSetIndexer {
        let (indexer, _indexer_inv, num_edges) = make_indexer(adj_list);
        EdgeSetIndexer { indexer, num_edges }
    }
}

impl EdgeSetIterator {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> EdgeSetIterator {
        EdgeSetIterator {
            indexer: EdgeSetIndexer::new(adj_list),
            edges: 0,
        }
    }
}

impl EdgeSet {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>) -> EdgeSet {
        let (indexer, _indexer_inv, _len) = make_indexer(adj_list);
        EdgeSet {
            indexer,
            edges: 0,
        }
    }

    pub fn of_int(adj_list: &VertexVec<Vec<Vertex>>, code: u128) -> EdgeSet {
        let (indexer, _indexer_inv, _len) = make_indexer(adj_list);
        EdgeSet {
            indexer,
            edges: code,
        }
    }

    pub fn of_hack_PLEASEDELETEMESOON(indexer: &Vec<Option<usize>>, code: u128) -> EdgeSet {
        EdgeSet { indexer: indexer.to_owned(), edges: code }
    }

    pub fn add_edge(&mut self, e: Edge) {
        self.edges &= 1 << self.indexer[e.encode()].unwrap();
    }

    pub fn remove_edge(&mut self, e: Edge) {
        self.edges &= !(1 << self.indexer[e.encode()].unwrap());
    }

    pub fn flip_edge(&mut self, e: Edge) {
        self.edges ^= 1 << self.indexer[e.encode()].unwrap();
    }

    pub fn inverse(&self) -> EdgeSet {
        let mut set = self.to_owned();
        set.edges ^= 1 << self.indexer.len();
        set
    }

    pub fn has_edge(&self, e: Edge) -> bool {
        (self.edges >> self.indexer[e.encode()].unwrap()) % 2 == 1
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

    pub fn inter(&self, other: &EdgeSet) -> EdgeSet {
        EdgeSet {
            indexer: self.indexer.to_owned(),   // Yuck. Also, doesn't check if they agree.
            edges: self.edges & other.edges
        }
    }
}

impl<T: Debug + Copy> EdgeVec<T> {
    pub fn new(adj_list: &VertexVec<Vec<Vertex>>, value: T) -> EdgeVec<T> {
        let (indexer, indexer_inv, len) = make_indexer(adj_list);
        EdgeVec {
            indexer,
            indexer_inv,
            vec: vec![value; len],
        }
    }

    pub fn new_fn(adj_list: &VertexVec<Vec<Vertex>>, f: fn(Edge) -> T) -> EdgeVec<T> {
        let (indexer, indexer_inv, len) = make_indexer(adj_list);
        let mut vec = vec![f(Edge::FILLER); len];

        for (u, adj) in adj_list.iter_enum() {
            for v in adj.iter() {
                if *v > u {
                    let e = Edge::of_pair(u, *v); 
                    vec[indexer[e.encode()].unwrap()] = f(e);
                }
            }
        }

        EdgeVec { indexer, indexer_inv, vec }
    }

    pub fn set(&mut self, e: Edge, value: T) {
        self.vec[self.indexer[e.encode()].unwrap()] = value;
    }

    pub fn get(&self, e: Edge) -> T {
        self.vec[self.indexer[e.encode()].unwrap()]
    }

    pub fn find_max(&self, min: T, cmp: fn(&T, &T) -> Ordering) -> Option<Edge> {
        let mut best_edge = None;
        let mut max = min;

        for (i, val) in self.vec.iter().enumerate() {
            if cmp(val, &max) == Ordering::Greater {
                max = *val;
                best_edge = Some(self.indexer_inv[i]);
            }
        }

        best_edge
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.vec.iter()
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (Edge, &T)> {
        self.vec.iter().enumerate().map(|(i, t)| (self.indexer_inv[i], t))
    }

    fn n(&self) -> usize {
        detriangle(self.vec.len())
    }

    pub fn print(&self) {
        let n = Order::of_usize(self.n());
        for (u, v) in n.iter_pairs() {
            let e = Edge::of_pair(u, v);
            match self.indexer[e.encode()] {
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

impl Index<Edge> for EdgeSet {
    type Output = bool;

    fn index(&self, index: Edge) -> &Self::Output {
        // Something tells me this isn't the intended solution.
        if self.has_edge(index) { &true } else { &false }
    }
}

impl<T: Debug + Copy> Index<Edge> for EdgeVec<T> {
    type Output = T;

    fn index(&self, index: Edge) -> &Self::Output {
        &self.vec[self.indexer[index.encode()].unwrap()]
    }
}

impl<T: Debug + Copy> IndexMut<Edge> for EdgeVec<T> {
    fn index_mut(&mut self, index: Edge) -> &mut Self::Output {
        &mut self.vec[self.indexer[index.encode()].unwrap()]
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
            Some(EdgeSet::of_hack_PLEASEDELETEMESOON(&self.indexer.indexer, edges))
        }
    }
}