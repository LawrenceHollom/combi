use crate::Edge;

use std::fmt;

#[derive(Clone)]
pub struct EdgeSet {
    indexer: Vec<usize>,
    n: usize,
    edges: u128,
}

impl EdgeSet {
    pub fn new(adj_list: Vec<Vec<usize>>) -> EdgeSet {
        let n = adj_list.len();
        let mut indexer: Vec<usize> = vec![0; (n * (n - 1)) / 2];
        let mut i = 0;
        for (u, adj) in adj_list.iter().enumerate() {
            for v in adj.iter() {
                if *v > u {
                    indexer[Edge::of_pair(u, *v).encode(n)] = i;
                    i += 1;
                }
            }
        }
        EdgeSet {
            indexer,
            n,
            edges: 0,
        }
    }

    pub fn add_edge(&mut self, e: Edge) {
        self.edges += 1 << self.indexer[e.encode(self.n)];
    }

    pub fn has_edge(&self, e: Edge) -> bool {
        (self.edges >> self.indexer[e.encode(self.n)]) % 2 == 1
    }
}


impl fmt::Display for EdgeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:b}", self.edges)
    }
}