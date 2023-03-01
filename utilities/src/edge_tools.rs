use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;

#[derive(Copy, Clone, Debug, Hash)]
pub struct Edge(usize, usize);

#[derive(Clone)]
pub struct EdgeSet {
    indexer: Vec<Option<usize>>,
    edges: u128,
}

#[derive(Clone)]
pub struct EdgeVec<T: Debug + Copy> {
    indexer: Vec<Option<usize>>,
    vec: Vec<T>,
}

pub fn detriangle(x: usize) -> usize {
    (1 + ((1 + 8 * x) as f64).sqrt() as usize) / 2
}

impl Edge {
    pub fn of_string(text: &str) -> Edge {
        let pars: Vec<&str> = text.split("~").map(|x| x.trim()).collect();
        Edge(pars[0].parse().unwrap(), pars[1].parse().unwrap())
    }

    pub fn of_pair(x: usize, y: usize) -> Edge {
        Edge(x, y)
    }

    pub fn to_pair(&self) -> (usize, usize) {
        (self.0, self.1)
    }

    pub fn encode(&self) -> usize {
        fn tri(x: usize) -> usize { (x * (x - 1)) / 2 }
        if self.0 > self.1 {
            tri(self.0) + self.1
        } else {
            tri(self.1) + self.0
        }
    }

    pub fn decode(i: usize) -> Edge {
        let x = detriangle(i);
        let y = i - x;
        Edge::of_pair(x, y)
    }

    pub fn fst(&self) -> usize {
        self.0
    }

    pub fn snd(&self) -> usize {
        self.1
    }

    pub fn contains(&self, v: usize) -> bool {
        self.0 == v || self.1 == v
    }

    pub fn equals(&self, d: usize) -> bool {
        self.0 == d
    }    
}

fn make_indexer(adj_list: &Vec<Vec<usize>>) -> (Vec<Option<usize>>, usize) {
    let n = adj_list.len();
    let mut indexer: Vec<Option<usize>> = vec![None; (n * (n - 1)) / 2];
    let mut i = 0;
    for (u, adj) in adj_list.iter().enumerate() {
        for v in adj.iter() {
            if *v > u {
                indexer[Edge::of_pair(u, *v).encode()] = Some(i);
                i += 1;
            }
        }
    }
    (indexer, i)
}

impl PartialEq for Edge {
    fn eq(&self, other: &Edge) -> bool {
        (self.0 == other.0 && self.1 == other.1) || (self.0 == other.1 && self.1 == other.0)
    }
}

impl Eq for Edge { }

impl EdgeSet {
    pub fn new(adj_list: &Vec<Vec<usize>>) -> EdgeSet {
        let (indexer, _len) = make_indexer(adj_list);
        EdgeSet {
            indexer,
            edges: 0,
        }
    }

    pub fn add_edge(&mut self, e: Edge) {
        self.edges += 1 << self.indexer[e.encode()].unwrap();
    }

    pub fn has_edge(&self, e: Edge) -> bool {
        (self.edges >> self.indexer[e.encode()].unwrap()) % 2 == 1
    }
}

impl<T: Debug + Copy> EdgeVec<T> {
    pub fn new(adj_list: &Vec<Vec<usize>>, value: T) -> EdgeVec<T> {
        let (indexer, len) = make_indexer(adj_list);
        EdgeVec {
            indexer,
            vec: vec![value; len],
        }
    }

    pub fn new_fn(adj_list: &Vec<Vec<usize>>, f: fn(Edge) -> T) -> EdgeVec<T> {
        let (indexer, len) = make_indexer(adj_list);
        let mut vec = vec![f(Edge::of_pair(0, 1)); len];

        for (u, adj) in adj_list.iter().enumerate() {
            for v in adj.iter() {
                if *v > u {
                    let e = Edge::of_pair(u, *v); 
                    vec[indexer[e.encode()].unwrap()] = f(e);
                }
            }
        }

        EdgeVec { indexer, vec }
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
                best_edge = Some(Edge::decode(i));
            }
        }

        best_edge
    }

    fn n(&self) -> usize {
        detriangle(self.vec.len())
    }

    pub fn print(&self) {
        let n = self.n();
        for u in 0..(n - 1) {
            for v in (u + 1)..n {
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

