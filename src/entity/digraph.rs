use queues::*;
use rand::{thread_rng, Rng};
use utilities::{edge_tools::*, vertex_tools::*, *};

use crate::entity::graph::*;

mod standard;

#[derive(Clone)]
pub struct Digraph {
    pub n: Order,
    pub adj: VertexVec<VertexVec<bool>>,
    pub out_adj_list: VertexVec<Vec<Vertex>>,
    pub in_adj_list: VertexVec<Vec<Vertex>>,
    pub out_deg: VertexVec<Degree>,
    pub in_deg: VertexVec<Degree>,
    pub parameters: Vec<f64>,
}

pub struct EdgeIterator<'a> {
    d: &'a Digraph,
    sub_iter: VertexDirectedPairIterator,
}

impl Digraph {
    pub fn of_matrix(adj: VertexVec<VertexVec<bool>>, parameters: Vec<f64>) -> Self {
        let n = adj.len();
        let mut out_adj_list = VertexVec::new(n, &vec![]);
        let mut in_adj_list = VertexVec::new(n, &vec![]);
        let mut out_deg = VertexVec::new(n, &Degree::ZERO);
        let mut in_deg = VertexVec::new(n, &Degree::ZERO);

        for i in n.iter_verts() {
            for j in n.iter_verts() {
                if adj[i][j] {
                    out_adj_list[i].push(j);
                    in_adj_list[j].push(i);
                    out_deg[i].incr_inplace();
                    in_deg[j].incr_inplace();
                }
            }
        }

        Self {
            n,
            adj,
            out_adj_list,
            in_adj_list,
            out_deg,
            in_deg,
            parameters,
        }
    }

    pub fn of_out_adj_list(out_adj_list: VertexVec<Vec<Vertex>>, parameters: Vec<f64>) -> Self {
        let n = out_adj_list.len();
        let mut in_adj_list = VertexVec::new(n, &vec![]);
        let mut out_deg = VertexVec::new(n, &Degree::ZERO);
        let mut in_deg = VertexVec::new(n, &Degree::ZERO);
        let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));

        for (i, nbrs) in out_adj_list.iter_enum() {
            for j in nbrs.iter() {
                in_adj_list[*j].push(i);
                out_deg[i].incr_inplace();
                in_deg[*j].incr_inplace();
                adj[i][*j] = true;
            }
        }

        Self {
            n,
            adj,
            out_adj_list,
            in_adj_list,
            out_deg,
            in_deg,
            parameters,
        }
    }

    pub fn undirect(&self) -> Graph {
        Graph::of_matrix_to_be_symmetrised(&self.adj, crate::constructor::Constructor::Special)
    }

    pub fn reverse_edge(&mut self, e: Edge) {
        let (x, y) = if self.adj[e.fst()][e.snd()] {
            (e.fst(), e.snd())
        } else {
            (e.snd(), e.fst())
        };
        if self.adj[x][y] && !self.adj[y][x] {
            self.adj[x][y] = false;
            self.adj[y][x] = true;

            let sta = self.out_adj_list[x]
                .iter()
                .position(|z| *z == y)
                .expect("wut");
            self.out_adj_list[x].remove(sta);
            let sta = self.in_adj_list[y]
                .iter()
                .position(|z| *z == x)
                .expect("wut");
            self.in_adj_list[y].remove(sta);

            self.out_adj_list[y].push(x);
            self.in_adj_list[x].push(y);
            self.out_deg[x].decr_inplace();
            self.in_deg[x].incr_inplace();
            self.out_deg[y].incr_inplace();
            self.in_deg[y].decr_inplace();
        }
    }

    /**
     * Randomly orient edges, with a set probability that an edge is in
     * fact bidirectional
     */
    pub fn random_semiorientation(g: &Graph, bidirectional_prob: f64) -> Self {
        let mut rng = thread_rng();
        let mut adj = VertexVec::new(g.n, &VertexVec::new(g.n, &false));
        for (i, j) in g.iter_pairs() {
            if g.adj[i][j] {
                if rng.gen_bool(bidirectional_prob) {
                    adj[i][j] = true;
                    adj[j][i] = true;
                } else if rng.gen_bool(0.5) {
                    adj[i][j] = true;
                } else {
                    adj[j][i] = true;
                }
            }
        }
        Self::of_matrix(adj, vec![bidirectional_prob])
    }

    pub fn random_orientation(g: &Graph) -> Self {
        let mut rng = thread_rng();
        let mut adj = VertexVec::new(g.n, &VertexVec::new(g.n, &false));
        for (i, j) in g.iter_pairs() {
            if g.adj[i][j] {
                if rng.gen_bool(0.5) {
                    adj[i][j] = true;
                } else {
                    adj[j][i] = true;
                }
            }
        }
        Self::of_matrix(adj, vec![])
    }

    /**
     * Can everywhere send a path to everywhere?
     */
    pub fn is_connected(&self) -> bool {
        for v in self.iter_verts() {
            let mut found = VertexVec::new(self.n, &false);
            let mut q = queue![];
            let _ = q.add(v);
            let mut num_found = 1;
            found[v] = true;
            while let Ok(next) = q.remove() {
                for u in self.out_adj_list[next].iter() {
                    if !found[*u] {
                        found[*u] = true;
                        num_found += 1;
                        let _ = q.add(*u);
                    }
                }
            }
            if num_found < self.n.to_usize() {
                return false;
            }
        }
        true
    }

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        self.n.iter_verts()
    }

    pub fn iter_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        self.n.iter_pairs()
    }

    pub fn iter_directed_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        self.n.iter_directed_pairs()
    }

    pub fn iter_vertex_subsets(&self) -> VertexSubsetIterator {
        VertexSubsetIterator::new(self.n)
    }

    pub fn iter_edges(&self) -> EdgeIterator {
        EdgeIterator::new(self)
    }

    pub fn print(&self) {
        println!("n: {}", self.n);
        println!(
            "in degs: {:?}",
            self.in_deg
                .iter()
                .map(|x| x.to_usize())
                .collect::<Vec<usize>>()
        );
        println!(
            "out degs: {:?}",
            self.out_deg
                .iter()
                .map(|x| x.to_usize())
                .collect::<Vec<usize>>()
        );
        println!("parameters: {:?}", self.parameters);
        for u in self.n.iter_verts() {
            if self.out_deg[u].at_least(1) {
                print!("{} -> ", u);
                for (i, v) in self.out_adj_list[u].iter().enumerate() {
                    if i == 0 {
                        print!("{}", v)
                    } else {
                        print!(", {}", v)
                    }
                }
                println!();
            }
        }
    }
}

impl EdgeIterator<'_> {
    fn new(d: &Digraph) -> EdgeIterator<'_> {
        EdgeIterator {
            d,
            sub_iter: VertexDirectedPairIterator::new(d.n),
        }
    }
}

impl Iterator for EdgeIterator<'_> {
    type Item = Edge;

    fn next(&mut self) -> Option<Self::Item> {
        let mut pair = self.sub_iter.next();
        let mut e = None;
        loop {
            if let Some((x, y)) = pair {
                if self.d.adj[x][y] {
                    e = Some(Edge::of_pair(x, y));
                    break;
                }
            } else {
                break;
            }
            pair = self.sub_iter.next();
        }
        e
    }
}
