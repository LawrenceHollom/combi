use rand::{thread_rng, Rng};
use queues::*;
use utilities::{*, vertex_tools::*, edge_tools::*};

use crate::entity::graph::*;

#[derive(Clone)]
pub struct Digraph {
    pub n: Order,
    pub adj: VertexVec<VertexVec<bool>>,
    pub out_adj_list: VertexVec<Vec<Vertex>>,
    pub in_adj_list: VertexVec<Vec<Vertex>>,
    pub out_deg: VertexVec<Degree>,
    pub in_deg: VertexVec<Degree>,
}

impl Digraph {
    pub fn of_matrix(adj: VertexVec<VertexVec<bool>>) -> Digraph {
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

        Digraph { n, adj, out_adj_list, in_adj_list, out_deg, in_deg }
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

            let sta = self.out_adj_list[x].iter().position(|z| *z == y).expect("wut");
            self.out_adj_list[x].remove(sta);
            let sta = self.in_adj_list[y].iter().position(|z| *z == x).expect("wut");
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
    pub fn random_semiorientation(g: &Graph, bidirectional_prob: f64) -> Digraph {
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
        Digraph::of_matrix(adj)
    }

    pub fn random_orientation(g: &Graph) -> Digraph {
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
        Digraph::of_matrix(adj)
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
                return false
            }
        }
        true
    }

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        self.n.iter_verts()
    }

    pub fn print(&self) {
        println!("n: {}", self.n);
        println!("in degs: {:?}", self.in_deg.iter().map(|x| x.to_usize()).collect::<Vec<usize>>());
        println!("out degs: {:?}", self.out_deg.iter().map(|x| x.to_usize()).collect::<Vec<usize>>());
        for i in self.n.iter_verts() {
            if self.out_deg[i].at_least(1) {
                print!("{} ~ ", i);
                for j in self.out_adj_list[i].iter() {
                    print!("{} ", j);
                }
                println!();
            }
        }
    }
}