use rand::{thread_rng, Rng};
use utilities::{*, vertex_tools::*, edge_tools::*};

use crate::graph::*;

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
            self.in_adj_list[x].push(x);
            self.out_deg[x].decr_inplace();
            self.in_deg[x].incr_inplace();
            self.out_deg[y].incr_inplace();
            self.in_deg[y].decr_inplace();
        }
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

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        self.n.iter_verts()
    }

    pub fn print(&self) {
        println!("n: {}", self.n);
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