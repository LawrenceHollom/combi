use rand::{thread_rng, Rng};
use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new(order: Order, p: f64) -> Graph {
    if p > 1.0 || p < 0.0 {
        panic!("The probability p must satisfy 0 <= p <= 1 (which {} does not!)", p)
    }
    let mut adj_list = VertexVec::new(order, &vec![]);
    let mut rng = thread_rng();

    for (i, j) in order.iter_pairs() {
        if rng.gen_bool(p) {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(
        RandomConstructor::ErdosRenyi(order, p)))
}

pub fn new_based(n: Order, p: f64, base: &Constructor) -> Graph {
    let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
    let base_g = base.new_entity().as_owned_graph();
    let base_n = base_g.n;
    let mut rng = thread_rng();

    for (u, v) in n.iter_pairs() {
        if rng.gen_bool(p) || (u.less_than(base_n) && v.less_than(base_n) && base_g.adj[u][v]) {
            adj[u][v] = true;
            adj[v][u] = true;
        }
    }

    Graph::of_matrix(adj, Constructor::Random(RandomConstructor::BasedErdosRenyi(n, p, Box::new(base.to_owned()))))
}

pub fn new_induced(n: Order, p: f64, base: &Constructor) -> Graph {
    let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
    let base_g = base.new_entity().as_owned_graph();
    let base_n = base_g.n;
    let mut rng = thread_rng();

    for (u, v) in n.iter_pairs() {
        if u.less_than(base_n) && v.less_than(base_n) {
            if base_g.adj[u][v] {
                adj[u][v] = true;
                adj[v][u] = true;
            }
        } else {
            if rng.gen_bool(p) {
                adj[u][v] = true;
                adj[v][u] = true;
            }
        }
    }

    Graph::of_matrix(adj, Constructor::Random(RandomConstructor::InducedErdosRenyi(n, p, Box::new(base.to_owned()))))
}

pub fn new_bipartite(order: Order, p: f64) -> Graph {
    let part_1 = order.div(2);
    let mut adj_list = VertexVec::new(order, &vec![]);
    let mut rng = thread_rng();

    for (i, j) in order.iter_pairs() {
        if (i.less_than(part_1) ^ j.less_than(part_1)) && rng.gen_bool(p) {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(
        RandomConstructor::RandomBipartite(order, p)))
}

pub fn new_random_subgraph(constructor: &Box<Constructor>, p: f64) -> Graph {
    let g = constructor.new_entity().as_owned_graph();
    let mut adj_list = VertexVec::new(g.n, &vec![]);
    let mut rng = thread_rng();

    for (i, j) in g.iter_pairs() {
        if g.adj[i][j] && rng.gen_bool(p) {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(
        RandomConstructor::RandomSubgraph(constructor.to_owned(), p)))
}