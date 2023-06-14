use rand::{thread_rng, Rng};
use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new(order: Order, p: f64) -> Graph {    
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
    let g = constructor.new_graph();
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