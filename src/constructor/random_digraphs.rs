use rand::{thread_rng, Rng};
use utilities::*;
use utilities::vertex_tools::*;

use super::*;


pub fn new_oriented(n: Order, min_p: f64, max_p: f64) -> Digraph {
    let mut rng = thread_rng();
    let mut adj_list = VertexVec::new(n, &vec![]);
    let p = rng.gen_range(min_p..max_p);

    for (x, y) in n.iter_pairs() {
        if rng.gen_bool(p) {
            if rng.gen_bool(0.5) {
                adj_list[x].push(y);
            } else {
                adj_list[y].push(x);
            }
        }
    }

    Digraph::of_out_adj_list(adj_list, vec![p])//, Constructor::DigraphConstr(DigraphConstructor::Oriented(n, p)))
}