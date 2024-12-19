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

/**
 * Returns a random n-vertex digraph where every vertex has out-degree exactly d.
 */
pub fn new_out(n: Order, d: Degree) -> Digraph {
    if d.at_least(n.to_usize()) {
        panic!("Order must be less than degree!")
    }

    let mut rng = thread_rng();
    let mut out_adj_list = VertexVec::new(n, &vec![]);
    
    for v in n.iter_verts() {
        let mut connected = VertexVec::new(n, &false);
        for _i in 0..d.to_usize() {
            'find_vertex: loop {
                let u = Vertex::of_usize(rng.gen_range(0..n.to_usize()));
                if u != v && !connected[u] {
                    // Can connect to u.
                    out_adj_list[v].push(u);
                    connected[u] = true;
                    break 'find_vertex
                }
            }
        }
    }

    Digraph::of_out_adj_list(out_adj_list, vec![d.to_usize() as f64])
}