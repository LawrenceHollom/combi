use utilities::*;
use crate::graph::*;

pub fn new(order: Order, p: f64) -> Graph {
    let n = order.to_usize();
    
    let mut adj_list = VertexVec::new(order, &vec![]);
    let mut rng = thread_rng();

    for (i, j) in order.iter_pairs() {
        if rng.gen_bool(p) {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(crate::constructor::RandomConstructor::ErdosRenyi(order, p)))
}