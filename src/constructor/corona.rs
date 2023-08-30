use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_corona_product(constructor: &Constructor, g: &Graph, h: &Graph) -> Graph {
    let gn = g.n.to_usize();
    let hn = h.n.to_usize();
    let order = Order::of_usize(gn * (1 + hn));
    let mut adj_list = VertexVec::new(order, &vec![]);
    
    // Edges within G
    for (u, v) in g.iter_pairs() {
        if g.adj[u][v] {
            adj_list[u].push(v);
            adj_list[v].push(u);
        }
    }

    // Edges within all the copies of H
    for (uh, vh) in h.iter_pairs() {
        if h.adj[uh][vh] {
            let mut u = uh.incr_by(gn);
            let mut v = vh.incr_by(gn);
            for _i in 0..gn {
                adj_list[u].push(v);
                adj_list[v].push(u);
                u = u.incr_by(hn);
                v = v.incr_by(hn);
            }
        }
    }

    // Edges between G and all the copies of H
    for vh in h.iter_verts() {
        let mut v = vh.incr_by(gn);
        for u in g.iter_verts() {
            adj_list[u].push(v);
            adj_list[v].push(u);
            v = v.incr_by(hn);
        }
    }

    Graph::of_adj_list(adj_list, constructor.to_owned())
}