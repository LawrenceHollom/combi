use crate::dossier::percolate::*;
use crate::entity::graph::*;

use utilities::polynomial::*;
use utilities::vertex_tools::*;

pub fn print_polynomials(g: &Graph) {
    let mut percolator = Percolator::new(g.n.times(2), g.size());

    let posts: VertexVec<bool> = VertexVec::of_vec(vec![false, true, false, false]);

    for subset in 0..utilities::pow(2, g.size() as u64) {
        let mut up_edges = vec![false; g.size()];
        let mut num_down_edges = 0;
        let mut sta = subset;
        for up_edge in up_edges.iter_mut().take(g.size()) {
            if sta % 2 == 1 {
                *up_edge = true;
            } else {
                num_down_edges += 1;
            }
            sta /= 2;
        }

        let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(g.n.times(2), &vec![]);
        let mut edge_index = 0;
        for (i, j) in g.n.iter_pairs() {
            if g.adj[i][j] {
                if up_edges[edge_index] {
                    adj_list[i.incr_by_order(g.n)].push(j.incr_by_order(g.n));
                    adj_list[j.incr_by_order(g.n)].push(i.incr_by_order(g.n));
                } else {
                    adj_list[i].push(j);
                    adj_list[j].push(i);
                }
                edge_index += 1;
            }
        }

        for i in g.n.iter_verts() {
            if posts[i] {
                adj_list[i].push(i.incr_by_order(g.n));
                adj_list[i.incr_by_order(g.n)].push(i);
            }
        }

        let reduced_g = Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special);
        percolator.add_percolation(num_down_edges, &reduced_g, false);
    }

    g.print();

    for v in g.n.times(2).iter_verts() {
        println!("{}: {}", v, percolator.polys[v]);
    }

    println!("Differences (same + same - cross - cross):");
    for v in g.n.iter_verts() {
        let poly = percolator.polys[v].sub(&percolator.polys[v.incr_by_order(g.n)]);
        println!("old {}: {}", v, poly);
        println!(
            "new {}: {}",
            v,
            poly.add(&poly.apply(&Polynomial::of_vec(&vec![1, -1])))
        );
    }
}
