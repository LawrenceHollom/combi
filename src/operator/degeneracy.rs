use crate::graph::*;

use utilities::vertex_tools::*;
use utilities::*;

pub fn is_d_degenerate(g: &Graph, d: Degree) -> bool {
    // Remove any vertex of degree <= d, and see if we can remove
    // everything.
    let mut degs = g.deg.clone();
    let mut removed = VertexSet::new(g.n);
    let mut num_removed = 0;
    let mut removed_something = true;
    while g.n.more_than(num_removed) && removed_something {
        removed_something = false;
        for v in g.iter_verts() {
            if !removed.has_vert(v) && degs[v] <= d {
                // Remove v.
                removed.add_vert(v);
                num_removed += 1;
                removed_something = true;
                for w in g.adj_list[v].iter() {
                    degs[*w].decr_inplace();
                }
            }
        }
    }
    g.n.at_most(num_removed)
}

pub fn degeneracy(g: &Graph) -> u32 {
    // Remove the vertex of lowest degree iteratively, 
    // and return highest deg we had to remove.
    let mut degs = g.deg.clone();
    let mut removed = VertexSet::new(g.n);
    let mut num_removed = 0;
    let mut d = g.min_degree();
    let max_deg = g.max_degree();
    while g.n.more_than(num_removed) {
        let mut min_deg_found = max_deg;
        for v in g.iter_verts() {
            if !removed.has_vert(v) {
                if degs[v] < max_deg {
                    min_deg_found = degs[v];
                }
                if degs[v] <= d {
                    // Remove v.
                    num_removed += 1;
                    removed.add_vert(v);
                    for w in g.adj_list[v].iter() {
                        degs[*w].decr_inplace();
                    }
                }
            }
        }
        if min_deg_found > d {
            d = min_deg_found;
        }
    }
    d.to_usize() as u32
}