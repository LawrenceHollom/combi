use rand::{Rng, thread_rng};
use utilities::vertex_tools::*;

use crate::entity::graph::*;

fn is_unfriendly(g: &Graph, left: VertexSet) -> bool {
    for v in g.iter_verts() {
        let mut num_same = 0;
        let mut num_other = 0;
        for w in g.adj_list[v].iter() {
            if left.has_vert(v) == left.has_vert(*w) {
                num_same += 1;
            } else {
                num_other += 1;
            }
        }
        if num_same > num_other {
            return false;
        }
    }
    true
}

/**
 * Pick a random vertex and force that to be on the LHS of the
 * partition, Then test whether some other vertex is forced
 * to be on the same side in any unfriendly partition.
 */
pub fn can_force_random(g: &Graph, should_print: bool) -> bool {
    let mut rng = thread_rng();

    let u = Vertex::of_usize(rng.gen_range(0..g.n.to_usize()));
    let mut can_be_left = VertexVec::new(g.n, &false);
    let mut can_be_right = VertexVec::new(g.n, &false);
    let mut num_unfriendly = 0;
    let dists = g.flood_fill_dist(u);
    'test_set: for left in g.iter_vertex_subsets() {
        if !left.has_vert(u) {
            // We only care about sets containing u
            continue 'test_set
        }
        // Test if this partition is unfriendly
        let is_unfriendly = is_unfriendly(g, left);

        if is_unfriendly {
            num_unfriendly += 1;
            for v in g.iter_verts() {
                if left.has_vert(v) {
                    can_be_left[v] = true;
                } else {
                    can_be_right[v] = true;
                }
            }
        }
    }

    let mut out = false;
    if should_print {
        println!("Num_unfriendly = {}", num_unfriendly);
    }
    for v in g.iter_verts() {
        let dist = dists[v].unwrap_or(999);
        if should_print {
            println!("{}: {}, {} (dist = {})", v, can_be_left[v], can_be_right[v], dist);
        }
        if v != u && can_be_left[v] && !can_be_right[v] && dist >= 3 {
            out = true
        }
    }
    out
}

pub fn num_unfriendly(g: &Graph) -> u32 {
    let mut out = 0;
    for left in g.iter_vertex_subsets() {
        if is_unfriendly(g, left) {
            out += 1;
        }
    }
    out
}