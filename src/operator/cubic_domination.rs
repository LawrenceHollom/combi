use crate::{graph::*, operator::domination};

use utilities::vertex_tools::*;

use super::connectedness;

pub fn is_good(g: &Graph) -> bool {
    let mut twos = vec![];
    for (v, d) in g.deg.iter_enum() {
        if d.equals(2) {
            twos.push(v);
        }
    }
    let num_twos = twos.len();
    if num_twos >= 3 {
        let mut new_adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(g.n.incr(), &vec![]);
        for u in g.n.iter_verts() {
            for v in g.adj_list[u].iter() {
                new_adj_list[u].push(*v);
            }
        }
        let new_vert = Vertex::of_usize(g.n.to_usize());
        for x in twos.iter() {
            new_adj_list[*x].push(new_vert);
            new_adj_list[new_vert].push(*x);
        }
        let h = Graph::of_adj_list(new_adj_list, crate::constructor::Constructor::Special);
        if !connectedness::is_k_connected(&h, 3) {
            return false;
        }
    }

    // Now test what happens to gamma with various combinations of the deg-two verts
    // already dominated
    let cap = 2_usize.pow(num_twos as u32);
    let mut masked_domination = vec![0; cap];
    let mut num_good = 0;
    let target_gamma = (g.n.to_usize() / 3) + 1;
    for mask in 0..cap {
        masked_domination[mask] = domination::domination_number_with_predominations(g, VertexSet::of_usize(cap));
        if masked_domination[mask] == target_gamma as u32 {
            num_good += 1;
        }
    }
    if num_good >= 2 {
        println!("{:?}", masked_domination);
    }

    //println!("Masked domination: {:?}", masked_domination);
    num_good >= num_twos + 1 && masked_domination[cap - 1] >= target_gamma as u32 - 1
}