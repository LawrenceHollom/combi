use crate::{graph::*, operator::domination};

use super::connectedness;

pub fn is_good(g: &Graph) -> bool {
    let n = g.n.to_usize();
    let mut twos = vec![];
    for (v, d) in g.deg.iter().enumerate() {
        if d.equals(2) {
            twos.push(v);
        }
    }
    let mut new_adj_list: Vec<Vec<usize>> = vec![vec![]; n+1];
    for u in 0..n {
        for v in g.adj_list[u].iter() {
            new_adj_list[u].push(*v);
        }
    }
    for x in twos.iter() {
        new_adj_list[*x].push(n);
        new_adj_list[n].push(*x);
    }
    let h = Graph::of_adj_list(new_adj_list, crate::constructor::Constructor::Special);
    if !connectedness::is_k_connected(&h, 3) {
        return false;
    }

    // Switch vertices 0 and 1 to test symmetry.
    new_adj_list = vec![vec![]; n];
    fn flip(x: usize) -> usize { if x == 0 { 1 } else if x == 1 { 0 } else { x } }
    for u in 0..n {
        for v in g.adj_list[u].iter() {
            new_adj_list[flip(u)].push(flip(*v));
        }
    }
    let k = Graph::of_adj_list(new_adj_list, crate::constructor::Constructor::Special);

    // Now test what happens to gamma with various combinations of the 3 verts
    // already dominated
    let mut masked_domination = vec![0; 8];
    let mut num_sixes = 0;
    for mask in 0..8 {
        masked_domination[mask] = domination::domination_number_with_predominations(&k, mask as u128);
        if masked_domination[mask] == 6 {
            num_sixes += 1;
        }
    }
    let one = if masked_domination[1] == 6 { 1 } else { 0 };
    let two = if masked_domination[2] == 6 { 1 } else { 0 };
    let four = if masked_domination[4] == 6 { 1 } else { 0 };
    if one + two + four >= 1 {
        println!("{}{}{}", one, two, four);
        let new_one = domination::domination_number_with_predominations(g, 1_u128);
        let new_two = domination::domination_number_with_predominations(g, 2_u128);
        let new_four = domination::domination_number_with_predominations(g, 4_u128);
        println!("{}{}{}", new_one, new_two, new_four);
    }
    //println!("Masked domination: {:?}", masked_domination);
    num_sixes >= 4 && masked_domination[7] >= 5
}