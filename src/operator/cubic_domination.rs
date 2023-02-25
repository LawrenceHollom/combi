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
    let num_twos = twos.len();
    if num_twos >= 3 {
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
    }

    // Now test what happens to gamma with various combinations of the deg-two verts
    // already dominated
    let cap = 2_usize.pow(num_twos as u32);
    let mut masked_domination = vec![0; cap];
    let mut num_good = 0;
    let target_gamma = (n / 3) + 1;
    for mask in 0..cap {
        masked_domination[mask] = domination::domination_number_with_predominations(g, mask as u128);
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