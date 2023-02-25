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
        println!("Is not well-connected!");
        return false;
    }
    println!("Is well-connected!");

    // Now rip out vertices of degree 2 and see what happens to gamma.
    new_adj_list = vec![vec![]; n-twos.len()];
    let mut map = vec![0; n];
    let mut sub = 0;
    for i in 0..n {
        map[i] = i - sub;
        if twos.contains(&i) {
            sub += 1;
        }
    }
    for u in 0..n {
        if !twos.contains(&u) {
            for v in g.adj_list[u].iter() {
                if !twos.contains(v) {
                    new_adj_list[map[u]].push(map[*v]);
                }
            }
        }
    }
    let k = Graph::of_adj_list(new_adj_list, crate::constructor::Constructor::Special);
    let gamma = domination::domination_number(g);
    let new_gamma = domination::domination_number(&k);
    println!("Old gamma: {}, new gamma: {}", gamma, new_gamma);
    gamma == new_gamma
}