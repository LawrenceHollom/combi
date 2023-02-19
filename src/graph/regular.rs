use utilities::*;
use crate::graph::*;
use rand::prelude::SliceRandom;

pub fn new_regular(order: &Order, degree: &Degree) -> Graph {
    let n = order.to_usize();
    let d = degree.to_usize();
    if d >= n {
        panic!("Order must be larger than degree!")
    } else if d % 2 == 1 && n % 2 == 1 {
        panic!("Cannot have order and degree both odd!")
    }
    let mut rng = thread_rng();
    let mut pairings: Vec<usize> = (0..d*n).map(|x| x / d).collect();
    let mut adj_list: Vec<Vec<usize>>;

    'find_good_shuffle: loop {
        pairings.shuffle(&mut rng);
        adj_list = vec![vec![]; n];
        let mut is_good = true;
        'test_shuffle: for i in 0..((d * n) / 2) {
            let u = pairings[i];
            let v = pairings[i + ((d * n) / 2)];
            if u == v || adj_list[u].contains(&v) {
                // Might fail due to pointer fuckery.
                is_good = false;
                break 'test_shuffle;
            }
            adj_list[u].push(v);
            adj_list[v].push(u);
        }
        if is_good {
            break 'find_good_shuffle;
        }
    }

    Graph::of_adj_list(adj_list, Random(RandomConstructor::Regular(*order, *degree)))
}