use utilities::*;
use crate::graph::*;

pub fn new(order: &Order, p: f64) -> Graph {
    let n = order.to_usize();
    
    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut adj_list = vec![vec![]; n];
    let mut rng = thread_rng();

    for i in 0..(n-1) {
        for j in (i+1)..n {
            if rng.gen_bool(p) {
                adj[i][j] = true;
                adj[j][i] = true;
                deg[i] += 1;
                deg[j] += 1;
                adj_list[i].push(j);
                adj_list[j].push(i);
            }
        }
    }

    Graph { 
        n: *order,
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect()
    }
}