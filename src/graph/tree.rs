use utilities::*;
use crate::graph::*;

pub fn new_rooted(parents: &Vec<usize>) -> Graph {
    let n = parents.len() + 1;

    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut adj_list = vec![vec![]; n];

    for (i, parent) in parents.iter().enumerate() {
        let u = i+1;
        let v = *parent;
        adj[u][v] = true;
        adj[v][u] = true;
        adj_list[u].push(v);
        adj_list[v].push(u);
        deg[u] += 1;
        deg[v] += 1;
    }
    
    Graph { 
        n: Order::of_usize(n), 
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::RootedTree(parents.to_owned())
    }
}