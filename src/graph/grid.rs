use utilities::*;
use crate::graph::*;

pub fn new(height: &Order, width: &Order) -> Graph {
    let n = height.to_usize() * width.to_usize();

    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut adj_list = vec![vec![]; n];
    
    fn code(i: usize, j: usize, width: &Order) -> usize {
        i * width.to_usize() + j
    }

    for i in 0..height.to_usize() {
        for j in 0..width.to_usize() {
            let v = code(i, j, width);
            if i > 0 {
                let u = code(i - 1, j, width);
                adj[v][u] = true;
                adj[u][v] = true;
                deg[v] += 1;
                deg[u] += 1;
                adj_list[v].push(u);
                adj_list[u].push(v);
            }
            if j > 0 {
                let u = code(i, j - 1, width);
                adj[v][u] = true;
                adj[u][v] = true;
                deg[v] += 1;
                deg[u] += 1;
                adj_list[v].push(u);
                adj_list[u].push(v);
            }
        }
    }

    Graph { 
        n: Order::of_usize(n), 
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::Raw(crate::constructor::RawConstructor::Grid(*height, *width))
    }
}