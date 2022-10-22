use utilities::*;
use crate::graph::*;

// condition(g1, g2, u1, v1, u2, v1) = "does (u1, u2) ~ (v1, v2)?"
fn new_product(g1: &Graph, g2: &Graph, condition: fn(g1: &Graph, g2: &Graph, 
        u1: usize, v1: usize, u2: usize, v2: usize) -> bool) -> Graph {
    let n1 = g1.n.to_usize();
    let n = n1 * g2.n.to_usize();

    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut adj_list = vec![vec![]; n];

    for i in 0..(n-1) {
        for j in (i+1)..n {
            if condition(g1, g2, i / n1, j / n1, i % n1, j % n1) {
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
        n: Order::of_usize(n), 
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect() 
    }
}

pub fn new_box(g1: &Graph, g2: &Graph) -> Graph {
    fn condition(g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
        (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2)
    }
    new_product(g1, g2, condition)
}

pub fn new_tensor(g1: &Graph, g2: &Graph) -> Graph {
    fn condition(g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
        g1.adj[u1][v1] && g2.adj[u2][v2]
    }
    new_product(g1, g2, condition)
}

pub fn new_lex(g1: &Graph, g2: &Graph) -> Graph {
    fn condition(g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
        g1.adj[u1][v1] || (u1 == v1 && g2.adj[u2][v2])
    }
    new_product(g1, g2, condition)
}

pub fn new_strong(g1: &Graph, g2: &Graph) -> Graph {
    fn condition(g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
        (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2) || (g1.adj[u1][v1] && g2.adj[u2][v2])
    }
    new_product(g1, g2, condition)
}

pub fn new_conormal(g1: &Graph, g2: &Graph) -> Graph {
    fn condition(g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
        g1.adj[u1][v1] || g2.adj[u2][v2]
    }
    new_product(g1, g2, condition)
}