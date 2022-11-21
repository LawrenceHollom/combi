use utilities::*;
use crate::graph::*;

// "Does (u1, u2) ~ (v1, v2) in the product graph?"
fn generic_condition(product: &ProductConstructor, g1: &Graph, g2: &Graph, u1: usize, v1: usize, u2: usize, v2: usize) -> bool {
    use ProductConstructor::*;
    match product {
        Cartesian => (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2),
        Tensor => g1.adj[u1][v1] && g2.adj[u2][v2],
        Lex => g1.adj[u1][v1] || (u1 == v1 && g2.adj[u2][v2]),
        RevLex => g2.adj[u2][v2] || (u2 == v2 && g1.adj[u1][v1]),
        Strong => (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2) || (g1.adj[u1][v1] && g2.adj[u2][v2]),
        Conormal => g1.adj[u1][v1] || g2.adj[u2][v2],
        Rooted => (u1 == v1 && g2.adj[u2][v2]) || (u2 == 0 && v2 == 0 && g1.adj[u1][v1]),
        RevRooted => (u2 == v2 && g1.adj[u1][v1]) || (u1 == 0 && v1 == 0 && g2.adj[u2][v2]),
    }
}

pub fn new_product(product: &ProductConstructor, constructor: &Constructor, g1: &Graph, g2: &Graph) -> Graph {
    let n1 = g1.n.to_usize();
    let n = n1 * g2.n.to_usize();

    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut adj_list = vec![vec![]; n];

    for i in 0..(n-1) {
        for j in (i+1)..n {
            if generic_condition(product, g1, g2, i % n1, j % n1, i / n1, j / n1) {
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
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: constructor.to_owned()
    }
}