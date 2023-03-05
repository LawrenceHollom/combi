use utilities::*;
use crate::graph::*;

// "Does (u1, u2) ~ (v1, v2) in the product graph?"
fn generic_condition(product: &ProductConstructor, g1: &Graph, g2: &Graph, u1: Vertex, v1: Vertex, u2: Vertex, v2: Vertex) -> bool {
    use ProductConstructor::*;
    match product {
        Cartesian => (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2),
        Tensor => g1.adj[u1][v1] && g2.adj[u2][v2],
        Lex => g1.adj[u1][v1] || (u1 == v1 && g2.adj[u2][v2]),
        RevLex => g2.adj[u2][v2] || (u2 == v2 && g1.adj[u1][v1]),
        Strong => (u1 == v1 && g2.adj[u2][v2]) || (g1.adj[u1][v1] && u2 == v2) || (g1.adj[u1][v1] && g2.adj[u2][v2]),
        Conormal => g1.adj[u1][v1] || g2.adj[u2][v2],
        Rooted => (u1 == v1 && g2.adj[u2][v2]) || (u2.is_zero() && v2.is_zero() && g1.adj[u1][v1]),
        RevRooted => (u2 == v2 && g1.adj[u1][v1]) || (u1.is_zero() && v1.is_zero() && g2.adj[u2][v2]),
    }
}

pub fn new_product(product: &ProductConstructor, constructor: &Constructor, g1: &Graph, g2: &Graph) -> Graph {
    let n1 = g1.n.to_usize();
    let n = n1 * g2.n.to_usize();
    let order = Order::of_usize(n);

    let mut adj_list = VertexVec::new(order, &vec![]);

    for (i, j) in order.iter_pairs() {
        if generic_condition(product, g1, g2, i.rem(n1), j.rem(n1), i.div(n1), j.div(n1)) {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }

    Graph::of_adj_list(adj_list, constructor.to_owned())
}