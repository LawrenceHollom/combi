use crate::pattern::*;

pub fn new() -> GraphWithEdges {
    let adj_list = vec![vec![1,2,3],vec![0,5,6],vec![0,7,8],
        vec![0,4,9],vec![3,5],vec![1,4],vec![1,7],vec![2,6],vec![2,9],vec![3,8]];
    GraphWithEdges { 
        h: Graph::of_adj_list(crate::entity::graph::adj_list_of_manual(adj_list), 
            crate::constructor::Constructor::Special),
        edges: [(4,5),(6,7),(8,9)].iter().map(|(x, y)| 
            Edge::of_pair(Vertex::of_usize(*x), Vertex::of_usize(*y))).collect(), 
        verts_not_in_edge: VertexVec::new_fn(Order::of_usize(4), |v| v),
        num_edges: 3 
    }
}