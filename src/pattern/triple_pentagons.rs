use crate::pattern::*;

pub fn new() -> GraphWithEdges {
    let adj_list = vec![vec![1,2,3],vec![0,5,6],vec![0,7,8],
        vec![0,4,9],vec![3,5],vec![1,4],vec![1,7],vec![2,6],vec![2,9],vec![3,8]];
    let h = Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special);
    GraphWithEdges { 
        h, 
        edges: vec![(4,5),(6,7),(8,9)], 
        verts_not_in_edge: vec![0, 1, 2, 3],
        num_edges: 3 
    }
}