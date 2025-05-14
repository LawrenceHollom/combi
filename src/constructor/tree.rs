use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_rooted(parents: &Vec<usize>) -> Graph {
    let n = Order::of_usize(parents.len() + 1);

    let mut adj_list = VertexVec::new(n, &vec![]);

    for (i, parent) in parents.iter().enumerate() {
        let u = Vertex::of_usize(i + 1);
        let v = Vertex::of_usize(*parent);
        adj_list[u].push(v);
        adj_list[v].push(u);
    }

    Graph::of_adj_list(adj_list, Constructor::RootedTree(parents.to_owned()))
}
