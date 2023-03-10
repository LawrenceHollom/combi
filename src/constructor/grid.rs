use utilities::*;
use crate::graph::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new(height: &Order, width: &Order) -> Graph {
    let n = height.to_usize() * width.to_usize();
    let order = Order::of_usize(n);

    let mut adj_list = VertexVec::new(order, &vec![]);
    
    fn code(i: usize, j: usize, width: &Order) -> Vertex {
        Vertex::of_usize(i * width.to_usize() + j)
    }

    for i in 0..height.to_usize() {
        for j in 0..width.to_usize() {
            let v = code(i, j, width);
            if i > 0 {
                let u = code(i - 1, j, width);
                adj_list[v].push(u);
                adj_list[u].push(v);
            }
            if j > 0 {
                let u = code(i, j - 1, width);
                adj_list[v].push(u);
                adj_list[u].push(v);
            }
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Raw(crate::constructor::RawConstructor::Grid(*height, *width)))
}