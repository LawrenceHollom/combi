use crate::pattern::*;

pub fn new(degree: &Degree) -> GraphWithVertices {
    let d = degree.to_usize();
    let n = Order::of_usize(d * 2);
    let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(n, &vec![]);
    adj_list[Vertex::ZERO].push(Vertex::of_usize(1));
    adj_list[Vertex::of_usize(1)].push(Vertex::ZERO);
    for i in 2..(d + 1) {
        adj_list[Vertex::of_usize(i)].push(Vertex::ZERO);
        adj_list[Vertex::ZERO].push(Vertex::of_usize(i));
    }
    for i in n.iter_verts().skip(d + 1) {
        adj_list[i].push(Vertex::of_usize(1));
        adj_list[Vertex::of_usize(1)].push(i);
    }
    GraphWithVertices {
        h: Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special),
        verts: n.iter_verts().skip(2).collect(),
        other_verts: VertexVec::new(Order::of_usize(1), &Vertex::ZERO),
        num_verts: 2 * (d - 1),
    }
}
