use crate::pattern::*;

pub fn new(degree: &Degree) -> GraphWithVertices {
    let d = degree.to_usize();
    let n = d * 2;
    let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];
    adj_list[0].push(1);
    adj_list[1].push(0);
    for i in 2..(d + 1) {
        adj_list[i].push(0);
        adj_list[0].push(i);
    }
    for i in (d + 1)..n {
        adj_list[i].push(1);
        adj_list[1].push(i);
    }
    GraphWithVertices {
        h: Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special),
        verts: (2..n).collect(),
        other_verts: vec![0, 1],
        num_verts: 2 * (d - 1),
    }
}