use crate::graph::*;

pub fn new_cube(dimension: usize) -> Graph {
    let n = 2_u32.pow(dimension as u32) as usize;
    let order = Order::of_usize(n);
    let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);

    for (i, j) in order.iter_pairs() {
        let mut delta = i.xor(j);
        while delta.rem(2).is_zero() {
            delta.div_inplace(2);
        }
        if delta == Vertex::of_usize(1) {
            // They are adjacent in the cube.
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }
    
    Graph::of_adj_list(adj_list, Raw(Cube(dimension)))
}

pub fn new_fano_plane() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 5, 6], vec![0, 2, 3, 5, 6], vec![1, 3, 6], 
        vec![1, 2, 4, 5, 6], vec![3, 5, 6], vec![0, 1, 3, 4, 6], vec![0, 1, 2, 3, 4, 5]];
    Graph::of_adj_list(adj_list_of_manual(adj_list), Raw(FanoPlane))
}

pub fn new_petersen(cycles: usize, skip: usize) -> Graph {
    let order = Order::of_usize(2 * cycles);
    let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);
    for i in 0..cycles {
        let u = Vertex::of_usize(i);
        let v = Vertex::of_usize((i + 1) % cycles);
        let w = Vertex::of_usize(((i + skip) % cycles) + cycles);
        let x = Vertex::of_usize(i + cycles);
        adj_list[u].push(v);
        adj_list[v].push(u);
        adj_list[u].push(x);
        adj_list[x].push(u);
        adj_list[x].push(w);
        adj_list[w].push(x);

    }
    Graph::of_adj_list(adj_list, Raw(Petersen(cycles, skip)))
}

pub fn new_octahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4], vec![0, 2, 4, 5], vec![0, 1, 3, 5], 
        vec![0, 2, 4, 5], vec![0, 1, 3, 5], vec![1, 2, 3, 4]];
    Graph::of_adj_list(adj_list_of_manual(adj_list), Raw(Octahedron))
}

pub fn new_icosahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4, 5], vec![0, 2, 5, 6, 7], vec![0, 1, 3, 7, 8], 
        vec![0, 2, 4, 8, 9], vec![0, 3, 5, 9, 10], vec![0, 1, 4, 10, 6], vec![1, 5, 7, 10, 11],
        vec![1, 2, 6, 8, 11], vec![2, 3, 7, 9, 11], vec![3, 4, 8, 10, 11], vec![4, 5, 9, 6, 11], 
        vec![6, 7, 8, 9, 10]];
    Graph::of_adj_list(adj_list_of_manual(adj_list), Raw(Icosahedron))
}

pub fn new_dodecahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![
        vec![2,3,4],
        vec![5,6,7],
        vec![0,8,9],
        vec![0,10,12],
        vec![0,11,13],
        vec![1,14,15],
        vec![1,16,18],
        vec![1,17,19],
        vec![2,10,17],
        vec![2,11,16],
        vec![3,8,19],
        vec![4,9,18],
        vec![3,13,14],
        vec![4,12,15],
        vec![5,12,19],
        vec![5,13,18],
        vec![6,9,17],
        vec![7,8,16],
        vec![6,11,15],
        vec![7,10,14],
    ];
    Graph::of_adj_list(adj_list_of_manual(adj_list), Raw(Dodecahedron))
}