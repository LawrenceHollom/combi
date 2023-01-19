use crate::graph::*;

fn of_adj_list(adj_list: Vec<Vec<usize>>, constructor: RawConstructor) -> Graph {
    let n: usize = adj_list.len();
    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];

    for i in 0..n {
        for j in adj_list[i].iter() {
            adj[i][*j] = true;
            deg[i] += 1;
        }
    }
    Graph {
        n: Order::of_usize(n),
        adj,
        adj_list,
        deg: deg.iter().map(|x| Degree::of_usize(*x)).collect(),
        constructor: Raw(constructor)
    }

}

pub fn new_fano_plane() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 5, 6], vec![0, 2, 3, 5, 6], vec![1, 3, 6], 
        vec![1, 2, 4, 5, 6], vec![3, 5, 6], vec![0, 1, 3, 4, 6], vec![0, 1, 2, 3, 4, 5]];
    of_adj_list(adj_list, FanoPlane)
}

pub fn new_petersen() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 4, 5], vec![0, 2, 6], vec![1, 3, 7], vec![2, 4, 8], vec![0, 3, 9],
        vec![0, 7, 8], vec![1, 8, 9], vec![2, 5, 9], vec![3, 5, 6], vec![4, 6, 7]];
    of_adj_list(adj_list, Petersen)
}

pub fn new_cube() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 3, 4], vec![0, 2, 5], vec![1, 3, 6], 
        vec![2, 0, 7], vec![0, 5, 7], vec![1, 4, 6], vec![2, 5, 7], vec![3, 4, 6]];
    of_adj_list(adj_list, Cube)
}

pub fn new_octahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4], vec![0, 2, 4, 5], vec![0, 1, 3, 5], 
        vec![0, 2, 4, 5], vec![0, 1, 3, 5], vec![1, 2, 3, 4]];
    of_adj_list(adj_list, Octahedron)
}

pub fn new_icosahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4, 5], vec![0, 2, 5, 6, 7], vec![0, 1, 3, 7, 8], 
        vec![0, 2, 4, 8, 9], vec![0, 3, 5, 9, 10], vec![0, 1, 4, 10, 6], vec![1, 5, 7, 10, 11],
        vec![1, 2, 6, 8, 11], vec![2, 3, 7, 9, 11], vec![3, 4, 8, 10, 11], vec![4, 5, 9, 6, 11], 
        vec![6, 7, 8, 9, 10]];
    of_adj_list(adj_list, Icosahedron)
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
    of_adj_list(adj_list, Dodecahedron)
}