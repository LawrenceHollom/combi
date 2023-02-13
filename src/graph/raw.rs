use crate::graph::*;

pub fn new_cube(dimension: usize) -> Graph {
    let n = 2_u32.pow(dimension as u32) as usize;
    let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];

    for i in 0..(n-1) {
        for j in (i+1)..n {
            let mut delta = i ^ j;
            while delta % 2 == 0 {
                delta /= 2;
            }
            if delta == 1 {
                // They are adjacent in the cube.
                adj_list[i].push(j);
                adj_list[j].push(i);
            }
        }
    }
    
    Graph::of_adj_list(adj_list, Raw(Cube(dimension)))
}

pub fn new_fano_plane() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 5, 6], vec![0, 2, 3, 5, 6], vec![1, 3, 6], 
        vec![1, 2, 4, 5, 6], vec![3, 5, 6], vec![0, 1, 3, 4, 6], vec![0, 1, 2, 3, 4, 5]];
    Graph::of_adj_list(adj_list, Raw(FanoPlane))
}

pub fn new_petersen() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 4, 5], vec![0, 2, 6], vec![1, 3, 7], vec![2, 4, 8], vec![0, 3, 9],
        vec![0, 7, 8], vec![1, 8, 9], vec![2, 5, 9], vec![3, 5, 6], vec![4, 6, 7]];
    Graph::of_adj_list(adj_list, Raw(Petersen))
}

pub fn new_octahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4], vec![0, 2, 4, 5], vec![0, 1, 3, 5], 
        vec![0, 2, 4, 5], vec![0, 1, 3, 5], vec![1, 2, 3, 4]];
    Graph::of_adj_list(adj_list, Raw(Octahedron))
}

pub fn new_icosahedron() -> Graph {
    let adj_list: Vec<Vec<usize>> = vec![vec![1, 2, 3, 4, 5], vec![0, 2, 5, 6, 7], vec![0, 1, 3, 7, 8], 
        vec![0, 2, 4, 8, 9], vec![0, 3, 5, 9, 10], vec![0, 1, 4, 10, 6], vec![1, 5, 7, 10, 11],
        vec![1, 2, 6, 8, 11], vec![2, 3, 7, 9, 11], vec![3, 4, 8, 10, 11], vec![4, 5, 9, 6, 11], 
        vec![6, 7, 8, 9, 10]];
    Graph::of_adj_list(adj_list, Raw(Icosahedron))
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
    Graph::of_adj_list(adj_list, Raw(Dodecahedron))
}