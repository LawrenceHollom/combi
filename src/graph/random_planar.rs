use utilities::*;
use crate::graph::*;

pub fn new_triangulation(order: &Order) -> Graph {
    let n = order.to_usize();
    let mut rng = thread_rng();
    let mut faces = vec![[0, 1, 2], [0, 1, 3], [0, 2, 3], [1, 2, 3]];
    let pairs = vec![(0,1), (1,0), (0,2), (2,0), (1,2), (2,1)];

    for v in 4..n {
        // Add a new vertex
        let i = rng.gen_range(0..faces.len());
        let old_face = faces[i];
        faces[i] = [old_face[0], old_face[1], v];
        faces.push([old_face[1], old_face[2], v]);
        faces.push([old_face[2], old_face[0], v]);
    }

    let mut adj = vec![vec![false; n]; n];
    for face in faces.iter() {
        for (i, j) in pairs.iter() {
            adj[face[*i]][face[*j]] = true;
        }
    }

    let mut adj_list = vec![vec![]; n];
    let mut deg = vec![0; n];
    for i in 0..n {
        for j in 0..n {
            if adj[i][j] {
                adj_list[i].push(j);
                deg[i] += 1;
            }
        }
    }

    Graph {
        n: *order,
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::Random(crate::constructor::RandomConstructor::Triangulation(*order))
    }
}

pub fn new_maximal(order: &Order) -> Graph {
    let n = order.to_usize();
    let mut rng = thread_rng();
    let mut next_vert_around_face = vec![n; n];
    let mut adj = vec![vec![false; n]; n];
    let pairs = vec![(0,1), (1,0), (0,2), (2,0), (1,2), (2,1)];

    for (i, j) in pairs.iter() {
        adj[*i][*j] = true;
    }
    next_vert_around_face[0] = 1;
    next_vert_around_face[1] = 2;
    next_vert_around_face[2] = 0;

    for v in 3..n {
        // Add the vertex v to the outerplanar triangulation.
        let u = rng.gen_range(0..v);
        let w = next_vert_around_face[u];
        next_vert_around_face[v] = w;
        next_vert_around_face[u] = v;
        adj[u][v] = true;
        adj[v][u] = true;
        adj[w][v] = true;
        adj[v][w] = true;
    }

    // We now need to add n-3 more edges to actually make it maximal.
    for _ in 0..(n-3) {
        let mut u = rng.gen_range(0..n);
        while next_vert_around_face[u] == n || adj[u][next_vert_around_face[next_vert_around_face[u]]] {
            u = (u + 1) % n;
        }
        let v = next_vert_around_face[u];
        let w = next_vert_around_face[v];
        next_vert_around_face[u] = w;
        next_vert_around_face[v] = n;
        adj[u][w] = true;
        adj[w][u] = true;
    }

    let mut adj_list = vec![vec![]; n];
    let mut deg = vec![0; n];
    for i in 0..n {
        for j in 0..n {
            if adj[i][j] {
                adj_list[i].push(j);
                deg[i] += 1;
            }
        }
    }

    Graph {
        n: *order,
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::Random(crate::constructor::RandomConstructor::MaximalPlanar(*order))
    }
}