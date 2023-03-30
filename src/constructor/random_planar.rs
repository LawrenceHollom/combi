use rand::{thread_rng, Rng};
use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_triangulation(order: Order) -> Graph {
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

    let mut adj_list = VertexVec::new(order, &vec![]);
    for face in faces.iter() {
        for (i, j) in pairs.iter() {
            adj_list[Vertex::of_usize(face[*i])].push(Vertex::of_usize(face[*j]));
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(crate::constructor::RandomConstructor::Triangulation(order)))
}

pub fn new_maximal(order: Order) -> Graph {
    let n = order.to_usize();
    let mut rng = thread_rng();
    let mut next_vert_around_face = vec![n; n];
    let mut adj = vec![vec![false; n]; n];
    let mut adj_list = VertexVec::new(order, &vec![]);
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

    for i in 0..n {
        for j in 0..n {
            if adj[i][j] {
                adj_list[Vertex::of_usize(i)].push(Vertex::of_usize(j));
            }
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(crate::constructor::RandomConstructor::MaximalPlanar(order)))
}

pub fn new_conditioned(order: Order, max_deg: Option<Degree>, min_girth: Option<usize>) -> Graph {
    let mut g = new_maximal(order);
    if let Some(min_girth) = min_girth {
        for k in 3..min_girth {
            g.remove_all_k_cycles(k);
        }
    }
    if let Some(max_deg) = max_deg {
        g.reduce_max_degree(max_deg);
    }
    g
}

pub fn k_gon_gluing(order: Order, k: usize) -> Graph {
    let n = order.to_usize();
    let mut adj_list = VertexVec::new(order, &vec![]);
    let mut outer_face: Vec<Vertex> = (0..k).map(Vertex::of_usize).collect();
    let mut rng = thread_rng();
    let mut next_vert = Vertex::of_usize(k);
    let mut num_placed = k;
    fn add_cycle(adj_list: &mut VertexVec<Vec<Vertex>>, verts: &[Vertex], k: usize) {
        for i in 0..k {
            let j = (i + 1) % k;
            adj_list[verts[i]].push(verts[j]);
            adj_list[verts[j]].push(verts[i]);
        }
    }
    add_cycle(&mut adj_list, &outer_face, k);
    'place_verts: loop {
        if next_vert.is_n(order) && outer_face.len() < 2 * k {
            break 'place_verts;
        }
        // pick a random overlap length.
        // This will crash due to usize underflow.
        let min = if k + num_placed > n + 1 { k + num_placed - n } else { 2 };
        let overlap_len = rng.gen_range(min..=k.min(outer_face.len() - 1));
        // pick the interval to connect to
        let outer_len = outer_face.len();
        let interval_start = rng.gen_range(0..outer_len);
        let mut new_face: Vec<Vertex> = vec![];
        let mut new_outer_face: Vec<Vertex> = vec![];
        for i in 0..overlap_len {
            let pos = (i + interval_start) % outer_len;
            new_face.push(outer_face[pos]);
        }
        for i in 0..(outer_len - overlap_len + 2) {
            new_outer_face.push(outer_face[(outer_len + interval_start + overlap_len + i - 1) % outer_len]);
        }
        for _i in 0..(k - overlap_len) {
            new_face.push(next_vert);
            new_outer_face.push(next_vert);
            next_vert.incr_inplace();
        }
        //println!("Adding cycle {:?}; overlap_len: {}, interval_start: {}; outer_face: {:?}, new_outer_face: {:?}", new_face, overlap_len, interval_start, outer_face, new_outer_face);
        add_cycle(&mut adj_list, &new_face, k);
        num_placed += k - overlap_len;
        outer_face = new_outer_face;
    }
    let g = Graph::of_adj_list(adj_list, Constructor::Random(RandomConstructor::PlanarGons(order, k)));
    if !g.is_adj_commutative() {
        panic!("Not commutative!");
    }
    g
}