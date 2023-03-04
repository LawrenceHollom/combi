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

pub fn new_conditioned(order: &Order, max_deg: Option<Degree>, min_girth: Option<usize>) -> Graph {
    let mut g = new_maximal(order);
    match min_girth {
        Some(girth) => {
            for k in 3..girth {
                g.remove_all_k_cycles(k);
            }
        }
        None => (),
    }
    match max_deg {
        Some(max_deg) => {
            g.reduce_max_degree(max_deg);
        }
        None => ()
    }
    g
}

pub fn k_gon_gluing(order: &Order, k: usize) -> Graph {
    let n = order.to_usize();
    let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];
    let mut outer_face: Vec<usize> = (0..k).collect();
    let mut rng = thread_rng();
    let mut num_placed = k;
    fn add_cycle(adj_list: &mut Vec<Vec<usize>>, verts: &Vec<usize>, k: usize) {
        for i in 0..k {
            let j = (i + 1) % k;
            adj_list[verts[i]].push(verts[j]);
            adj_list[verts[j]].push(verts[i]);
        }
    }
    add_cycle(&mut adj_list, &(0..k).collect(), k);
    'place_verts: loop {
        if num_placed == n && outer_face.len() < 2 * k {
            break 'place_verts;
        }
        // pick a random overlap length.
        // This will crash due to usize underflow.
        let min = if k + num_placed > n + 1 { k + num_placed - n } else { 2 };
        let overlap_len = rng.gen_range(min..=k.min(outer_face.len() - 1));
        // pick the interval to connect to
        let outer_len = outer_face.len();
        let interval_start = rng.gen_range(0..outer_len);
        let mut new_face: Vec<usize> = vec![];
        let mut new_outer_face: Vec<usize> = vec![];
        for i in 0..overlap_len {
            let pos = (i + interval_start) % outer_len;
            new_face.push(outer_face[pos]);
        }
        for i in 0..(outer_len - overlap_len + 2) {
            new_outer_face.push(outer_face[(outer_len + interval_start + overlap_len + i - 1) % outer_len]);
        }
        for i in 0..(k - overlap_len) {
            new_face.push(num_placed + i);
            new_outer_face.push(num_placed + i);
        }
        //println!("Adding cycle {:?}; overlap_len: {}, interval_start: {}; outer_face: {:?}, new_outer_face: {:?}", new_face, overlap_len, interval_start, outer_face, new_outer_face);
        add_cycle(&mut adj_list, &new_face, k);
        num_placed += k - overlap_len;
        outer_face = new_outer_face;
    }
    let g = Graph::of_adj_list(adj_list, Random(RandomConstructor::PlanarGons(*order, k)));
    if !g.is_adj_commutative() {
        panic!("Not commutative!");
    }
    g
}