use rand::Rng;
use utilities::*;
use rand::{prelude::SliceRandom, thread_rng};

use super::*;
use super::Constructor::*;
use utilities::vertex_tools::*;

pub fn new_from_degree_sequence(deg_seq: &Vec<Degree>, is_regular: bool) -> Graph {
    let n = deg_seq.len();
    let mut sum = 0;
    for d in deg_seq.iter() {
        if d.at_least(n) {
            panic!("Order must be larger than max degree!");
        }
        sum += d.to_usize();
    }
    if sum % 2 == 1 {
        panic!("Sum of degrees must be even");
    }
    let mut rng = thread_rng();
    let mut pairings: Vec<usize> = vec![];
    for (i, d) in deg_seq.iter().enumerate() {
        for _j in 0..d.to_usize() {
            pairings.push(i);
        }
    }
    let mut adj_list: VertexVec<Vec<Vertex>>;

    'find_good_shuffle: loop {
        adj_list = VertexVec::new(Order::of_usize(n), &vec![]);
        pairings.shuffle(&mut rng);
        let mut is_good = true;
        'test_shuffle: for i in 0..(sum / 2) {
            let u = Vertex::of_usize(pairings[i]);
            let v = Vertex::of_usize(pairings[i + (sum / 2)]);
            if u == v || adj_list[u].contains(&v) {
                is_good = false;
                break 'test_shuffle;
            }
            adj_list[u].push(v);
            adj_list[v].push(u);
        }
        if is_good {
            break 'find_good_shuffle;
        }
    }

    let constructor = if is_regular {
        Random(RandomConstructor::Regular(Order::of_usize(n), deg_seq[0]))
    } else {
        Random(RandomConstructor::DegreeSequence(deg_seq.to_owned()))
    };
    Graph::of_adj_list(adj_list, constructor)
}

pub fn new_regular(order: &Order, degree: &Degree) -> Graph {
    let n = order.to_usize();
    let deg_seq = vec![*degree; n];
    self::new_from_degree_sequence(&deg_seq, true)
}

pub fn new_approximately_regular(n: &Order, degree: &Degree) -> Graph {
    let mut rng = thread_rng();
    let mut adj = VertexVec::new(*n, &VertexVec::new(*n, &false));
    let mut deg = VertexVec::new(*n, &Degree::ZERO);

    let mut subdeg_verts: Vec<Vertex> = n.iter_verts().collect();

    let mut attempts_since_success = 0;
    while attempts_since_success < 50 && subdeg_verts.len() > 1 {
        let l = subdeg_verts.len();
        let i1 = rng.gen_range(0..l);
        let mut i2 = rng.gen_range(0..(l-1));
        if i2 >= i1 {
            i2 += 1;
        }
        let x = subdeg_verts[i1];
        let y = subdeg_verts[i2];
        if x != y && !adj[x][y] {
            attempts_since_success = 0;
            adj[x][y] = true;
            adj[y][x] = true;
            deg[x].incr_inplace();
            deg[y].incr_inplace();
            for w in [x, y] {
                if deg[w] >= *degree {
                    subdeg_verts.swap_remove(subdeg_verts.iter().position(|z| *z == w).unwrap());
                }
            }
        } else {
            attempts_since_success += 1;
        }
    }

    Graph::of_matrix(adj, Random(RandomConstructor::RegularIsh(*n, *degree)))
}

pub fn new_hamilton_plus_matchings(n: Order, degree: Degree) -> Graph {
    let mut rng = thread_rng();
    let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
    let num_matchings = degree.to_usize() - 2;
    for v in n.iter_verts() {
        let u = v.incr_wrap(n);
        adj[u][v] = true;
        adj[v][u] = true;
    }

    let mut num_matchings_added = 0;
    let half_n = n.to_usize() / 2;
    'find_matchings: while num_matchings_added < num_matchings {
        let mut ordering = n.iter_verts().collect::<Vec<Vertex>>();
        ordering.shuffle(&mut rng);
        for i in 0..half_n {
            if adj[ordering[2 * i]][ordering[2 * i + 1]] {
                // Some edge has already been added.
                continue 'find_matchings
            }
        }

        // If we reach this point, then the matching is good.
        num_matchings_added += 1;
        for i in 0..half_n {
            let x = ordering[2 * i];
            let y = ordering[2 * i + 1];
            adj[x][y] = true;
            adj[y][x] = true;
        }
    }

    Graph::of_matrix(adj, Random(RandomConstructor::HamiltonPlusMatchings(n, degree)))
}