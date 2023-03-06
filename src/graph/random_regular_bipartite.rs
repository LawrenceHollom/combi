use utilities::*;
use crate::graph::*;

pub fn new_biregular(left_order: Order, left_deg: Degree, right_deg: Degree) -> Graph {
    // CR someday: if degree > order / 4 then generate the complement instead (needs less alternating paths)
    let left_n = left_order.to_usize();
    let left_d = left_deg.to_usize();
    let right_d = right_deg.to_usize();
    let right_n = (left_n * left_d) / right_d;
    let right_order = Order::of_usize(right_n);
    let n = Order::of_usize(left_n + right_n);
    if (left_n * left_d) % right_d != 0 {
        panic!("Degrees don't divide order properly!");
    }
    if left_n < right_d || right_n < left_d {
        panic!("One side is smaller than the degree of the other side!");
    }
    let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
    let mut deg = VertexVec::new(n, &Degree::ZERO);
    let mut rng = thread_rng();

    for _ in 0..(left_n * left_d) {
        // add an edge, flipping an alternating path if necessary
        let start = Vertex::of_usize(rng.gen_range(0..left_n));
        let mut u = start;
        'find_startpoint: loop {
            if deg[u] < left_deg {
                break 'find_startpoint;
            }
            u.incr_wrap_inplace(left_order);
            if u == start {
                break 'find_startpoint;
            }
        }
        let v_start = Vertex::of_usize(rng.gen_range(0..right_n));
        let mut v = v_start;
        let mut path_start = Vertex::ZERO;
        let mut edge_found = false;
        'find_endpoint: loop {
            if !adj[u][v.incr_by(left_n)] {
                path_start = v.incr_by(left_n);
                if deg[v.incr_by(left_n)] < right_deg {
                    edge_found = true;
                    break 'find_endpoint;
                }
            }
            v.incr_wrap_inplace(right_order);
            if v == v_start {
                break 'find_endpoint;
            }
        }
        v.incr_by(left_n);

        if edge_found {
            // there's somewhere we can connect to directly
            adj[u][v] = true;
            adj[v][u] = true;
            deg[u].incr_inplace();
            deg[v].incr_inplace();
        } else {
            // We need to construct an alternating path via flood-filling
            let mut flood: VertexVec<Option<Vertex>> = VertexVec::new(n, &None);
            let mut q: Queue<Vertex> = queue![];
            flood[u] = Some(u);
            flood[path_start] = Some(u);
            let _ = q.add(path_start);
            let mut path_end = Vertex::ZERO;

            'find_path: while q.size() > 0 {
                let x = q.remove().expect("wtf");
                if x.less_than(left_order) {
                    for y in n.iter_verts().skip(left_n) {
                        if !adj[x][y] && flood[y].is_none() {
                            flood[y] = Some(x);
                            let _ = q.add(y);
                            if deg[y] < right_deg {
                                // We can end the alternating path here
                                path_end = y;
                                break 'find_path;
                            }
                        }
                    }
                } else {
                    for (y, floodness) in flood.iter_mut_enum().take(left_n) {
                        if adj[x][y] && floodness.is_none() {
                            *floodness = Some(x);
                            let _ = q.add(y);
                        }
                    }
                }
            }

            // flip along the alternating path
            let mut node = path_end;

            while node != u {
                let prev = flood[node].unwrap();
                let sta: bool = adj[prev][node];
                adj[prev][node] = !sta;
                adj[node][prev] = !sta;
                node = prev;
            }
            deg[path_end].incr_inplace();
            deg[u].incr_inplace();
        }
    }

    let mut adj_list = VertexVec::new(n, &vec![]);
    for (i, j) in n.iter_pairs() {
        if adj[i][j] {
            adj_list[i].push(j);
            adj_list[j].push(i);
        }
    }
    Graph {
        n,
        adj,
        adj_list,
        deg,
        constructor: Constructor::Random(crate::constructor::RandomConstructor::Biregular(n, left_deg, right_deg))
    }
}