use utilities::*;
use crate::graph::*;

pub fn new_biregular(left_order: &Order, left_deg: &Degree, right_deg: &Degree) -> Graph {
    // CR someday: if degree > order / 4 then generate the complement instead (needs less alternating paths)
    let left_n = left_order.to_usize();
    let left_d = left_deg.to_usize();
    let right_d = right_deg.to_usize();
    let right_n = (left_n * left_d) / right_d;
    let n = left_n + right_n;
    if (left_n * left_d) % right_d != 0 {
        panic!("Degrees don't divide order properly!");
    }
    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut rng = thread_rng();

    for _ in 0..(left_n * left_d) {
        // add an edge, flipping an alternating path if necessary
        let start = rng.gen_range(0..left_n);
        let mut u = start;
        'find_startpoint: loop {
            if deg[u] < left_d {
                break 'find_startpoint;
            }
            u = (u + 1) % left_n;
            if u == start {
                break 'find_startpoint;
            }
        }
        let v_start: usize = rng.gen_range(0..right_n);
        let mut v = v_start;
        let mut path_start = left_n;
        let mut edge_found = false;
        'find_endpoint: loop {
            if !adj[u][v + left_n] {
                path_start = v + left_n;
                if deg[v + left_n] < right_d {
                    edge_found = true;
                    break 'find_endpoint;
                }
            }
            v = (v + 1) % right_n;
            if v == v_start {
                break 'find_endpoint;
            }
        }
        v += left_n;

        if edge_found {
            // there's somewhere we can connect to directly
            adj[u][v] = true;
            adj[v][u] = true;
            deg[u] += 1;
            deg[v] += 1;
        } else {
            // We need to construct an alternating path via flood-filling
            let mut flood: Vec<usize> = vec![n; n];
            let mut q: Queue<usize> = queue![];
            flood[u] = u;
            flood[path_start] = u;
            let _ = q.add(path_start);
            let mut path_end = n;

            'find_path: while q.size() > 0 {
                let x = q.remove().expect("wtf");
                if x < left_n {
                    for y in left_n..n {
                        if !adj[x][y] && flood[y] == n {
                            flood[y] = x;
                            let _ = q.add(y);
                            if deg[y] < right_d {
                                // We can end the alternating path here
                                path_end = y;
                                break 'find_path;
                            }
                        }
                    }
                } else {
                    for (y, floodness) in flood.iter_mut().enumerate().take(left_n) {
                        if adj[x][y] && *floodness == n {
                            *floodness = x;
                            let _ = q.add(y);
                        }
                    }
                }
            }

            // flip along the alternating path
            let mut node = path_end;

            while node != u {
                let sta: bool = adj[flood[node]][node];
                adj[flood[node]][node] = !sta;
                adj[node][flood[node]] = !sta;
                node = flood[node];
            }
            deg[path_end] += 1;
            deg[u] += 1;
        }
    }

    let mut adj_list = vec![vec![]; n];
    for i in 0..n {
        for j in 0..n {
            if adj[i][j] {
                adj_list[i].push(j);
            }
        }
    }

    let order = Order::of_usize(n);
    Graph {
        n: order,
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::Random(crate::constructor::RandomConstructor::Biregular(order, *left_deg, *right_deg))
    }
}