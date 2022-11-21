use utilities::*;
use crate::graph::*;

pub fn new(order: &Order, degree: &Degree) -> Graph {
    // CR someday: if degree > order / 4 then generate the complement instead (needs less alternating paths)
    let n = order.to_usize();
    let d = degree.to_usize();
    if n % 2 == 1 {
        panic!("n must be even!");
    }
    let part = n / 2;
    let mut adj = vec![vec![false; n]; n];
    let mut deg = vec![0; n];
    let mut rng = thread_rng();

    for _ in 0..(n * degree.to_usize() / 2) {
        // add an edge, flipping an alternating path if necessary
        let start = rng.gen_range(0..part);
        let mut u = (start + 1) % part;
        'find_startpoint: while u != start {
            if deg[u] < d {
                break 'find_startpoint;
            }
            u = (u + 1) % part;
        }
        let v_start: usize = rng.gen_range(0..part);
        let mut v = (v_start + 1) % part;
        let mut path_start = part;
        'find_endpoint: while v != v_start {
            if !adj[u][v + part] {
                path_start = v + part;
                if deg[v + part] < d {
                    break 'find_endpoint;
                }
            }
            v = (v + 1) % part;
        }
        v += part;

        if v != v_start + part {
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
                if x < part {
                    for y in part..n {
                        if !adj[x][y] && flood[y] == n {
                            flood[y] = x;
                            let _ = q.add(y);
                            if deg[y] < d {
                                // We can end the alternating path here
                                path_end = y;
                                break 'find_path;
                            }
                        }
                    }
                } else {
                    for (y, floodness) in flood.iter_mut().enumerate().take(part) {
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

    Graph {
        n: *order,
        adj,
        adj_list,
        deg: deg.iter().map(|d| Degree::of_usize(*d)).collect(),
        constructor: Constructor::Random(crate::constructor::RandomConstructor::RegularBipartite(*order, *degree))
    }
}