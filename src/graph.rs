extern crate utilities;
extern crate rand;
extern crate queues;

use utilities::*;
use crate::instruction::*;

use rand::{thread_rng, Rng};
use queues::*;

pub struct Graph {
    pub n: Order,
    pub adj: Vec<Vec<bool>>,
    pub adj_list: Vec<Vec<usize>>,
    pub deg: Vec<Degree>,
}

impl Graph {
    fn new_random_regular_bipartite(order: Order, degree: Degree) -> Graph {
        let n = order.to_usize();
        let d = degree.to_usize();
        if n % 2 == 1 {
            panic!("n must be even!");
        }
        let part = n / 2;
        let mut adj = vec![vec![false; n]; n];
        let mut deg = vec![0; n];
        let mut rng = thread_rng();

        for _ in 0..(n * degree.to_usize()) {
            // add an edge, flipping an alternating path if necessary
            let start = rng.gen_range(0..part);
            let mut u = (start + 1) % part;
            'find_startpoint: while u != start {
                if deg[u] < d {
                    break 'find_startpoint;
                }
                u += 1
            }
            let mut v: usize = part;
            let mut path_start = part;
            'find_endpoint: while v < n {
                if !adj[u][v] {
                    path_start = v;
                    if deg[v] < d {
                        break 'find_endpoint;
                    }
                }
                v += 1;
            }

            if v < n {
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
                q.add(path_start);
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
                        for y in 0..part {
                            if adj[x][y] && flood[y] == n {
                                flood[y] = x;
                                let _ = q.add(y);
                            }
                        }
                    }
                }

                // flip along the alternating path
                panic!("Need to flip along alternating path!");
            }
        }

        let mut adj_list = vec![vec![n-1]; n];
        panic!("Need to populate adj_list");

        Graph {
            n: order,
            adj,
            adj_list,
            deg: deg.iter().map(|d| Degree::of_usize(*d)).collect()
        }
    }

    fn new_complete(order: Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![n-1]; n];
        let deg: Vec<Degree> = vec![Degree::of_usize(n-1); n];

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..(n-1) {
            for j in (i+1)..n {
                adj[i][j] = true;
                adj[j][i] = true;
            }
        }

        for i in 0..n {
            for j in 0..n {
                if i != j {
                    adj_list[i].push(j);
                }
            }
        }

        Graph {
            n: order,
            adj,
            adj_list,
            deg,
        }
    }

    pub fn new(constructor: Constructor) -> Graph {
        match constructor {
            Constructor::RandomRegularBipartite(order, degree) =>
                Graph::new_random_regular_bipartite(order, degree),
            Constructor::Complete(order) =>
                Graph::new_complete(order),
        }
    }
}