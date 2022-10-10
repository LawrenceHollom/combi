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
    fn new_random_regular_bipartite(order: &Order, degree: &Degree) -> Graph {
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
            deg: deg.iter().map(|d| Degree::of_usize(*d)).collect()
        }
    }

    fn new_erdos_renyi(order: &Order, p: f64) -> Graph {
        let n = order.to_usize();
        
        let mut adj = vec![vec![false; n]; n];
        let mut deg = vec![0; n];
        let mut adj_list = vec![vec![]; n];
        let mut rng = thread_rng();

        for i in 0..(n-1) {
            for j in (i+1)..n {
                if rng.gen_bool(p) {
                    adj[i][j] = true;
                    adj[j][i] = true;
                    deg[i] += 1;
                    deg[j] += 1;
                    adj_list[i].push(j);
                    adj_list[j].push(i);
                }
            }
        }

        Graph { 
            n: *order,
            adj,
            adj_list,
            deg: deg.iter().map(|d| Degree::of_usize(*d)).collect()
        }
    }

    fn new_complete(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![n-1]; n];
        let deg: Vec<Degree> = vec![n-1; n].iter().map(|x| Degree::of_usize(*x)).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..(n-1) {
            for j in (i+1)..n {
                adj[i][j] = true;
                adj[j][i] = true;
            }
        }

        for (i, nbrs) in adj_list.iter_mut().enumerate() {
            for j in 0..n {
                if i != j {
                    nbrs.push(j);
                }
            }
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
        }
    }

    fn new_fano_plane() -> Graph {
        let adj_list: Vec<Vec<usize>> = vec![vec![1, 5, 6], vec![0, 2, 3, 5, 6], vec![1, 3, 6], 
            vec![1, 2, 4, 5, 6], vec![3, 5, 6], vec![0, 1, 3, 4, 6], vec![0, 1, 2, 3, 4, 5]];
        let n: usize = 7;
        let mut adj = vec![vec![false; n]; n];
        for i in 0..n {
            for j in adj_list[i].iter() {
                adj[i][*j] = true;
            }
        }
        let deg = vec![3, 5, 3, 5, 3, 5, 6].iter().map(|x| Degree::of_usize(*x)).collect();
        Graph {
            n: Order::of_usize(n),
            adj,
            adj_list,
            deg
        }
    }

    fn new_petersen() -> Graph {
        let adj_list: Vec<Vec<usize>> = vec![vec![1, 4, 5], vec![0, 2, 6], vec![1, 3, 7], vec![2, 4, 8], vec![0, 3, 9],
            vec![0, 7, 8], vec![1, 8, 9], vec![2, 5, 9], vec![3, 5, 6], vec![4, 6, 7]];
        let n: usize = 10;
        let mut adj = vec![vec![false; n]; n];
        for i in 0..n {
            for j in adj_list[i].iter() {
                adj[i][*j] = true;
            }
        }
        let deg = vec![3; n].iter().map(|x| Degree::of_usize(*x)).collect();
        Graph {
            n: Order::of_usize(n),
            adj,
            adj_list,
            deg
        }
    }

    pub fn new(constructor: &Constructor) -> Graph {
        match constructor {
            Constructor::RandomRegularBipartite(order, degree) => {
                Graph::new_random_regular_bipartite(order, degree)
            },
            Constructor::ErdosRenyi(order, p) => {
                Graph::new_erdos_renyi(order, *p)
            },
            Constructor::Complete(order) => Graph::new_complete(order),
            Constructor::FanoPlane => Graph::new_fano_plane(),
            Constructor::Petersen => Graph::new_petersen(),
        }
    }

    pub fn complement(&self) -> Graph {
        let n = self.n.to_usize();
        let mut new_adj = vec![vec![false; n]; n];
        let mut new_adj_list = vec![vec![n-1]; n];
        let new_deg: Vec<Degree> = self.deg.iter().map(|x| Degree::of_usize(n - x.to_usize() - 1)).collect();

        for i in 0..(n-1) {
            for j in (i+1)..n {
                new_adj[i][j] = !self.adj[i][j];
                new_adj[j][i] = !self.adj[j][i];
            }
        }
        
        for i in 0..n {
            for j in 0..n {
                if new_adj[i][j] {
                    new_adj_list[i].push(j);
                }
            }
        }

        Graph {
            n: self.n,
            adj: new_adj,
            adj_list: new_adj_list,
            deg: new_deg,
        }
    }

    pub fn print(&self) {
        let n: usize = self.n.to_usize();
        for i in 0..(n-1) {
            for j in (i+1)..n {
                if self.adj[i][j] {
                    println!("{} ~ {}", i, j);
                }
            }
        }
    }
}