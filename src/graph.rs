use utilities::*;
use crate::constructor::*;

use rand::{thread_rng, Rng};
use queues::*;

mod products;
mod erdos_renyi;
mod grid;
mod random_regular_bipartite;
mod tree;

pub struct Graph {
    pub n: Order,
    pub adj: Vec<Vec<bool>>,
    pub adj_list: Vec<Vec<usize>>,
    pub deg: Vec<Degree>,
}

impl Graph {
    fn new_complete(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
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

    fn new_cyclic(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = {
            if n == 1 { vec![0; n] }
            else if n == 2 { vec![1; n] }
            else { vec![2; n]}
        }.iter().map(|x| Degree::of_usize(*x)).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..n {
            adj[i][(i + 1) % n] = true;
            adj[i][(i + n - 1) % n] = true;
            adj_list[i].push((i + 1) % n);
            adj_list[i].push((i + n - 1) % n);
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
        }
    }

    fn new_path(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = vec![2; n].iter().map(|x| Degree::of_usize(*x)).collect();

        // This could be done directly and non-mutably; possibly would be more idiomatic
        for i in 0..n {
            if i + 1 < n {
                adj[i][i + 1] = true;
                adj_list[i].push(i + 1);
            }
            if i >= 1 {
                adj[i][i - 1] = true;
                adj_list[i].push(i - 1);
            }
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
        }
    }

    fn new_star(order: &Order) -> Graph {
        let n = order.to_usize();
        let mut adj = vec![vec![false; n]; n];
        let mut adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = (0..n).map(|x| Degree::of_usize(if x == 0 { n - 1 } else { 1 })).collect();

        for i in 1..n {
            adj[0][i] = true;
            adj[i][0] = true;
            adj_list[0].push(i);
            adj_list[i].push(0);
        }

        Graph {
            n: *order,
            adj,
            adj_list,
            deg,
        }
    }

    fn new_empty(order: &Order) -> Graph {
        let n = order.to_usize();
        let adj = vec![vec![false; n]; n];
        let adj_list = vec![vec![]; n];
        let deg: Vec<Degree> = vec![0; n].iter().map(|x| Degree::of_usize(*x)).collect();
        
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

    pub fn bunkbed(&self) -> Graph {
        let n = self.n.to_usize();
        let mut adj = vec![vec![false; 2*n]; 2*n];
        let mut adj_list = vec![vec![]; 2*n];
        let mut deg: Vec<Degree> = vec![];

        for j in 0..2_usize {
            for i in 0..n {
                deg.push(self.deg[i].incr());
                adj[i + j * n][i + (1 - j) * n] = true; //post
                adj_list[i + j * n].push(i + (1 - j) * n);
                for k in 0..n {
                    if self.adj[i][k] {
                        adj[i + j * n][k + j * n] = true;
                        adj_list[i + j * n].push(k + j * n);
                    }
                }
            }
        }

        Graph {
            n: Order::of_usize(2*n),
            adj,
            adj_list,
            deg
        }
    }

    pub fn new(constructor: &Constructor) -> Graph {
        use Constructor::*;
        use RandomConstructor::*;
        use RawConstructor::*;

        match constructor {
            Product(product, c1, c2) => {
                products::new_product(product, &Self::new(c1), &Self::new(c2))
            }
            RootedTree(parents) => tree::new_rooted(parents),
            Random(RegularBipartite(order, degree)) => random_regular_bipartite::new(order, degree),
            Random(ErdosRenyi(order, p)) => erdos_renyi::new(order, *p),
            Raw(Grid(height, width)) => grid::new(height, width),
            Raw(Complete(order)) => Graph::new_complete(order),
            Raw(Cyclic(order)) => Graph::new_cyclic(order),
            Raw(Path(order)) => Graph::new_path(order),
            Raw(Star(order)) => Graph::new_star(order),
            Raw(Empty(order)) => Graph::new_empty(order),
            Raw(FanoPlane) => Graph::new_fano_plane(),
            Raw(Petersen) => Graph::new_petersen(),
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

    pub fn size(&self) -> usize {
        self.deg.iter().fold(0, |accum, val| accum + val.to_usize()) / 2
    }
}