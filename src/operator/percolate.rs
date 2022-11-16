use crate::graph::*;
use rand::{Rng, thread_rng};

use std::io::*;
use queues::*;

use utilities::polynomial::*;

pub struct Percolator {
    order: usize,
    probs: Vec<Polynomial>,
    pub polys: Vec<Polynomial>,
}

impl Percolator {
    pub fn new(order: usize, size: usize) -> Percolator {
        let mut probs: Vec<Polynomial> = vec![];

        let p = Polynomial::of_vec(&vec![0, 1]);
        let one_minus_p = Polynomial::of_vec(&vec![1, -1]);
        for i in 0..(size+1) {
            probs.push(p.pow(i).mul(&one_minus_p.pow(size - i)));
        }
        let mut polys: Vec<Polynomial> = vec![(); order].iter().map(|_| Polynomial::new()).collect();
        polys[0] = Polynomial::of_vec(&vec![1]);

        Percolator {
            order,
            probs,
            polys
        }
    }

    fn test_connections(order: usize, adj_list: &[Vec<usize>]) -> Vec<bool> {
        let mut connected = vec![false; order];
        let mut q: Queue<usize> = queue![];
        connected[0] = true;
        let _ = q.add(0);
        while q.size() > 0 {
            let node = q.remove().unwrap();
            for v in adj_list[node].iter() {
                if !connected[*v] {
                    connected[*v] = true;
                    let _ = q.add(*v);
                }
            }
        }
        connected
    }

    pub fn add_percolation(&mut self, num_edges: usize, adj_list: &[Vec<usize>]) {
        let connected = Self::test_connections(self.order, adj_list);

        for (v, is_connected) in connected.iter().enumerate().skip(1) {
            if *is_connected {
                self.polys[v].add_inplace(&self.probs[num_edges]);
            }
        }
    }

    pub fn empirically_percolate_once(g: &Graph, p: f64) -> Vec<bool> {
        let n = g.n.to_usize();
        let mut rng = thread_rng();

        let mut true_adj_list: Vec<Vec<usize>> = vec![vec![0; n]; n];
        
        for i in 0..(n-1) {
            for j in (i+1)..n {
                if g.adj[i][j] && rng.gen_bool(p) {
                    true_adj_list[i].push(j);
                    true_adj_list[j].push(i);
                }
            }
        }
        Self::test_connections(n, &true_adj_list)
    }

    pub fn percolate(g: &Graph) -> Percolator {
        let n = g.n.to_usize();
        let mut percolator = Percolator::new(n, g.size());

        let loop_max = utilities::pow(2, g.size() as u64);
        println!("Starting the big loop! size: {}", loop_max);
        let mut next_alert = 1;
        for subset in 0..loop_max {
            if (100 * subset) / loop_max >= next_alert {
                print!("{}% ", next_alert);
                std::io::stdout().flush().unwrap();
                next_alert += 1;
            }
            let mut edges = vec![false; g.size()];
            let mut num_edges = 0;
            let mut sta = subset;
            for edge in edges.iter_mut() {
                if sta % 2 == 1 {
                    *edge = true;
                    num_edges += 1;
                }
                sta /= 2;
            }

            let mut true_adj_list: Vec<Vec<usize>> = vec![vec![0; n]; n];
            let mut edge_index = 0;
            for i in 0..(n-1) {
                for j in (i+1)..n {
                    if g.adj[i][j] {
                        if edges[edge_index] {
                            true_adj_list[i].push(j);
                            true_adj_list[j].push(i);
                        }
                        edge_index += 1;
                    }
                }
            }
            percolator.add_percolation(num_edges, &true_adj_list);
        }

        g.print();
        percolator
    }
}