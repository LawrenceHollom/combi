use crate::graph::*;

use queues::*;

use utilities::polynomial::*;

pub fn percolate(g: &Graph) {
    let mut probs: Vec<Polynomial> = vec![];

    let p = Polynomial::of_vec(&vec![0, 1]);
    let one_minus_p = Polynomial::of_vec(&vec![1, -1]);
    for i in 0..(g.size()+1) {
        probs.push(p.pow(i).mul(&one_minus_p.pow(g.size() - i)));
    }

    let n = g.n.to_usize();
    let mut poly: Vec<Polynomial> = vec![(); n].iter().map(|_| Polynomial::new()).collect();

    for subset in 0..utilities::pow(2, g.size() as u64) {
        let mut edges = vec![false; g.size()];
        let mut num_edges = 0;
        let mut sta = subset;
        for j in 0..g.size() {
            if sta % 2 == 1 {
                edges[j] = true;
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

        let mut connected = vec![false; n];
        let mut q: Queue<usize> = queue![];
        connected[0] = true;
        let _ = q.add(0);
        while q.size() > 0 {
            let node = q.remove().unwrap();
            for v in true_adj_list[node].iter() {
                if !connected[*v] {
                    connected[*v] = true;
                    let _ = q.add(*v);
                }
            }
        }

        // This is probably horrifically inefficient due to making billions of 
        // new vecs. Cod rewriting with some kind of single mutable vec, or
        // add_inplace
        for v in 1..n {
            if connected[v] {
                poly[v] = poly[v].add(&probs[num_edges]);
            }
        }
    }

    g.print();

    for v in 1..n {
        println!("{}: {}", v, poly[v]);
    }
}