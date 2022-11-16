use crate::graph::*;
use crate::operator::percolate::*;

use utilities::polynomial::*;

pub fn print_polynomials(g: &Graph) {
    let n = g.n.to_usize();
    let mut percolator = Percolator::new(2*n, g.size());

    let posts = vec![false, true, false, false];

    for subset in 0..utilities::pow(2, g.size() as u64) {
        let mut up_edges = vec![false; g.size()];
        let mut num_down_edges = 0;
        let mut sta = subset;
        for up_edge in up_edges.iter_mut().take(g.size()) {
            if sta % 2 == 1 {
                *up_edge = true;
            } else {
                num_down_edges += 1;
            }
            sta /= 2;
        }

        let mut adj_list: Vec<Vec<usize>> = vec![vec![0; 2*n]; 2*n];
        let mut edge_index = 0;
        for i in 0..(n-1) {
            for j in (i+1)..n {
                if g.adj[i][j] {
                    if up_edges[edge_index] {
                        adj_list[n + i].push(n + j);
                        adj_list[n + j].push(n + i);
                    } else {
                        adj_list[i].push(j);
                        adj_list[j].push(i);
                    }
                    edge_index += 1;
                }
            }
        }

        for i in 0..n {
            if posts[i] {
                adj_list[i].push(n + i);
                adj_list[n + i].push(i);
            }
        }

        percolator.add_percolation(num_down_edges, &adj_list);
    }

    g.print();

    for v in 0..(2*n) {
        println!("{}: {}", v, percolator.polys[v]);
    }

    println!("Differences (same + same - cross - cross):");
    for v in 0..n {
        let poly = percolator.polys[v].sub(&percolator.polys[v + n]);
        println!("old {}: {}", v, poly);
        println!("new {}: {}", v, poly.add(&poly.apply(&Polynomial::of_vec(&vec![1, -1]))));
    }

}