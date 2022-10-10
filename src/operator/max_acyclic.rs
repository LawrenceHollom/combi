extern crate utilities;
extern crate queues;

use utilities::*;
use crate::graph::*;

use queues::*;

pub fn max_acyclic_subgraph(g: &Graph) -> u32 {
    let mut max_acyclic = 2;
    let n = g.n.to_usize();
    for i in 0..pow(2, n as u64) {
        let mut subset = vec![false; n];
        let mut size = 0;
        let mut sta = i;
        for node in subset.iter_mut() {
            if sta % 2 == 1 {
                size += 1;
                *node = true;
            }
            sta /= 2;
        }

        let mut is_cycle = false;

        if size > max_acyclic { // no point testing stuff that's too small
            // now test if there is a cycle amongst the vertices of the subset
            // we use flood fill to test this. Takes N^2 time, could be optd
            let mut flood: Vec<usize> = vec![n; n];
            let mut found;
            let mut q : Queue<usize> = queue![];

            'cycle_search: loop {
                found = false;
                'unvisited_search: for i in 0..n {
                    if subset[i] && flood[i] == n {
                        found = true;
                        flood[i] = i;
                        let _ = q.add(i);
                        break 'unvisited_search;
                    }
                }

                if !found { // We have flood filled everything in subset (and found no cycle)
                    break 'cycle_search;
                }

                'flood_fill: loop {
                    match q.remove() {
                        Ok(node) => {
                            for i in 0..n {
                                if subset[i] && g.adj[node][i] {
                                    if flood[i] != n && flood[node] != i { // it's already been found from somewhere else, so there's a cycle
                                        is_cycle = true;
                                        break 'cycle_search;
                                    } else if flood[i] == n { // First time this is found
                                        flood[i] = node;
                                        let _ = q.add(i);
                                    }
                                }
                            }
                        },
                        _error => break 'flood_fill,
                    }
                }
            }
        }

        if !is_cycle && size > max_acyclic {
            max_acyclic = size;
        }
    }
    max_acyclic
}