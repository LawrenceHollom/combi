use crate::graph::*;

fn monot_predecessors(g: &Graph, forward: bool) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut pred: Vec<usize> = vec![n; n];
    let start = if forward { 0 } else { n-1 };
    pred[start] = start;
    if forward {
        for i in 0..n {
            if pred[i] != n {
                for j in g.adj_list[i].iter() {
                    if *j > i && pred[*j] == n {
                        pred[*j] = i;
                    }
                }
            }
        }
    } else {
        for i in (0..n).rev() {
            if pred[i] != n {
                for j in g.adj_list[i].iter() {
                    if *j < i && pred[*j] == n {
                        pred[*j] = i;
                    }
                }
            }
        }
    }

    pred
}

pub fn has_long_monotone(g: &Graph) -> bool {
    let pred = monot_predecessors(g, true);
    let n = g.n.to_usize();
    pred[n-1] != n
}

pub fn num_on_long_monotone(g: &Graph) -> u32 {
    let pred = monot_predecessors(g, true);
    let succ = monot_predecessors(g, false);
    let n = g.n.to_usize();
    let mut num_monotted = 0;
    for i in 0..n {
        if pred[i] != n && succ[i] != n {
            num_monotted += 1;
        }
    }
    num_monotted
}