use crate::graph::*;

fn monot_predecessors(g: &Graph, forward: bool, start: usize) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut pred: Vec<usize> = vec![n; n];
    pred[start] = start;
    if forward {
        for i in 0..n {
            if pred[i] != n {
                for j in g.adj_list[i].iter() {
                    if *j > i /*&& pred[*j] == n*/ {
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
    let pred = monot_predecessors(g, true, 0);
    let n = g.n.to_usize();
    pred[n-1] != n
}

pub fn num_on_long_monotone(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let pred = monot_predecessors(g, true, 0);
    let succ = monot_predecessors(g, false, n-1);
    let mut num_monotted = 0;
    for i in 0..n {
        if pred[i] != n && succ[i] != n {
            num_monotted += 1;
        }
    }
    num_monotted
}

pub fn max_monotone(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut dp: Vec<u32> = vec![1; n];
    let mut max: u32 = 1;
    for i in 0..n {
        for j in g.adj_list[i].iter() {
            if *j < i && dp[*j] >= dp[i] {
                dp[i] = dp[*j] + 1;
                if dp[i] > max {
                    max = dp[i];
                }
            }
        }
    }
    max
}

pub fn num_on_monot_cycle(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut on_monot = vec![false; n];
    for start in 0..(n-2) {
        let pred = monot_predecessors(g, true, start);
        for i in start+1..n {
            if pred[i] != n && pred[i] != start && g.adj[start][i] {
                let mut j = i;
                'trace_back_path: loop {
                    on_monot[j] = true;
                    if j == start {
                        break 'trace_back_path;
                    }
                    j = pred[j];
                }
            }
        }
    }

    on_monot.iter().filter(|x| **x).count() as u32
}