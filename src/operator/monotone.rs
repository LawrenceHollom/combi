use std::collections::HashSet;

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

pub fn max_rigid_component(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut components: Vec<HashSet<usize>> = vec![];
    for start in 0..(n-2) {
        let pred = monot_predecessors(g, true, start);
        for i in start+1..n {
            if pred[i] != n && pred[i] != start && g.adj[start][i] {
                let mut j = i;
                let mut h = HashSet::new();
                'trace_back_path: loop {
                    h.insert(j);
                    if j == start {
                        break 'trace_back_path;
                    }
                    j = pred[j];
                }
                components.push(h);
            }
        }
    }

    'combine: loop {
        let mut combinations_made = 0;
        let m = components.len();
        if m == 0 {
            break 'combine;
        }
        for i in 0..m-1 {
            for j in (i+1)..m {
                if components[i].intersection(&components[j]).count() >= 2 {
                    let comp_j_copy: HashSet<usize> = components[j].iter().copied().collect();
                    components[i].extend(&comp_j_copy);
                    components[j].clear();
                    combinations_made += 1;
                }
            }
        }

        if combinations_made == 0 {
            break 'combine;
        }
    }

    let mut largest_rigid = 0;

    for comp in components.iter() {
        if comp.len() > largest_rigid {
            largest_rigid = comp.len();
        }
    }

    largest_rigid as u32
}