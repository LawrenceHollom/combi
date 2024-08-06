use std::collections::HashSet;

use utilities::vertex_tools::*;

use crate::entity::graph::*;

fn monot_predecessors(g: &Graph, forward: bool, start: Vertex) -> VertexVec<Option<Vertex>> {
    let mut pred = VertexVec::new(g.n, &None);
    pred[start] = Some(start);
    if forward {
        for i in g.n.iter_verts() {
            if pred[i].is_some() {
                for j in g.adj_list[i].iter() {
                    if *j > i {
                        pred[*j] = Some(i);
                    }
                }
            }
        }
    } else {
        for i in g.n.iter_verts_rev() {
            if pred[i].is_some() {
                for j in g.adj_list[i].iter() {
                    if *j < i && pred[*j].is_none() {
                        pred[*j] = Some(i);
                    }
                }
            }
        }
    }

    pred
}

pub fn has_long_monotone(g: &Graph) -> bool {
    let pred = monot_predecessors(g, true, Vertex::ZERO);
    pred[g.n.to_max_vertex()].is_some()
}

pub fn num_on_long_monotone(g: &Graph) -> u32 {
    let pred = monot_predecessors(g, true, Vertex::ZERO);
    let succ = monot_predecessors(g, false, g.n.to_max_vertex());
    let mut num_monotted = 0;
    for i in g.n.iter_verts() {
        if pred[i].is_some() && succ[i].is_some() {
            num_monotted += 1;
        }
    }
    num_monotted
}

pub fn max_monotone(g: &Graph) -> u32 {
    let mut dp: VertexVec<u32> = VertexVec::new(g.n, &1);
    let mut max: u32 = 1;
    for i in g.n.iter_verts() {
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
    let mut on_monot = VertexVec::new(g.n, &false);
    for start in g.n.iter_verts() {
        let pred = monot_predecessors(g, true, start);
        for i in start.incr().iter_from(g.n) {
            if pred[i].is_some() && pred[i] != Some(start) && g.adj[start][i] {
                let mut j = i;
                'trace_back_path: loop {
                    on_monot[j] = true;
                    if j == start {
                        break 'trace_back_path;
                    }
                    j = pred[j].unwrap();
                }
            }
        }
    }

    on_monot.iter().filter(|x| **x).count() as u32
}

pub fn max_rigid_component(g: &Graph) -> u32 {
    let mut components: Vec<HashSet<Vertex>> = vec![];
    for start in g.n.iter_verts() {
        let pred = monot_predecessors(g, true, start);
        for i in start.incr().iter_from(g.n) {
            if pred[i].is_some() && pred[i] != Some(start) && g.adj[start][i] {
                let mut j = i;
                let mut h = HashSet::new();
                'trace_back_path: loop {
                    h.insert(j);
                    if j == start {
                        break 'trace_back_path;
                    }
                    j = pred[j].unwrap();
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
                    let comp_j_copy: HashSet<Vertex> = components[j].iter().copied().collect();
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