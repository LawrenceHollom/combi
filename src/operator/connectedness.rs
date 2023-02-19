use crate::graph::*;

use queues::*;

fn is_k_connected_rec(g: &Graph, picked: &mut Vec<bool>, next_vert: usize, picks_left: usize) -> bool {
    let n = g.n.to_usize();
    if picks_left == 0 {
        // test if g is connected without picked vertices.
        let mut start = n;
        'find_start: for i in 0..n {
            if !picked[i] {
                start = i;
                break 'find_start;
            }
        }
        let mut q = queue![];
        let mut visited = vec![false; n];
        let _ = q.add(start);
        while q.size() > 0 {
            let u = q.remove().unwrap();
            for v in g.adj_list[u].iter() {
                if !picked[*v] && !visited[*v] {
                    visited[*v] = true;
                    let _ = q.add(*v);
                }
            }
        }
        let mut connected = true;
        'test_connectedness: for i in 0..n {
            if !visited[i] && !picked[i] {
                connected = false;
                break 'test_connectedness;
            }
        }
        connected
    } else if next_vert == n {
        true
    } else {
        // pick the next vertex and recurse.
        picked[next_vert] = true;
        let pick_result = is_k_connected_rec(g, picked, next_vert + 1, picks_left - 1);
        picked[next_vert] = false;
        let skip_result = is_k_connected_rec(g, picked, next_vert + 1, picks_left);
        skip_result && pick_result
    }
}

pub fn is_k_connected(g: &Graph, k: usize) -> bool {
    // try removing any subset of k-1 vertices recursively.
    if k == 0 {
        true
    } else {
        let n = g.n.to_usize();
        let mut picked = vec![false; n];
        is_k_connected_rec(g, &mut picked, 0, k - 1)
    }
}

// This is stupid slow for e.g. the complete graph.
pub fn connectedness(g: &Graph) -> u32 {
    let mut k = 1;
    let n = g.n.to_usize();
    'find_connectedness: while k < n {
        if !is_k_connected(g, k) {
            break 'find_connectedness;
        }
        k += 1;
    }
    (k - 1) as u32
}