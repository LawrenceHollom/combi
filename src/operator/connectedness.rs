use crate::graph::*;

use utilities::vertex_tools::*;

use queues::*;

fn is_k_connected_rec(g: &Graph, picked: &mut VertexVec<bool>, next_vert: Vertex, picks_left: usize) -> bool {
    if picks_left == 0 {
        // test if g is connected without picked vertices.
        let mut start = Vertex::ZERO;
        'find_start: for i in g.n.iter_verts() {
            if !picked[i] {
                start = i;
                break 'find_start;
            }
        }
        let mut q = queue![];
        let mut visited = VertexVec::new(g.n, &false);
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
        'test_connectedness: for i in g.n.iter_verts() {
            if !visited[i] && !picked[i] {
                connected = false;
                break 'test_connectedness;
            }
        }
        connected
    } else if next_vert.is_n(g.n) {
        true
    } else {
        // pick the next vertex and recurse.
        picked[next_vert] = true;
        let pick_result = is_k_connected_rec(g, picked, next_vert.incr(), picks_left - 1);
        picked[next_vert] = false;
        let skip_result = is_k_connected_rec(g, picked, next_vert.incr(), picks_left);
        skip_result && pick_result
    }
}

pub fn is_k_connected(g: &Graph, k: usize) -> bool {
    // try removing any subset of k-1 vertices recursively.
    if k == 0 {
        true
    } else {
        let mut picked = VertexVec::new(g.n, &false);
        is_k_connected_rec(g, &mut picked, Vertex::ZERO, k - 1)
    }
}

// This is stupid slow for e.g. the complete graph.
pub fn connectedness(g: &Graph) -> u32 {
    let mut k = 1;
    'find_connectedness: while k < g.n.to_usize() {
        if !is_k_connected(g, k) {
            break 'find_connectedness;
        }
        k += 1;
    }
    (k - 1) as u32
}
#[cfg(test)]
mod tests {
    use crate::graph::*;
    use super::*;
    use utilities::*;

    #[test]
    fn test_connectedness_1() {
        assert_eq!(connectedness(&Graph::test_graph(1)), 1);
    }

    #[test]
    fn test_connectedness_2() {
        assert_eq!(connectedness(&Graph::test_graph(2)), 0);
    }

    #[test]
    fn test_connectedness_3() {
        assert_eq!(connectedness(&Graph::test_graph(3)), 2);
    }

    #[test]
    fn test_connectedness_k10() {
        assert_eq!(connectedness(&Graph::new_empty(Order::of_usize(10))), 9);
    }
}