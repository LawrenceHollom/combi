use crate::entity::graph::*;

use utilities::vertex_tools::*;

use queues::*;

fn is_k_connected_rec(
    g: &Graph,
    picked: &mut VertexVec<bool>,
    next_vert: Vertex,
    picks_left: usize,
    filter: Option<&VertexVec<bool>>,
) -> bool {
    fn passes_filter(filter: Option<&VertexVec<bool>>, v: Vertex) -> bool {
        filter.is_none_or(|filter| filter[v])
    }
    if picks_left == 0 {
        // test if g is connected without picked vertices.
        let mut start = Vertex::ZERO;
        let mut found_start = false;
        'find_start: for i in g.n.iter_verts() {
            if !picked[i] && passes_filter(filter, i) {
                start = i;
                found_start = true;
                break 'find_start;
            }
        }
        if !found_start {
            // This only fails if everything has been picked, i.e. g has k vertices.
            return false;
        }
        let mut q = queue![];
        let mut visited = VertexVec::new(g.n, &false);
        let _ = q.add(start);
        while q.size() > 0 {
            let u = q.remove().unwrap();
            for v in g.adj_list[u].iter() {
                if !picked[*v] && !visited[*v] && passes_filter(filter, *v) {
                    visited[*v] = true;
                    let _ = q.add(*v);
                }
            }
        }
        let mut connected = true;
        'test_connectedness: for i in g.n.iter_verts() {
            if !visited[i] && !picked[i] && passes_filter(filter, i) {
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
        let mut next_next_vert = next_vert.incr();
        while !next_next_vert.is_n(g.n) && !passes_filter(filter, next_next_vert) {
            next_next_vert.incr_inplace()
        }
        let pick_result = is_k_connected_rec(g, picked, next_next_vert, picks_left - 1, filter);
        picked[next_vert] = false;
        let skip_result = is_k_connected_rec(g, picked, next_next_vert, picks_left, filter);
        skip_result && pick_result
    }
}

pub fn is_filtered_k_connected(g: &Graph, k: usize, filter: Option<&VertexVec<bool>>) -> bool {
    // try removing any subset of k-1 vertices recursively.
    if k == 0 {
        true
    } else {
        let mut picked = VertexVec::new(g.n, &false);
        is_k_connected_rec(g, &mut picked, Vertex::ZERO, k - 1, filter)
    }
}

pub fn is_k_connected(g: &Graph, k: usize) -> bool {
    is_filtered_k_connected(g, k, None)
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

pub fn num_cutvertices(g: &Graph) -> u32 {
    let mut num_cutvertices = 0;
    let mut filter = VertexVec::new(g.n, &true);
    for v in g.iter_verts() {
        filter[v] = false;
        let nc = g.num_filtered_components(Some(&filter));
        if nc > 1 {
            num_cutvertices += 1;
        }
        filter[v] = true;
    }
    num_cutvertices
}

#[cfg(test)]
mod tests {
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
        assert_eq!(connectedness(&Graph::new_complete(Order::of_usize(10))), 9);
    }

    #[test]
    fn test_num_cutvertices_1() {
        assert_eq!(num_cutvertices(&Graph::test_graph(1)), 3);
    }

    #[test]
    fn test_num_cutvertices_3() {
        assert_eq!(num_cutvertices(&Graph::test_graph(3)), 0);
    }

    #[test]
    fn test_num_cutvertices_p10() {
        assert_eq!(num_cutvertices(&Graph::new_path(Order::of_usize(10))), 8);
    }

    #[test]
    fn test_num_cutvertices_k10() {
        assert_eq!(
            num_cutvertices(&Graph::new_complete(Order::of_usize(10))),
            0
        );
    }
}
