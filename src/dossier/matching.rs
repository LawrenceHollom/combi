use utilities::vertex_tools::*;

use crate::entity::graph::*;

/**
 * This is for finding maximum matchings in bipartite graphs.
 */

fn visit_vertex_rec(
    g: &Graph,
    v: Vertex,
    visited: &mut VertexSet,
    a: &mut VertexSet,
    a_side: bool,
) {
    if a_side {
        a.add_vert(v);
    }
    for u in g.adj_list[v].iter() {
        if !visited.has_vert(*u) {
            // We need to visit here as well!
            visited.add_vert(*u);
            visit_vertex_rec(g, *u, visited, a, !a_side);
        }
    }
}

/**
 * Attempts to find an augmenting path starting at v.
 * If it succees, and finds an augmenting path to u, Some(u) is returned.
 * Otherwise, None is returned.
 * v is necessarily in the same part of the graph as the starting vertex.
 */
fn find_augmenting_path_rec(
    g: &Graph,
    v: Vertex,
    matching: &VertexVec<Option<Vertex>>,
    aug_predecessor: &mut VertexVec<Option<Vertex>>,
) -> Option<Vertex> {
    for u in g.adj_list[v].iter() {
        if aug_predecessor[*u].is_none() {
            // u has not been visited, so we can visit it now.
            aug_predecessor[*u] = Some(v);
            if let Some(w) = matching[*u] {
                aug_predecessor[w] = Some(*u);
                let x = find_augmenting_path_rec(g, w, matching, aug_predecessor);
                if x.is_some() {
                    return x;
                }
            } else {
                // This is not in the matching, so is the end of an augmenting path!
                return Some(*u);
            }
        }
    }
    None
}

pub fn max_matching_size(g: &Graph) -> u32 {
    let mut a = VertexSet::new(g.n);
    let mut visited = VertexSet::new(g.n);

    // Compute the bipartition; a is one of the parts.
    for v in g.iter_verts() {
        if !visited.has_vert(v) {
            visited.add_vert(v);
            visit_vertex_rec(g, v, &mut visited, &mut a, true);
        }
    }

    // Now start building a matching.
    let mut matching = VertexVec::new(g.n, &None);
    let mut matching_grown = true;
    let mut matching_size = 0;
    while matching_grown {
        matching_grown = false;
        for v in a.iter() {
            if matching[v].is_none() {
                // Try and add v to the matching
                let mut aug_predecessor = VertexVec::new(g.n, &None);
                if let Some(u) = find_augmenting_path_rec(g, v, &matching, &mut aug_predecessor) {
                    // There is an augmenting path from u to v.
                    matching_size += 1;
                    matching_grown = true;
                    let mut x = u;
                    let mut y = aug_predecessor[x].unwrap();
                    'flip_path: loop {
                        matching[x] = Some(y);
                        matching[y] = Some(x);
                        if y == v {
                            break 'flip_path;
                        } else {
                            x = aug_predecessor[y].unwrap();
                            y = aug_predecessor[x].unwrap();
                        }
                    }
                }
            }
        }
    }

    matching_size
}
