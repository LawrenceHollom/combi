use crate::entity::digraph::*;
use utilities::vertex_tools::*;

/**
 * Following the terminology of Bucic (and probably earlier authors
 * as well), we call an edge uv witnessed if u and v have a common
 * parent.
 * Any minimal counterexample to the Bermond--Thomassen conjecture
 * must have every edge witnessed,
 */
pub fn is_every_edge_witnessed(d: &Digraph) -> bool {
    let in_sets = d
        .in_adj_list
        .iter()
        .map(|vs| VertexSet::of_vec(d.n, vs))
        .collect::<VertexVec<VertexSet>>();
    for e in d.iter_edges() {
        if in_sets[e.fst()].inter(&in_sets[e.snd()]).is_empty() {
            // This edge is not witnessed as there is no common parent.
            return false;
        }
    }
    true
}

/**
 * Tests whether the digraph has an edge which can be traversed in both
 * directions, i.e. a two-edge cycle
 */
pub fn has_bidirectional_edge(d: &Digraph) -> bool {
    for (x, y) in d.iter_pairs() {
        if d.adj[x][y] && d.adj[y][x] {
            return true;
        }
    }
    false
}
