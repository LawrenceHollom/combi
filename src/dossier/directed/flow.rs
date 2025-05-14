use crate::entity::digraph::*;

/**
 * This is for flows on digraphs.
 */

/**
 * Returns true if there is some vertex v such that, for every x in D,
 * there is a walk from x to v.
 * Uses Floyd--Warshall, and so runs in n^3 time.
 */
pub fn has_global_basin(d: &Digraph) -> bool {
    let dist = d.floyd_warshall();
    for v in d.iter_verts() {
        let mut is_v_basin = true;
        // Can we flow from anywhere to v?
        'test_v: for u in d.iter_verts() {
            if u != v && dist[u][v].is_none() {
                is_v_basin = false;
                break 'test_v;
            }
        }
        if is_v_basin {
            return true;
        }
    }
    false
}
