use queues::*;

use crate::entity::graph::*;

use utilities::component_tools::*;
use utilities::vertex_tools::*;

/**
 * This file deals with structural constructions, based on other
 * graphs, for example the giant component of G(n,p) or the
 * 2-core of a graph
 */

/**
 * Returns a graph isomorphic to the largest component of g.
 */
pub fn giant_component(g: Graph) -> Graph {
    let components = g.components();
    let sizes = ComponentVec::<usize>::new_sizes(&components);
    let biggest = sizes.arg_max(&0, usize::cmp).unwrap();
    let filter = components.iter().map(|c| *c == biggest).collect::<VertexVec<bool>>();
    g.of_filtered(&filter)
}

/**
 * Returns a graph isomorphic to the 2-core of the given graph.
 * It is assumed that the given graph is connected.
 * Runs in O(edges + verts) or so
 */
pub fn two_core(g: Graph) -> Graph {
    let mut filter = VertexVec::new(g.n, &true);
    let mut untrimmed_degree = g.deg.to_owned();
    let mut q = queue![];
    
    for v in g.iter_verts() {
        if g.deg[v].at_most(1) {
            let _ = q.add(v);
        }
    }

    while let Ok(v) = q.remove() {
        // v has trimmed deg at most 1, so remove it.
        filter[v] = false;
        'find_parent: for w in g.adj_list[v].iter() {
            if filter[*w] {
                // this must be the parent. Decrease its untrimmed degree.
                untrimmed_degree[*w].decr_inplace();
                if untrimmed_degree[*w].at_most(1) {
                    // This is now a leaf, so add to the queue.
                    let _ = q.add(*w);
                }
                break 'find_parent;
            }
        }
    }

    g.of_filtered(&filter)
}