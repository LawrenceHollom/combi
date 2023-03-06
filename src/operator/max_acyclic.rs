extern crate utilities;
extern crate queues;

use utilities::*;
use utilities::vertex_tools::*;
use crate::graph::*;

use queues::*;

pub fn max_acyclic_subgraph(g: &Graph) -> u32 {
    let mut max_acyclic = 2;
    for i in 0..pow(2, g.n.to_usize() as u64) {
        let subset = VertexSet::of_int(i as u128);

        let mut is_cycle = false;
        let size = subset.size();

        if size > max_acyclic { // no point testing stuff that's too small
            // now test if there is a cycle amongst the vertices of the subset
            // we use flood fill to test this. Takes N^2 time, could be optd
            let mut flood: VertexVec<Option<Vertex>> = VertexVec::new(g.n, &None);
            let mut found;
            let mut q : Queue<Vertex> = queue![];

            'cycle_search: loop {
                found = false;
                'unvisited_search: for i in g.n.iter_verts() {
                    if subset.has_vert(i) && flood[i].is_none() {
                        found = true;
                        flood[i] = Some(i);
                        let _ = q.add(i);
                        break 'unvisited_search;
                    }
                }

                if !found { // We have flood filled everything in subset (and found no cycle)
                    break 'cycle_search;
                }

                'flood_fill: loop {
                    match q.remove() {
                        Ok(node) => {
                            for i in g.n.iter_verts() {
                                if subset.has_vert(i) && g.adj[node][i] {
                                    if flood[i].is_some() && flood[node] != Some(i) { // it's already been found from somewhere else, so there's a cycle
                                        is_cycle = true;
                                        break 'cycle_search;
                                    } else if flood[i].is_none() { // First time this is found
                                        flood[i] = Some(node);
                                        let _ = q.add(i);
                                    }
                                }
                            }
                        },
                        Err(_err) => break 'flood_fill,
                    }
                }
            }
        }

        if !is_cycle && size > max_acyclic {
            max_acyclic = size;
        }
    }
    max_acyclic as u32
}