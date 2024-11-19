use crate::entity::graph::*;
use utilities::vertex_tools::*;

use super::percolate;

/**
 * Simulate the percolation probabilities
 * P(0 <-> b) and P(0 <-> A) min {P(a <-> b) : a in A}
 * Might as well add any and all points to A that don't decrease the min
 * and so there are only n different choices of A that we need to test.
 * 
 * We assume that p = 1/2 because sometimes life needs to be easy.
 * The start vertex is 0 and the end vertex is 1
 */
pub fn print(g: &Graph) {
    // We first need to count how many things connect to b.
    let b = Vertex::of_usize(1);
    let counts = percolate::get_connection_counts(g, b);
    // Order the vertices by how easily they connect to b.
    let mut for_ordering = counts.iter_enum().collect::<Vec<(Vertex, &u64)>>();
    for_ordering.sort_by(|(_, c1), (_, c2)| c1.cmp(c2) );
    let sorted_vertices = for_ordering.iter().map(|(v, _)| *v).collect::<Vec<Vertex>>();
    let zero_to_b_count = counts[Vertex::ZERO];
    let set_counts = percolate::get_initial_segment_connection_counts(g, b, &sorted_vertices);
}