use crate::entity::graph::*;
use utilities::{component_tools::Component, vertex_tools::*};

use super::percolate;

/**
 * counts: the number of configs in which 0 connects to 1
 * sorted_vertices: vertices in decreasing order of prob. of connection to b
 * set_counts[k]: the count of 0 connecting to one of the first k+1 sorted_vertices
 * denom: the total number of configs
 */
struct PercolatedData {
    counts: VertexVec<u64>,
    sorted_vertices: Vec<Vertex>,
    set_counts: Vec<u64>,
    denom: u64,
    b: Vertex,
}

impl PercolatedData {
    fn get_sorted_vertices(counts: &VertexVec<u64>) -> Vec<Vertex> {
        let mut for_ordering = counts.iter_enum().collect::<Vec<(Vertex, &u64)>>();
        for_ordering.sort_by(|(_, c1), (_, c2)| c1.cmp(c2).reverse() );
        for_ordering.iter().map(|(v, _)| *v).collect::<Vec<Vertex>>()
    }

    fn get_far(g: &Graph, start_dist: u32) -> Vertex {
        let mut out = None;
        let mut min_dist = start_dist;
        while out.is_none() {
            out = g.get_vertex_far_from(Vertex::ZERO, min_dist);
            min_dist -= 1;
        }
        out.unwrap()
    }


    pub fn new(g: &Graph) -> PercolatedData {
        let b = Self::get_far(g, 3);

        let unreachable = Self::get_far(g, 2);
        // True = should count, so we test if 0 and unreachable are in different components.
        fn generic_filter(components: &VertexVec<Component>, unreachable: Vertex) -> bool {
            components[Vertex::ZERO] != components[unreachable]
        }

        let counts = percolate::get_connection_counts(g, b, 
            Some(&|components| generic_filter(components, unreachable)));
        let denom = counts[b];

        // Order the vertices by how easily they connect to b.
        let sorted_vertices = Self::get_sorted_vertices(&counts);

        let set_counts = percolate::get_initial_segment_connection_counts(g, Vertex::ZERO, &sorted_vertices, 
            Some(&|components| generic_filter(components, unreachable)));

        PercolatedData {
            counts,
            sorted_vertices,
            set_counts,
            denom,
            b,
        }
    }

    pub fn new_site(g: &Graph) -> PercolatedData {
        let b = Self::get_far(g, 3);
        let counts = percolate::get_site_connection_counts(g, b);
        let denom = counts[b];

        // Order the vertices by how easily they connect to b.
        let sorted_vertices = Self::get_sorted_vertices(&counts);

        let set_counts = percolate::get_initial_segment_site_connection_counts(g, Vertex::ZERO, &sorted_vertices);

        PercolatedData {
            counts,
            sorted_vertices,
            set_counts,
            denom,
            b,
        }
    }

    pub fn print(&self) {
        println!("Sorted vertices: {:?}", self.sorted_vertices);
        println!("b = {}", self.b);
        let fdenom = self.denom as f64;

        let zero_to_b_count = self.counts[Vertex::ZERO];
        let lhs = (zero_to_b_count as f64) / fdenom;

        println!("Denominator = {}", self.denom);
        println!("Pr(0 <-> b) = {}, rhs = {:.5}", zero_to_b_count, lhs);

        println!("Possible right-hand sides:");
        for (i, a) in self.sorted_vertices.iter().enumerate() {
            let rhs = ((self.set_counts[i] as f64) / fdenom) * ((self.counts[*a] as f64) / fdenom);
            println!("a = {}, Pr(0 <-> A) = {}, Pr(a <-> b) = {}, rhs = {:.5}", a, self.set_counts[i], self.counts[*a], rhs);
        }
    }

    /**
     * Returns true if the Kozma--Nitzan conjecture is false for any vertex.
     */
    pub fn has_contradictory_count(&self) -> bool {
        let target = self.counts[Vertex::ZERO] * self.denom;
        for (i, a) in self.sorted_vertices.iter().enumerate() {
            let rhs = self.set_counts[i] * self.counts[*a];
            if rhs > target {
                println!("Contradictory vertex found: {}", a);
                println!("Printing percolation_data:");
                self.print();
                return true;
            }
        }
        false
    }
}

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
    let percolated_data = PercolatedData::new(g);
    percolated_data.print();
}

pub fn print_site(g: &Graph) {
    let percolated_data = PercolatedData::new_site(g);
    percolated_data.print();
}

pub fn does_contradict_kozma_nitzan(g: &Graph) -> bool {
    let percolated_data = PercolatedData::new(g);
    percolated_data.has_contradictory_count()
}

pub fn does_contradict_site_kozma_nitzan(g: &Graph) -> bool {
    let percolated_data = PercolatedData::new_site(g);
    percolated_data.has_contradictory_count()
}