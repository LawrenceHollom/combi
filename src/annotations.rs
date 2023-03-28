use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::graph::*;

use utilities::vertex_tools::*;
use utilities::component_tools::Component;

/**
 * This stores a graph along with a collection of information which
 * aims to make it easier to compute various (otherwise expensive)
 * computations on g.
 * In particular, this aims to capture a bunch of information about 
 * Aut(G) which can be used to skip a bunch of steps in iterating 
 * over G.
 * We use random algorithms and good heuristics to do all these
 * computations imperfectly but quickly.
 */

// This stores what isomorphisms we've found of a graph.
// This probably also stores e.g. distance matrix from Floyd-Warshall
struct AnnotatedGraph {
    g: Graph,
    dists: Option<VertexVec<VertexVec<usize>>>,
    vertex_hashes: Option<VertexVec<u64>>,
    weak_auto_comps: Option<VertexVec<Component>>,
    strong_auto_comps: Option<VertexVec<Component>>,
}

/**
 * This stores a bunch of information about a vertex which we will use
 * as a heuristic for determining autocs. We want this to be strong.
 * In an ideal world this would be a "perfect hash of the graph from the
 * perspective of v", but the difficulty is in arriving at a canonical
 * ordering of the vertices of G in this perspective, so that the hash
 * hashes in a hashful way.
 * Perhaps a silly manual thing--e.g. hashing the 4-onion--would be a
 * good start here, as we could surely hack out some canonical ordering.
 */
#[derive(Hash)]
struct VertexSignature {
    dist_counts: Vec<usize>,
}

impl AnnotatedGraph {
    pub fn new_blank(g: Graph) -> AnnotatedGraph {
        AnnotatedGraph {
            g, 
            dists: None,
            vertex_hashes: None,
            weak_auto_comps: None,
            strong_auto_comps: None, 

        }
    }

    pub fn new_dists(g: Graph) -> AnnotatedGraph {
        let dists = g.floyd_warshall();
        AnnotatedGraph {
            g, 
            dists: Some(dists), 
            vertex_hashes: None,
            weak_auto_comps: None, 
            strong_auto_comps: None
        }
    }

    pub fn new(g: Graph) -> AnnotatedGraph {
        let dists = g.floyd_warshall();
        let hashes = VertexSignature::compute_vertex_hashes(&g, &dists);
        let mut weak_auto_comps = VertexVec::new_fn(g.n, |v| Component::of_vertex(v));
        let mut strong_auto_comps = VertexVec::new_fn(g.n, |v| Component::of_vertex(v));
        AnnotatedGraph { 
            g,
            dists: Some(dists),
            vertex_hashes: Some(hashes),
            weak_auto_comps: Some(weak_auto_comps), 
            strong_auto_comps: Some(strong_auto_comps) 
        }
    }
}

impl VertexSignature {
    fn new(g: &Graph, v: Vertex, dists: &VertexVec<VertexVec<usize>>) -> VertexSignature {
        let mut dist_counts = vec![0; g.n.to_usize()];
        for u in g.iter_verts() {
            dist_counts[dists[u][v]] += 1;
        }
        VertexSignature { dist_counts }
    }

    pub fn compute_vertex_hashes(g: &Graph, dists: &VertexVec<VertexVec<usize>>) -> VertexVec<u64> {
        let mut hashes = VertexVec::new(g.n, &0);
        for (v, hash) in hashes.iter_mut_enum() {
            let sig = VertexSignature::new(g, v, dists);
            let mut hasher = DefaultHasher::new();
            sig.hash(&mut hasher);
            *hash = hasher.finish();
        }
        hashes
    }
}