use crate::entity::graph::*;

use utilities::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use std::fmt;

use rand::thread_rng;
use rand::prelude::SliceRandom;

mod bowties;
mod triple_pentagons;

#[derive(Clone, Copy)]
pub enum VertexPattern {
    Bowties(Degree),
}

pub struct GraphWithVertices {
    h: Graph,
    verts: Vec<Vertex>,
    other_verts: VertexVec<Vertex>,
    num_verts: usize,
}

#[derive(Clone, Copy)]
pub enum EdgePattern {
    TriplePentagons,
}

pub struct GraphWithEdges {
    h: Graph,
    edges: Vec<Edge>,
    verts_not_in_edge: VertexVec<Vertex>,
    num_edges: usize,
}

impl VertexPattern {
    fn num_around_vertex(&self) -> usize {
        use VertexPattern::*;
        match self {
            Bowties(d) => d.to_usize(),
        }
    }

    pub fn of_string(text: &str) -> Option<VertexPattern> {
        let (pattern, args) = parse_function_like(text);
        use VertexPattern::*;

        match pattern.to_lowercase().as_str() {
            "bowties" => Some(Bowties(Degree::of_string(args[0]))),
            _ => None,
        }
    }

    pub fn new_graph(&self, num: usize) -> Graph {
        let gwv = GraphWithVertices::new(self);
        let nav = self.num_around_vertex();
        if (gwv.num_verts * num) % nav != 0 {
            panic!("Parity does not work!");
        }
        let h_n = gwv.h.n.to_usize();
        let num_internal_per_h = h_n - gwv.num_verts;
        let num_extra_verts = (gwv.num_verts * num) / nav;
        let n = Order::of_usize(num_internal_per_h * num + num_extra_verts);
        let mut adj_list = VertexVec::new(n, &vec![]);

        let mut other_verts_inv = VertexVec::new(gwv.h.n, &Vertex::ZERO);
        for (i, x) in gwv.other_verts.iter_enum() {
            other_verts_inv[*x] = i;
        }

        // Add stuff we already know to adj_list
        for h_index in 0..num {
            let offset = h_index * num_internal_per_h;
            for v in gwv.other_verts.iter() {
                for w in gwv.h.adj_list[*v].iter() {
                    if gwv.other_verts.contains(w, Vertex::eq) {
                        adj_list[other_verts_inv[*v].incr_by(offset)].push(other_verts_inv[*w].incr_by(offset));
                    }
                }
            }
        }

        let mut ordering: VertexVec<Vertex> = (0..(num * gwv.num_verts)).map(|x| Vertex::of_usize(x / nav)).collect();
        let mut rng = thread_rng();

        'find_good_shuffle: loop {
            ordering.shuffle(&mut rng);
            let mut last_part = VertexVec::new(Order::of_usize(num_extra_verts), &num);
            let mut is_good = true;
            'test_goodness: for (i, v) in ordering.iter().enumerate() {
                let h_index = i / gwv.num_verts;
                if last_part[*v] == h_index {
                    is_good = false;
                    break 'test_goodness;
                }
                last_part[*v] = h_index;
            }
            if is_good {
                break 'find_good_shuffle;
            }
        }

        for (i, vert) in ordering.iter().enumerate() {
            let h_index = i / gwv.num_verts;
            let h_u = gwv.verts[i % gwv.num_verts];
            let v = vert.incr_by(num * num_internal_per_h);
            for h_x in gwv.h.adj_list[h_u].iter() {
                let x = other_verts_inv[*h_x].incr_by(num_internal_per_h * h_index);
                adj_list[x].push(v);
                adj_list[v].push(x);
            }
        }

        use crate::constructor::*;
        Graph::of_adj_list(adj_list, Constructor::Random(RandomConstructor::VertexStructured(*self, num)))
    }
}

impl GraphWithVertices {
    pub fn new(pattern: &VertexPattern) -> GraphWithVertices {
        use VertexPattern::*;
        match pattern {
            Bowties(d) => bowties::new(d),
        }
    }
}

impl EdgePattern {
    fn num_around_edge(&self) -> usize {
        use EdgePattern::*;
        match self {
            TriplePentagons => 2,
        }
    }

    pub fn of_string(text: &str) -> Option<EdgePattern> {
        let (pattern, _args) = parse_function_like(text);
        use EdgePattern::*;

        match pattern.to_lowercase().as_str() {
            "triple_pentagons" | "pentagons" | "pents" => Some(TriplePentagons),
            _ => None,
        }
    }

    pub fn new_graph(&self, num: usize) -> Graph {
        let gwe = GraphWithEdges::new(self);
        let nae = self.num_around_edge();
        if (gwe.num_edges * num) % nae != 0 {
            panic!("Parity doesn't work!");
        }
        let h_n = gwe.h.n.to_usize();
        let num_internal_per_h = h_n - 2 * gwe.num_edges;
        let num_extra_edges = (gwe.num_edges * num) / nae;
        let n = Order::of_usize(num_internal_per_h * num + 2 * num_extra_edges);
        let mut adj_list = VertexVec::new(n, &vec![]);

        let mut gwe_verts_inv = VertexVec::new(gwe.h.n, &Vertex::ZERO);
        for (i, x) in gwe.verts_not_in_edge.iter_enum() {
            gwe_verts_inv[*x] = i;
        }

        // Sort out adj_list stuff we already know.
        for h_index in 0..num {
            let offset = h_index * num_internal_per_h;
            for v in gwe.verts_not_in_edge.iter() {
                for w in gwe.h.adj_list[*v].iter() {
                    if gwe.verts_not_in_edge.contains(w, Vertex::eq) {
                        adj_list[gwe_verts_inv[*v].incr_by(offset)].push(gwe_verts_inv[*w].incr_by(offset));
                    }
                }
            }
        }
        for edge_index in 0..num_extra_edges {
            let offset = Vertex::of_usize(2 * edge_index + num * num_internal_per_h);
            adj_list[offset].push(offset.incr());
            adj_list[offset.incr()].push(offset);
        }

        let mut ordering: Vec<usize> = (0..(num * gwe.num_edges)).map(|x| x / nae).collect();
        let mut rng = thread_rng();
        
        'find_good_shuffle: loop {
            ordering.shuffle(&mut rng);
            let mut last_part = vec![num; num_extra_edges];
            let mut is_good = true;
            'test_goodness: for (i, e) in ordering.iter().enumerate() {
                let h_copy = i / gwe.num_edges;
                if last_part[*e] == h_copy {
                    is_good = false;
                    break 'test_goodness;
                }
                last_part[*e] = h_copy;
            }
            if is_good {
                break 'find_good_shuffle;
            }
        }

        // Now add in the extra edges from the shuffle.
        for (i, edge) in ordering.iter().enumerate() {
            let h_copy = i / gwe.num_edges;
            let (h_u, h_v) = gwe.edges[i % gwe.num_edges].to_pair();
            let u = Vertex::of_usize(num * num_internal_per_h + 2 * *edge);
            let v = Vertex::of_usize(num * num_internal_per_h + 2 * *edge + 1);
            for h_x in gwe.h.adj_list[h_u].iter() {
                if *h_x != h_v {
                    let x = gwe_verts_inv[*h_x].incr_by(num_internal_per_h * h_copy);
                    adj_list[x].push(u);
                    adj_list[u].push(x);
                }
            }
            for h_x in gwe.h.adj_list[h_v].iter() {
                if *h_x != h_u {
                    let x = gwe_verts_inv[*h_x].incr_by(num_internal_per_h * h_copy);
                    adj_list[x].push(v);
                    adj_list[v].push(x);
                }
            }
        }

        use crate::constructor::*;
        Graph::of_adj_list(adj_list, Constructor::Random(RandomConstructor::EdgeStructured(*self, num)))
    }
}

impl GraphWithEdges {
    pub fn new(pattern: &EdgePattern) -> GraphWithEdges {
        use EdgePattern::*;
        match pattern {
            TriplePentagons => triple_pentagons::new()
        }
    }
}

impl fmt::Display for VertexPattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use VertexPattern::*;
        match self {
            Bowties(d) => write!(f, "Bowties of degree {}", d),
        }
    }
}

impl fmt::Display for EdgePattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use EdgePattern::*;
        match self {
            TriplePentagons => write!(f, "Triple pentagons"),
        }
    }
}