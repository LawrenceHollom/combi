use crate::entity::graph::*;
use rand::{Rng, thread_rng};

use std::io::*;

use utilities::*;
use utilities::polynomial::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;
use utilities::component_tools::*;

pub struct Percolator {
    probs: Vec<Polynomial>,
    pub polys: VertexVec<Polynomial>,
    pub dist_polys: VertexVec<Vec<Polynomial>>,
}

impl Percolator {
    pub fn new(order: Order, size: usize) -> Self {
        let mut probs: Vec<Polynomial> = vec![];

        let p = Polynomial::of_vec(&vec![0, 1]);
        let one_minus_p = Polynomial::of_vec(&vec![1, -1]);
        for i in 0..(size+1) {
            probs.push(p.pow(i).mul(&one_minus_p.pow(size - i)));
        }
        let mut polys: VertexVec<Polynomial> = VertexVec::new(order, &Polynomial::new());
        let mut dist_polys: VertexVec<Vec<Polynomial>> = VertexVec::new(order, &vec![Polynomial::new(); 2 * order.to_usize()]);
        polys[Vertex::ZERO] = Polynomial::of_vec(&vec![1]);
        for poly in dist_polys[Vertex::ZERO].iter_mut() {
            *poly = Polynomial::of_vec(&vec![1]);
        }

        Self {
            probs,
            polys,
            dist_polys,
        }
    }

    pub fn add_percolation(&mut self, num_edges: usize, g: &Graph, compute_dists: bool) {
        let connected = g.flood_fill_dist(Vertex::ZERO);

        for (v, dist) in connected.iter_enum().skip(1) {
            if let Some(dist) = *dist {
                self.polys[v].add_inplace(&self.probs[num_edges]);
                if compute_dists {
                    for poly in self.dist_polys[v].iter_mut().skip(dist as usize) {
                        poly.add_inplace(&self.probs[num_edges]);
                    }
                }
            }
        }
    }

    pub fn empirically_percolate_once(g: &Graph, p: f64) -> Vec<bool> {
        let mut rng = thread_rng();

        let mut true_adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(g.n, &vec![]);
        
        for (i, j) in g.iter_pairs() {
            if g.adj[i][j] && rng.gen_bool(p) {
                true_adj_list[i].push(j);
                true_adj_list[j].push(i);
            }
        }
        Graph::of_adj_list(true_adj_list, crate::constructor::Constructor::Special)
            .flood_fill_dist(Vertex::ZERO)
            .iter()
            .map(|x| x.is_some())
            .collect()
    }

    pub fn percolate(g: &Graph, compute_dists: bool, should_print: bool) -> Self {
        let mut percolator = Self::new(g.n, g.size());

        let loop_max = utilities::pow(2, g.size() as u64);
        println!("Starting the big loop! size: {}", loop_max);
        let mut next_alert = 1;
        for subset in 0..loop_max {
            if (100 * subset) / loop_max >= next_alert {
                print!("{}% ", next_alert);
                std::io::stdout().flush().unwrap();
                next_alert += 1;
            }
            let mut edges = vec![false; g.size()];
            let mut num_edges = 0;
            let mut sta = subset;
            for edge in edges.iter_mut() {
                if sta % 2 == 1 {
                    *edge = true;
                    num_edges += 1;
                }
                sta /= 2;
            }

            let mut true_adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(g.n, &vec![]);
            let mut edge_index = 0;
            for (i, j) in g.iter_pairs() {
                if g.adj[i][j] {
                    if edges[edge_index] {
                        true_adj_list[i].push(j);
                        true_adj_list[j].push(i);
                    }
                    edge_index += 1;
                }
            }
            let restricted_g = Graph::of_adj_list(true_adj_list, crate::constructor::Constructor::Special);
            percolator.add_percolation(num_edges, &restricted_g, compute_dists);
        }
        println!();

        if should_print {
            g.print();
        }
        percolator
    }
}

pub fn print_polynomials(g: &Graph) {
    let percolator = Percolator::percolate(g, false, true);
    
    for v in g.n.iter_verts() {
        println!("P_{}: {}", v, percolator.polys[v]);
    }

    for u in Vertex::ZERO.incr().iter_from_to(g.n.to_max_vertex()) {
        for v in g.adj_list[u].iter() {
            if *v > u {
                let diff = percolator.polys[u].sub(&percolator.polys[*v]);
                let unimodicity = diff.find_prob_unimode();
                print!("P_{} - P_{} is ", u, *v);
                match unimodicity {
                    Modality::Unimodal(mode) => println!("unimodal with extremum at {}", mode),
                    e => println!("not unimodal: {}", e),
                }
            }
        }
    }
}

/**
 * Counts how many configurations there are in which v connects to u.
 * The filter is a test for whether an EdgeSet should count or not (true = counted)
 */
pub fn get_connection_counts(g: &Graph, v: Vertex, filter: Option<&dyn Fn(&VertexVec<Component>) -> bool>) -> VertexVec<u64> {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut out = VertexVec::new(g.n, &0);
    'iter_edge_sets: for edges in g.iter_edge_sets() {
        let components = g.edge_subset_components(edges, &indexer);
        if filter.is_some_and(|f| f(&components)) {
            continue 'iter_edge_sets
        }
        for u in g.iter_verts() {
            if components[u] == components[v] {
                out[u] += 1;
            }
        }
    }
    out
}

/**
 * Counts how many configurations there are in which v connects to u under site percolation.
 */
pub fn get_site_connection_counts(g: &Graph, v: Vertex) -> VertexVec<u64> {
    let mut out = VertexVec::new(g.n, &0);
    for vs in g.iter_vertex_subsets() {
        let big_vs = BigVertexSet::of_vertex_set(vs);
        let components = g.vertex_subset_components(big_vs);
        for u in g.iter_verts() {
            if components[u] == components[v] {
                out[u] += 1;
            }
        }
    }
    out
}

/**
 * Returns a vec x where x[k] is in how many ways v can connect to any
 * of the first k things in the list of ordered vertices.
 * The filter is a test for whether an EdgeSet should count or not (true = counted)
 */
pub fn get_initial_segment_connection_counts(g: &Graph, v: Vertex, sorted_vertices: &Vec<Vertex>, filter: Option<&dyn Fn(&VertexVec<Component>) -> bool>) -> Vec<u64> {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut out = vec![0; g.n.to_usize()];
    'iter_edge_sets: for edges in g.iter_edge_sets() {
        let components = g.edge_subset_components(edges, &indexer);
        if filter.is_some_and(|f| f(&components)) {
            continue 'iter_edge_sets
        }
        'find_first_connected: for (i, u) in sorted_vertices.iter().enumerate() {
            if components[*u] == components[v] {
                out[i] += 1;
                break 'find_first_connected;
            }
        }
    }

    // Now take a rolling sum of out to get the actual counts.
    let mut rolling_sum = 0;
    for val in out.iter_mut() {
        rolling_sum += *val;
        *val = rolling_sum;
    }

    out
}

/**
 * Returns a vec x where x[k] is in how many ways v can connect to any
 * of the first k things in the list of ordered vertices under site percolation.
 */
pub fn get_initial_segment_site_connection_counts(g: &Graph, v: Vertex, sorted_vertices: &Vec<Vertex>) -> Vec<u64> {
    let mut out = vec![0; g.n.to_usize()];
    for vs in g.iter_vertex_subsets() {
        let components = g.vertex_subset_components(BigVertexSet::of_vertex_set(vs));
        'find_first_connected: for (i, u) in sorted_vertices.iter().enumerate() {
            if components[*u] == components[v] {
                out[i] += 1;
                break 'find_first_connected;
            }
        }
    }

    // Now take a rolling sum of out to get the actual counts.
    let mut rolling_sum = 0;
    for val in out.iter_mut() {
        rolling_sum += *val;
        *val = rolling_sum;
    }

    out
}