use std::collections::HashMap;

use rand::Rng;
use rand::rngs::ThreadRng;
use rand::thread_rng;
use utilities::Order;
use utilities::edge_tools::*;
use utilities::file_write;
use utilities::vertex_tools::*;
use utilities::component_tools::*;

use crate::entity::graph::*;

const EPS: f64 = 0.00000001;

#[derive(Clone, Copy, Debug)]
struct Counts {
    none: usize,
    ab: usize,
    bc: usize,
    ac: usize,
    all: usize,
    total: usize,
}

struct Probabilities {
    none: f64,
    ab: f64,
    bc: f64,
    ac: f64,
    all: f64,
}

#[derive(Clone, Copy, Debug)]
struct PairCounts {
    neither: f64,
    left: f64,
    right: f64,
    both: f64,
    total: f64,
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
struct VertexTriple {
    pub a: Vertex,
    pub b: Vertex,
    pub c: Vertex,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
enum EdgeState {
    NONE,
    AB,
    BC,
    AC,
    ALL,
}

impl Counts {
    pub fn new() -> Counts {
        Counts { 
            none: 0, 
            ab: 0, 
            bc: 0, 
            ac: 0, 
            all: 0,
            total: 0, 
        }
    }

    pub fn update(&mut self, triple: VertexTriple, components: &VertexVec<Component>) {
        let ca = components[triple.a];
        let cb = components[triple.b];
        let cc = components[triple.c];
        self.total += 1;
        if ca == cb && cb == cc {
            self.all += 1;
        } else if ca == cb {
            self.ab += 1;
        } else if cb == cc {
            self.bc += 1;
        } else if ca == cc {
            self.ac += 1;
        } else {
            self.none += 1;
        }
    }

    pub fn is_balanced(&self) -> bool {
        self.ab == self.bc && self.ab == self.ac
    }

    pub fn print(&self) {
        Probabilities::of_counts(&self).print()
    }
}

impl PairCounts {
    pub fn new() -> PairCounts {
        PairCounts { 
            neither: 0.0, 
            left: 0.0, 
            right: 0.0, 
            both: 0.0, 
            total: 0.0, 
        }
    }

    /**
     * is_ei_full is for if ei is in an ALL triple, in which case it should
     * be down-weighted.
     */
    pub fn add(&mut self, p: f64, has_e1: bool, is_e1_full: bool, has_e2: bool, is_e2_full: bool, print_debug: bool) {
        if has_e1 && has_e2 {
            if is_e1_full && is_e2_full {
                self.both += p * 4.0 / 9.0;
                self.left += p * 2.0 / 9.0;
                self.right += p * 2.0 / 9.0;
                self.neither += p * 1.0 / 9.0;
                if print_debug {
                    println!("1 {:.3}", p)
                }
            } else if is_e1_full {
                self.both += p * 2.0 / 3.0;
                self.right += p * 1.0 / 3.0;
                if print_debug {
                    println!("2 {:.3}", p)
                }
            } else if is_e2_full {
                self.both += p * 2.0 / 3.0;
                self.left += p * 1.0 / 3.0;
                if print_debug {
                    println!("3 {:.3}", p)
                }
            } else {
                self.both += p;
                if print_debug {
                    println!("4 {:.3}", p)
                }
            }
        } else if has_e1 {
            if is_e1_full {
                self.left += p * 2.0 / 3.0;
                self.neither += p * 1.0 / 3.0;
                if print_debug {
                    println!("5")
                }
            } else {
                self.left += p;
                if print_debug {
                    println!("6")
                }
            }
        } else if has_e2 {
            if is_e2_full {
                self.right += p * 2.0 / 3.0;
                self.neither += p * 1.0 / 3.0;
                if print_debug {
                    println!("7")
                }
            } else {
                self.right += p;
                if print_debug {
                    println!("8")
                }
            }
        } else {
            self.neither += p;
            if print_debug {
                println!("9")
            }
        }
        self.total += p;
    }

    /**
     * Returns true if the two edges are positively correlated.
     */
    pub fn is_positively_correlated(&self) -> bool {
        self.both * self.total > (self.both + self.left) * (self.both + self.right) + EPS
    }

    pub fn print(&self) {
        println!("(neither, left, right, both) = ({:.3}, {:.3}, {:.3}, {:.3}) sum = {:.3}", self.neither, self.left, self.right, self.both, self.total);
        println!("P(both | left) = {:.3}", self.both / (self.left + self.both));
        println!("P(right) = {:.3}", (self.right + self.both) / self.total);
        println!("P(both | right) = {:.3}", self.both / (self.right + self.both));
        println!("P(left) = {:.3}", (self.left + self.both) / self.total);
    }
}

impl Probabilities {
    pub fn of_counts(counts: &Counts) -> Probabilities {
        Probabilities { 
            none: (counts.none as f64) / (counts.total as f64), 
            ab: (counts.ab as f64) / (counts.total as f64), 
            bc: (counts.bc as f64) / (counts.total as f64), 
            ac: (counts.ac as f64) / (counts.total as f64), 
            all: (counts.all as f64) / (counts.total as f64), 
        }
    }

    pub fn new_random(rng: &mut ThreadRng) -> Probabilities {
        let a = rng.gen_range(0.0..1.0);
        let b = rng.gen_range(0.0..1.0);
        let c = b;//rng.gen_range(0.0..1.0);
        let d = b;//rng.gen_range(0.0..1.0);
        let e = rng.gen_range(0.0..1.0);
        let sum = a + b + c + d + e;
        Probabilities { 
            none: a / sum,
            ab: b / sum,
            bc: c / sum,
            ac: d / sum,
            all: e / sum,
        }
        // Probabilities { 
        //     none: 1.0,// / 7.0, 
        //     ab: 1.0,// / 7.0, 
        //     bc: 1.0,// / 7.0, 
        //     ac: 1.0,// / 7.0, 
        //     all: 3.0,// / 7.0,
        // }
    }

    pub fn get_prob(&self, state: &EdgeState) -> f64 {
        use EdgeState::*;
        match state {
            NONE => self.none,
            AB => self.ab,
            BC => self.bc,
            AC => self.ac,
            ALL => self.all,
        }
    }

    pub fn get_product(&self, states: &Vec<EdgeState>) -> f64 {
        let mut out = 1.0;
        for state in states.iter() {
            out *= self.get_prob(state)
        }
        out
    }
    
    pub fn print(&self) {
        println!(" ({:.3}, {:.3}, {:.3}, {:.3}, {:.3})",
            self.none, self.ab, self.bc, self.ac, self.all);
    }
}

impl VertexTriple {
    pub fn new_ordered(a: Vertex, b: Vertex, c: Vertex) -> Option<VertexTriple> {
        if a < b && b < c {
            Some(VertexTriple { a, b, c })
        } else {
            None
        }
    }

    pub fn new_unordered(a: Vertex, b: Vertex, c: Vertex) -> Option<VertexTriple> {
        if a != b && b != c && a != c {
            Some(VertexTriple { a, b, c })
        } else {
            None
        }
    }

    pub fn get_present_edges(&self, state: EdgeState, maintain_acyclicity: bool) -> Vec<Edge> {
        use EdgeState::*;
        let ab = Edge::of_pair(self.a, self.b);
        let bc = Edge::of_pair(self.c, self.b);
        let ac = Edge::of_pair(self.a, self.c);
        match state {
            NONE => vec![],
            AB => vec![ab],
            BC => vec![bc],
            AC => vec![ac],
            ALL => {
                if maintain_acyclicity {
                    vec![ab, bc]
                } else {
                    vec![ab, bc, ac]
                }
            }
        }
    }

    // pub fn has_vertex(&self, v: Vertex) -> bool {
    //     self.a == v || self.b == v || self.c == v
    // }

    // pub fn has_edge(&self, e: Edge) -> bool {
    //     self.has_vertex(e.fst()) && self.has_vertex(e.snd())
    // }

    /**
     * This is terrible coding practise as the constructor makes it
     * look like these should always have the vertices in order.
     */
    pub fn permute(&self, rng: &mut ThreadRng) -> VertexTriple {
        match rng.gen_range(0..6) {
            0 => VertexTriple { a: self.a, b: self.b, c: self.c },
            1 => VertexTriple { a: self.a, b: self.c, c: self.b },
            2 => VertexTriple { a: self.b, b: self.a, c: self.c },
            3 => VertexTriple { a: self.b, b: self.c, c: self.a },
            4 => VertexTriple { a: self.c, b: self.a, c: self.b },
            5 => VertexTriple { a: self.c, b: self.b, c: self.a },
            _ => panic!("This should be impossible, and yet here we are")
        }
    }

    pub fn print(&self) {
        print!("({}, {}, {}) ", self.a, self.b, self.c)
    }
}

impl EdgeState {
    /**
     * Increases to the next one. Returns true if rollover.
     */
    pub fn incr_inplace(&mut self) -> bool {
        use EdgeState::*;
        *self = match self {
            NONE => AB,
            AB => BC,
            BC => AC,
            AC => ALL,
            ALL => NONE,
        };
        *self == NONE
    }
}

/**
 * Get the probabilities of various vertex triples being connected
 * under the arboreal gas.
 */
pub fn print_connectivities(g: &Graph) {
    // println!("Num edges = {}", g.size());
    let mut counts = HashMap::new();
    let indexer = g.get_edge_indexer();
    // We're just going straight in with a huge iteration.
    for filter in g.iter_edge_sets() {
        if g.is_filtered_acyclic(filter, &indexer) {
            let components = g.edge_subset_components(filter, &indexer);
            update_counts(g, &mut counts, &components);
        }
    }

    // println!("(a, b, c)  ( none,   ab ,   bc ,   ac ,  all )");

    for a in g.iter_verts() {
        for b in g.iter_verts() {
            for c in g.iter_verts() {
                if let Some(triple) = VertexTriple::new_ordered(a, b, c) {
                    // triple.print();
                    // counts.get(&triple).unwrap().print();
                    if let Some(count) = counts.get(&triple) {
                        if count.is_balanced() {
                            let probs = Probabilities::of_counts(count);
                            println!("gottem");
                            let line = format!("{:.5},{:.5}", probs.none, probs.all);
                            file_write::write("out", "possible", vec![line], true);
                        }
                    }
                }
            }
        }
    }
}

fn update_counts(g: &Graph, counts: &mut HashMap<VertexTriple, Counts>, components: &VertexVec<Component>) {
    for a in g.iter_verts() {
        for b in g.iter_verts() {
            for c in g.iter_verts() {
                if let Some(triple) = VertexTriple::new_ordered(a, b, c) {
                    if let Some(this_counts) = counts.get_mut(&triple) {
                        this_counts.update(triple, components);
                    } else {
                        let mut new_counts = Counts::new();
                        new_counts.update(triple, components);
                        counts.insert(triple, new_counts);
                    }
                }
            }
        }
    }
}

fn get_hypergraph_from_subcubic(g: &Graph, rng: &mut ThreadRng) -> (Vec<VertexTriple>, Order) {
    fn get_hyper_vertex(indexer: &EdgeIndexer, e: Edge) -> Vertex {
        Vertex::of_usize(indexer.index_unwrap(e))
    }
    let indexer = g.get_edge_indexer();
    let mut next_fake = Vertex::of_usize(g.size());
    let mut edge_list = vec![];
    for v in g.iter_verts() {
        if g.deg[v].equals(3) {
            edge_list.push(VertexTriple::new_unordered(
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][0])),
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][1])),
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][2])),
            ).unwrap().permute(rng))
        } else if g.deg[v].equals(2) {
            let c = next_fake;
            next_fake.incr_inplace();
            edge_list.push(VertexTriple::new_unordered(
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][0])),
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][1])),
                c,
            ).unwrap().permute(rng))
        } else if g.deg[v].equals(1) {
            let b = next_fake;
            next_fake.incr_inplace();
            let c = next_fake;
            next_fake.incr_inplace();
            edge_list.push(VertexTriple::new_unordered(
                get_hyper_vertex(&indexer, Edge::of_pair(v, g.adj_list[v][0])),
                b,
                c,
            ).unwrap().permute(rng))
        } else {
            let a = next_fake;
            next_fake.incr_inplace();
            let b = next_fake;
            next_fake.incr_inplace();
            let c = next_fake;
            next_fake.incr_inplace();
            edge_list.push(VertexTriple::new_unordered(a, b, c).unwrap().permute(rng))
        }
    }
    (edge_list, next_fake.to_order())
}

/**
 * Takes a connected subcubic graph G, converts it to a hypergraph
 * Takes a random Counts (representing probabilities)
 * Tests whether there are positive correlations in the resulting structure
 */
pub fn test_hypergraph(g: &Graph, print_debug: bool) {
    fn f(x: usize, y: usize) -> Edge {
        Edge::of_pair(Vertex::of_usize(x), Vertex::of_usize(y))
    }
    let mut rng = thread_rng();
    let (edge_list, n) = get_hypergraph_from_subcubic(g, &mut rng);
    let num_edges = edge_list.len();

    let probabilities = Probabilities::new_random(&mut rng);

    let mut adj_list = VertexVec::new(n, &vec![]);
    for triple in edge_list.iter() {
        adj_list[triple.a].push(triple.b);
        adj_list[triple.a].push(triple.c);
        adj_list[triple.b].push(triple.a);
        adj_list[triple.b].push(triple.c);
        adj_list[triple.c].push(triple.a);
        adj_list[triple.c].push(triple.b);
    }
    let h = Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special);
    let h_indexer = h.get_edge_indexer();
    let mut correlations: HashMap<(Edge, Edge), PairCounts> = HashMap::new();
    let mut containing_triple = EdgeVec::new_with_indexer(&h_indexer, 0);

    for (i, triple) in edge_list.iter().enumerate() {
        containing_triple[Edge::of_pair(triple.a, triple.b)] = i;
        containing_triple[Edge::of_pair(triple.b, triple.c)] = i;
        containing_triple[Edge::of_pair(triple.a, triple.c)] = i;
    }

    let mut edge_probs = EdgeVec::new_with_indexer(&h_indexer, 0.0);

    let mut states = vec![EdgeState::NONE; num_edges];
    'iter_states: loop {
        let mut filter_for_acyclicity = EdgeSet::new(&h_indexer);
        let mut filter_for_presence = EdgeSet::new(&h_indexer);
        for (j, triple) in edge_list.iter().enumerate() {
            let edges = triple.get_present_edges(states[j], true);
            for e in edges {
                filter_for_acyclicity.add_edge(e, &h_indexer);
            }
            let edges = triple.get_present_edges(states[j], false);
            for e in edges {
                filter_for_presence.add_edge(e, &h_indexer);
            }
        }

        if h.is_filtered_acyclic(filter_for_acyclicity, &h_indexer) {
            // This state counts.
            let p = probabilities.get_product(&states);
            for (i, e1) in h.iter_edges().enumerate() {
                let has_e1 = filter_for_presence.has_edge(e1, &h_indexer);
                let is_e1_full = states[containing_triple[e1]] == EdgeState::ALL;
                'iter_e2: for e2 in h.iter_edges().skip(i + 1) {
                    let print_debug = print_debug && e1 == f(0,1) && e2 == f(1,5);
                    if containing_triple[e1] == containing_triple[e2] {
                        continue 'iter_e2;
                    }
                    let has_e2 = filter_for_presence.has_edge(e2, &h_indexer);
                    let is_e2_full = states[containing_triple[e2]] == EdgeState::ALL;
                    if let Some(counts) = correlations.get_mut(&(e1, e2)) {
                        counts.add(p, has_e1, is_e1_full, has_e2, is_e2_full, print_debug);
                    } else {
                        let mut counts = PairCounts::new();
                        counts.add(p, has_e1, is_e1_full, has_e2, is_e2_full, print_debug);
                        correlations.insert((e1, e2), counts);
                    }
                }
                edge_probs[e1] += p * if is_e1_full { 2.0 / 3.0 } else { 1.0 };
            }
            // println!("State counts: {:?}\n", states);
        } else {
            // println!("State doesn't count: {:?}\n", states);
        }

        let mut i = 0;
        while i < num_edges && states[i].incr_inplace() {
            i += 1;
        }
        if i == num_edges {
            break 'iter_states
        }
    }

    // for e in h.iter_edges() {
    //     println!("{}: {:.5}", e, edge_probs[e]);
    // }

    // Test if there is a positively correlated pair.

    let mut found_positive = false;
    'iter_e1: for (i, e1) in h.iter_edges().enumerate() {
        for e2 in h.iter_edges().skip(i + 1) {
            if let Some(counts) = correlations.get(&(e1, e2)) {
                if counts.is_positively_correlated() {
                    // print!("Counts: ");
                    // counts.print();
                    // h.print();
                    // test_correlation_edges(&h, e1, e2);
                    // print!("Probabilities: ");
                    // probabilities.print();
                    // println!("Wow, positive correlation for pair {} {}.", e1, e2);
                    found_positive = true;
                    let line = format!("{:.5},{:.5}", probabilities.none, probabilities.all);
                    file_write::write("out", "coefficients", vec![line], true);
                    break 'iter_e1;
                }
            }
        }
    }
    // if !found_positive {
    //     println!("Valid probabilities: (all correlations negative)");
    //     probabilities.print();
    // }
}

/**
 * Manually test the correlation by running the process with
 * genuine bond percolation
 */
fn test_correlation_edges(h: &Graph, e1: Edge, e2: Edge) {
    let mut counts = PairCounts::new();
    let indexer = h.get_edge_indexer();
    let mut e1_count = 0;
    let mut e2_count = 0;

    for filter in h.iter_edge_sets() {
        if h.is_filtered_acyclic(filter, &indexer) {
            let has_e1 = filter.has_edge(e1, &indexer);
            let has_e2 = filter.has_edge(e2, &indexer);
            if has_e1 && has_e2 {
                counts.both += 1.0;
            } else if has_e1 {
                counts.left += 1.0;
            } else if has_e2 {
                counts.right += 1.0;
            } else {
                counts.neither += 1.0;
            }
            counts.total += 1.0;
            if has_e1 {
                e1_count += 1;
            }
            if has_e2 {
                e2_count += 1;
            }
        }
    }

    println!("\nRaw counts: e1 = {}, e2 = {}", e1_count, e2_count);

    println!("Directly:");
    counts.print();
}