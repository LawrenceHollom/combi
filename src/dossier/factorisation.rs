use queues::*;

use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};

use utilities::{*, edge_tools::*, vertex_tools::*};
use crate::entity::graph::*;

const DEBUG: bool = false;

/**
 * Build for working with 1-factorisations of the hypercube,
 * in particular, finding the minimal value of r so that there
 * is a 1-factorisation such that the union of any r factors
 * forms a connected component.
 */

struct Factorisation {
    n: Order,
    factors: EdgeVec<usize>
}

impl Factorisation {

    fn new(g: &Graph) -> Factorisation {
        Factorisation { 
            n: g.n, 
            factors: EdgeVec::new(&g.adj_list, 0) }
    }

    /**
     * Run the alternating path algorithm recursively to increase
     * the size of the matching. 
     * v is the currect active vertex
     * covered stores whether the vertex is in an edge of this partial-factor
     * traced stores whether the vertex has been used in this alternation
     * 
     * Returns true if an alternating path is found, and then the alternation
     * is applied on the way back down the DFS tree.
     */
    fn cover_vertex(&mut self, v: Vertex, covered: &mut VertexVec<Option<Vertex>>, traced: &mut BigVertexSet, g: &Graph, rng: &mut ThreadRng, factor: usize) -> bool {
        let mut nbrs = vec![];
        for u in g.adj_list[v].iter() {
            if covered[*u].is_none() && self.factors[Edge::of_pair(*u, v)] == 0 {
                nbrs.push(*u);
            }
        }
        if nbrs.len() > 0 {
            // Can just add an edge directly. No alternations required.
            let u = nbrs[rng.gen_range(0..nbrs.len())];
            let e = Edge::of_pair(v, u);
            self.factors[e] = factor;
            covered[u] = Some(v);
            covered[v] = Some(u);
            return true;
        } else {
            // We're going to need to flip an alternating path.
            // This can be done recursively.
            let mut covered_nbrs = vec![];
            for u in g.adj_list[v].iter() {
                if !traced.has_vert(*u) && self.factors[Edge::of_pair(*u, v)] == 0 {
                    covered_nbrs.push(*u);
                }
            }
            if covered_nbrs.len() == 0 {
                return false;
            } else {
                covered_nbrs.shuffle(rng);
                for u in covered_nbrs.iter() {
                    let e = Edge::of_pair(v, *u);
                    let w = covered[*u].unwrap();
                    traced.add_vert(*u);
                    traced.add_vert(v);
                    self.factors[e] = factor;
                    let f = Edge::of_pair(*u, w);
                    self.factors[f] = 0;
                    let old_covered_w = covered[w];
                    covered[w] = None;
                    // Now recurse to continue the path
                    if self.cover_vertex(w, covered, traced, g, rng, factor) {
                        covered[*u] = Some(v);
                        covered[v] = Some(*u);
                        return true
                    } else {
                        traced.remove_vert(*u);
                        traced.remove_vert(v);
                        covered[w] = old_covered_w;
                        self.factors[e] = 0;
                        self.factors[f] = factor;
                    }
                }
                return false
            }
        }
    }

    fn add_random_one_factor(&mut self, g: &Graph, rng: &mut ThreadRng, factor: usize) {
        let mut covered = VertexVec::new(self.n, &None);
        for v in self.n.iter_verts() {
            if covered[v].is_none() {
                // We need to add an edge to cover this vertex.
                let mut traced = BigVertexSet::new(self.n);
                self.cover_vertex(v, &mut covered, &mut traced, g, rng, factor);
            }
        }
        for v in self.n.iter_verts() {
            let mut count = 0;
            for u in g.adj_list[v].iter() {
                let e = Edge::of_pair(v, *u);
                if self.factors[e] == factor {
                    count += 1;
                }
            }
            if count != 1 {
                println!("oh no. count = {} at v = {}", count, v);
                println!("factor = {}", factor);
                self.print_covered(&covered);
                self.print_factorisation();
                panic!("The one-factorisation isn't a one-factorisation!")
            }
        }
    }

    /**
     * Tests whether the given collection of factors unite to connect
     * the entire graph
     */
    fn is_collection_connected(&self, in_use: &Vec<bool>, g: &Graph) -> bool {
        let mut q: Queue<Vertex> = queue![Vertex::ZERO];
        let mut visited = VertexVec::new(self.n, &false);
        visited[Vertex::ZERO] = true;
        let mut size = 1;
        while let Ok(v) = q.remove() {
            for u in g.adj_list[v].iter() {
                let e = Edge::of_pair(*u, v);
                if self.factors[e] == 0 {
                    println!("wtf?");
                    self.print_factorisation();
                }
                if !visited[*u] && in_use[self.factors[e] - 1] {
                    // We can reach u.
                    let _ = q.add(*u);
                    visited[*u] = true;
                    size += 1;
                }
            }
        }
        if DEBUG {
            println!("size = {}; visited = {:?}", size, visited);
        }
        size == self.n.to_usize()
    }

    /**
     * Returns the min number r such that the union of any r of the factors
     * is connected. Just manually test whether each combination is connected
     * or not.
     * Use a little bit of DP to cut down on processing time.
     */
    fn get_connectedness_cutoff(&self, g: &Graph, degree: usize) -> usize {
        // Do some dynamic programming to store whether old things are connected.
        //let mut is_connected = vec![false; 2_usize.pow(self.n.to_usize() as u32)];
        'test_r: for r in 3..=degree {
            let mut in_use = vec![false; degree];
            let mut size = 0;
            loop {
                let mut i = 0;
                while i < degree && in_use[i] {
                    in_use[i] = false;
                    size -= 1;
                    i += 1;
                }
                if i < degree {
                    in_use[i] = true;
                    size += 1;
                } else {
                    // The sets have all succeeded and so we return r.
                    return r;
                }
                if size == r {
                    if DEBUG {
                        println!("Testing set {:?}", in_use);
                    }
                    if !self.is_collection_connected(&in_use, g) {
                        // This collection is disconnected so we must increase r
                        continue 'test_r
                    }
                }
            }
        }

        return 0
    }

    fn print_covered(&self, covered: &VertexVec<Option<Vertex>>) {
        for (v, u_opn) in covered.iter_enum() {
            if let Some(u) = u_opn {
                if *u > v {
                    println!("\t{} ~ {}", v, *u);
                    if covered[*u] != Some(v) {
                        println!("\t{} ~ {:?} (!?)", *u, covered[*u])
                    }
                }
            } else {
                println!("\t{} ~ None", v);
            }
        }
    }

    fn print_factorisation(&self) {
        println!("Factorisation:");
        let mut colour_counts = vec![0; self.n.to_usize()];
        for (e, colour) in self.factors.iter_enum() {
            println!("({}, {}) = {}", e.fst(), e.snd(), colour);
            colour_counts[*colour] += 1;
        }
        println!("Colour_counts: {:?}", colour_counts);
    }
}

/**
 * Generate a u.a.r. 1-factorisation, and test the 
 * connectivity cutoff.
 */
pub fn randomly_factorise(g: &Graph) -> u32 {
    let mut factorisation = Factorisation::new(g);
    let mut rng = thread_rng();
    let degree = g.adj_list[Vertex::ZERO].len();
    for factor in 1..=degree {
        // Add a random 1-factor, with the given index.
        factorisation.add_random_one_factor(g, &mut rng, factor);
    }

    if DEBUG {
        factorisation.print_factorisation();
    }

    factorisation.get_connectedness_cutoff(g, degree) as u32
}