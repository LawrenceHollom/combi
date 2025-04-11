use std::f64::consts::PI;

use rand::rngs::ThreadRng;
use rand::{thread_rng, Rng};
use utilities::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use crate::entity::graph::*;

use super::pretty;
const PRINT_FREQ: usize = 100;
const EPS: f64 = 0.00000001;

/**
 * Stores the angles used in the Kuramoto model.
 */
#[derive(Clone)]
struct Theta {
    n: Order,
    theta: VertexVec<f64>,
}

impl Theta {
    pub const DELTA: f64 = 0.25;

    pub fn new_random(n: Order, rng: &mut ThreadRng) -> Theta {
        let mut theta = VertexVec::new(n, &0.0);
        for x in theta.iter_mut() {
            *x = rng.gen_range(0.0..(2.0 * PI));
        }
        Theta { n, theta }
    }

    pub fn new_mapped(&self, n: Order, map: VertexVec<Vertex>) -> Theta {
        let mut theta = VertexVec::new(n, &0.0);
        for i in n.iter_verts() {
            theta[i] = self.theta[map[i]];
        }
        Theta { n, theta }
    }

    /**
     * Returns the potential energy in the given edge
     */
    pub fn edge_energy(&self, e: Edge) -> f64 {
        let x1 = self.theta[e.fst()];
        let x2 = self.theta[e.snd()];
        1.0 - (x1 - x2).cos()
    }

    /**
     * Returns the total potential energy in the whole graph.
     */
    pub fn total_energy(&self, edges: &Vec<Edge>) -> f64 {
        let mut total_energy = 0.0;
        for e in edges.iter() {
            total_energy += self.edge_energy(*e)
        }
        total_energy
    }

    pub fn run_simulation_step(&mut self, edges: &Vec<Edge>) -> f64 {
        let mut new_theta = VertexVec::new(self.n, &0.0);
        for (v, ang) in self.theta.iter_enum() {
            new_theta[v] = *ang;
        }

        for e in edges.iter() {
            new_theta[e.fst()] -= Self::DELTA * (self.theta[e.fst()] - self.theta[e.snd()]).sin();
            new_theta[e.snd()] -= Self::DELTA * (self.theta[e.snd()] - self.theta[e.fst()]).sin();
        }

        let mut max_abs_diff = 0.0;

        for (v, new_ang) in new_theta.iter_enum() {
            let abs_diff = (*new_ang - self.theta[v]).abs();
            if abs_diff > max_abs_diff {
                max_abs_diff = abs_diff;
            }
            if *new_ang < -EPS {
                self.theta[v] = *new_ang + 2.0 * PI;
            } else if *new_ang > 2.0 * PI + EPS {
                self.theta[v] = *new_ang - 2.0 * PI;
            } else {
                self.theta[v] = *new_ang;
            }
        }

        max_abs_diff
    }

    /**
     * Picks an arbitrary starting point, and checks if all other angles are within PI / 4 - EPS
     * of this point. If they are, then all angles are strictly within a half-circle, and so
     * the arrangement must be globally synchronising.
     */
    pub fn is_synchronised(&self) -> bool {
        let start_angle = self.theta[Vertex::ZERO];
        let mut is_synchronised = true;
        'test_angles: for angle in self.theta.iter().skip(1) {
            let diff1 = (start_angle - *angle).abs();
            let diff2 = 2.0 * PI - diff1;
            if diff1.min(diff2) > (PI / 2.0) - EPS {
                is_synchronised = false;
                break 'test_angles
            }
        }
        is_synchronised
    }

    pub fn print(&self) {
        println!("Printing state:");
        for (v, ang) in self.theta.iter_enum() {
            println!("{}: {:.5}", v, *ang);
        }
    }
}

/**
 * A basic simulation, with u.a.r. starting point.
 */
pub fn simulate(g: &Graph) {
    let mut rng = thread_rng();
    let mut theta = Theta::new_random(g.n, &mut rng);

    let mut i = 0;
    let edges = g.iter_edges().collect::<Vec<Edge>>();

    // Could slightly optimise this by iterating over a list of edges.
    loop {
        theta.run_simulation_step(&edges);

        i += 1;
        if i % PRINT_FREQ == 0 {
            theta.print();
        }
        if i > 2000 {
            break;
        }
    }
}
/**
 * Tests whether any of [attempts] random starting configurations fails to synchronise.
 */
pub fn does_random_config_synchronise(g: &Graph, attempts: usize) -> bool {
    let mut rng = thread_rng();
    let edges = g.iter_edges().collect::<Vec<Edge>>();
    let mut theta = Theta::new_random(g.n, &mut rng);
    let mut out = true;
    
    'attempt: for _j in 0..attempts {
        theta = Theta::new_random(g.n, &mut rng);
        let mut motion = 1.0;
        while motion > 0.01 * Theta::DELTA / (g.n.to_usize() as f64) {
            motion = theta.run_simulation_step(&edges);
        }
        if !theta.is_synchronised() {
            out = false;
            break 'attempt;
        }
    }
    pretty::print_graph_hued(g, &theta.theta, 2.0 * PI);
    out

}

fn pretty_print_important_bit(g: &Graph, theta: &Theta) {
    let mut is_important = VertexVec::new(g.n, &false);

    // First compute which vertices are involved in high-energy edges.
    let energy_cutoff = 0.12;
    for e in g.iter_edges() {
        if theta.edge_energy(e) > energy_cutoff {
            is_important[e.fst()] = true;
            is_important[e.snd()] = true;
        }
    }

    // Now build the restricted graph.
    let (new_g, map) = g.of_filtered_with_map(&is_important);
    let new_theta = theta.new_mapped(new_g.n, map);

    pretty::print_graph_hued(&new_g, &new_theta.theta, 2.0 * PI);
}

/**
 * Returns true if all tests synchronise
 */
pub fn find_simplest_unsynchronised(g: &Graph, attempts: usize) -> bool {
    let mut rng = thread_rng();
    let edges = g.iter_edges().collect::<Vec<Edge>>();
    let mut theta;
    let mut simplest_theta = None;
    let mut minimal_energy = f64::MAX;
    
    for j in 0..attempts {
        theta = Theta::new_random(g.n, &mut rng);
        let mut motion = 1.0;
        while motion > 0.005 * Theta::DELTA / (g.n.to_usize() as f64) {
            motion = theta.run_simulation_step(&edges);
        }
        // Things have now settled down
        if !theta.is_synchronised() {
            // This is a stable, non-synchronised state.
            let energy = theta.total_energy(&edges);
            println!("Found unsync'd, \t attempt {} / {}, \t energy = {:.2}", j, attempts, energy);
            if energy < minimal_energy {
                minimal_energy = energy;
                simplest_theta = Some(theta.to_owned());
            }
        }
    }
    if let Some(theta) = &simplest_theta {
        println!("Minimal energy = {:.2}", minimal_energy);
        pretty_print_important_bit(g, theta);
    }
    simplest_theta.is_none()

}