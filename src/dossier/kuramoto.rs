use std::f64::consts::PI;

use rand::{thread_rng, Rng};
use utilities::vertex_tools::*;

use crate::entity::graph::*;

const DELTA: f64 = 0.01;
const PRINT_FREQ: usize = 100;
const EPS: f64 = 0.00000001;

/**
 * For simulating the Kuramoto model, and global synchronisation.
 */

fn print_theta(theta: &VertexVec<f64>) {
    println!("Printing state:");
    for (v, ang) in theta.iter_enum() {
        println!("{}: {:.5}", v, *ang);
    }
}

/**
 * A basic simulation, with u.a.r. starting point.
 */
pub fn simulate(g: &Graph) {
    let mut theta = VertexVec::new(g.n, &0.0_f64);
    let mut new_theta = VertexVec::new(g.n, &0.0);
    let mut rng = thread_rng();

    // Initialise the angles randomly
    for x in theta.iter_mut() {
        *x = rng.gen_range(0.0..(2.0 * PI));
    }

    let mut i = 0;

    // Could slightly optimise this by iterating over a list of edges.
    loop {
        for (v, ang) in theta.iter_enum() {
            new_theta[v] = theta[v];
            for u in g.adj_list[v].iter() {
                new_theta[v] += DELTA * (theta[*u] - *ang).sin();
            }
        }

        for v in g.iter_verts() {
            if new_theta[v] < -EPS {
                theta[v] = new_theta[v] + 2.0 * PI;
            } else if new_theta[v] > 2.0 * PI + EPS {
                theta[v] = new_theta[v] - 2.0 * PI;
            } else {
                theta[v] = new_theta[v];
            }
        }

        i += 1;
        if i % PRINT_FREQ == 0 {
            print_theta(&theta);
        }
        if i > 2000 {
            break;
        }
    }
}