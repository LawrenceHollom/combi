use crate::graph::*;

use super::domination::*;

pub fn print_signature(g: &Graph) {
    println!("gamma: {}", domination_number(g));
}