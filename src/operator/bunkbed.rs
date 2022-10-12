use crate::graph::*;
use crate::operator::percolate;

pub fn print_polynomials(g: &Graph) {
    let bunkbed = g.bunkbed();
    percolate::percolate(&bunkbed)
}