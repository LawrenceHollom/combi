use crate::entity::graph::*;

/**
 * Simulate the percolation probabilities
 * P(0 <-> b) and P(0 <-> A) min {P(a <-> b) : a in A}
 * Might as well add any and all points to A that don't decrease the min
 * and so there are only n different choices of A that we need to test.
 * 
 * We assume that p = 1/2 because sometimes life needs to be easy.
 */
pub fn print(g: &Graph) {
    
}