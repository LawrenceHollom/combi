use crate::entity::graph::*;

/**
 * This file deals with structural constructions, based on other
 * graphs, for example the giant component of G(n,p) or the
 * 2-core of a graph
 */

/**
 * Returns a graph isomorphic to the largest component of g.
 */
pub fn giant_component(g: Graph) -> Graph {
    let components = g.components();
    
}

/**
 * Returns a graph isomorphic to the 2-core of the given graph.
 * It is assumed that the given graph is connected.
 */
pub fn two_core(g: Graph) -> Graph {

}