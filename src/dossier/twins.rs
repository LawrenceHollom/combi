use crate::entity::*;
use crate::entity::poset::*;

/**
 * Elements are twin elements if they cannot be distinguished by their
 * neighbours; e.g. they cover and are covered by the same elements,
 * or have the same adj, or similar.
 * 
 * Algorithms could probably be n log n by e.g. sorting a list of vertex sets.
 */

fn poset_has_twin_elements(p: &Poset) -> bool {
    for (u, v) in p.iter_pairs() {
    }
}

fn graph_has_twin_elements(g: &Graph) -> bool {

}

fn digraph_has_twin_elements(g: &Digraph) -> bool {

}

pub fn has_twin_elements(e: &Entity) -> bool {
    use Entity::*;
    match e {
        Graph(g) => graph_has_twin_elements(g),
        Digraph(d) => digraph_has_twin_elements(d),
        Poset(p) => poset_has_twin_elements(p),
    }
}

pub fn has_almost_twin_elements(p: &Poset) -> bool {

}