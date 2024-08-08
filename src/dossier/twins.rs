use crate::entity::*;
use crate::entity::poset::*;
use crate::entity::graph::*;
use crate::entity::digraph::*;

use utilities::vertex_tools::*;

/**
 * Elements are twin elements if they cannot be distinguished by their
 * neighbours; e.g. they cover and are covered by the same elements,
 * or have the same adj, or similar.
 * 
 * Algorithms could probably be n log n by e.g. sorting a list of vertex sets.
 */

fn poset_has_twin_elements(p: &Poset) -> bool {
    for (u, v) in p.iter_pairs() {
        if p.upper_covers[u] == p.upper_covers[v] && p.lower_covers[u] == p.lower_covers[v] {
            return true
        }
    }
    false
}

fn graph_has_twin_elements(g: &Graph) -> bool {
    for (u, v) in g.iter_pairs() {
        if g.adj_list[u].len() != g.adj_list[v].len() {
            continue
        }
        let mut u_nbrs = VertexSet::new(g.n);
        let mut v_nbrs = VertexSet::new(g.n);
        for x in g.adj_list[u].iter() {
            u_nbrs.add_vert(*x);
        }
        for x in g.adj_list[v].iter() {
            v_nbrs.add_vert(*x);
        }
        if u_nbrs == v_nbrs {
            return true
        }
    }
    false
}

fn digraph_has_twin_elements(d: &Digraph) -> bool {
    for (u, v) in d.iter_pairs() {
        if d.out_adj_list[u].len() != d.out_adj_list[v].len() || d.in_adj_list[u].len() != d.in_adj_list[v].len() {
            continue
        }
        let mut u_in_nbrs = VertexSet::new(d.n);
        let mut v_in_nbrs = VertexSet::new(d.n);
        let mut u_out_nbrs = VertexSet::new(d.n);
        let mut v_out_nbrs = VertexSet::new(d.n);
        for x in d.in_adj_list[u].iter() {
            u_in_nbrs.add_vert(*x);
        }
        for x in d.in_adj_list[v].iter() {
            v_in_nbrs.add_vert(*x);
        }
        for x in d.out_adj_list[u].iter() {
            u_out_nbrs.add_vert(*x);
        }
        for x in d.out_adj_list[v].iter() {
            v_out_nbrs.add_vert(*x);
        }
        if u_in_nbrs == v_in_nbrs && u_out_nbrs == v_out_nbrs {
            return true
        }
    }
    false
}

pub fn has_twin_elements(e: &Entity) -> bool {
    use Entity::*;
    match e {
        Graph(g) => graph_has_twin_elements(g),
        Digraph(d) => digraph_has_twin_elements(d),
        Poset(p) => poset_has_twin_elements(p),
    }
}

/**
 * x and y are almost twins if they have the same downsets, and one upset minus the other is a
 * chain (and vice versa), or the other way up.
 * This is a little bit slow due to all the iterating.
 * If it's a sticking point then could probably be optimised significantly.
 */
pub fn has_almost_twin_elements(p: &Poset) -> bool {
    for (u, v) in p.iter_pairs() {
        if p.upper_covers[u] == p.upper_covers[v] {
            // We need that the downsets subtract to give chains
            let x = p.downsets[u].setminus(p.downsets[v]);
            let y = p.downsets[v].setminus(p.downsets[u]);
            if p.is_chain(x) && p.is_chain(y) {
                return true
            }
        } else if p.lower_covers[u] == p.lower_covers[v] {
            // We need that the upsets subtract to give chains
            let x = p.upsets[u].setminus(p.upsets[v]);
            let y = p.upsets[v].setminus(p.upsets[u]);
            if p.is_chain(x) && p.is_chain(y) {
                return true
            }
        }
    }
    false
}