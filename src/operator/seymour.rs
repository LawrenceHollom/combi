use rand::Rng;
use rand::rngs::ThreadRng;
use rand::thread_rng;

use utilities::{vertex_tools::*, edge_tools::*};

use crate::graph::*;
use crate::digraph::*;

const MAX_ATTEMPTS_MULT: usize = 10;
const NUM_TESTS: usize = 100;

fn must_trivially_have_seymour_vertex(g: &Graph) -> bool {
    for v in g.iter_verts() {
        if g.deg[v].at_most(5) {
            return true
        }
    }
    false
}

fn fix_vertex(d: &mut Digraph, rng: &mut ThreadRng, v: Vertex, avoid: Option<Vertex>, depth: usize) {
    let mut flip_to = None;
    let mut recurse = false;
    if depth > 100 {
        panic!("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    }
    'find_good_flipper: for u in d.in_adj_list[v].iter() {
        if d.out_deg[*u].at_least(4) && avoid.map_or(true, |x| x != *u) {
            // We can use u
            flip_to = Some(*u);
            break 'find_good_flipper;
        }
    }
    if flip_to.is_none() {
        // Maybe should target bigger-out-deg vertices?
        let mut u;
        'find_random_flipper: loop {
            u = d.in_adj_list[v][rng.gen_range(0..d.in_deg[v].to_usize())];
            if avoid.map_or(true, |x| x != u) {
                break 'find_random_flipper;
            }
        }
        recurse = true;
        flip_to = Some(u);
    }
    let u = flip_to.unwrap();
    d.reverse_edge(Edge::of_pair(u, v));
    if recurse {
        fix_vertex(d, rng, u, Some(v), depth+1)
    }
}

fn construct_candidate_orientation(g: &Graph) -> Option<Digraph> {
    let mut d = Digraph::random_orientation(g);
    let n = g.n;
    let mut is_good = false;
    let mut attempts = 0;
    let mut rng = thread_rng();
    'fix_problems: loop {
        attempts += 1;
        let mut all_good = true;

        for v in n.iter_verts() {
            if d.out_deg[v].less_than(3) {
                // This needs fixing.
                all_good = false;
                fix_vertex(&mut d, &mut rng, v, None, 0);
            }
        }
        if all_good {
            is_good = true;
            break 'fix_problems;
        }

        if attempts > MAX_ATTEMPTS_MULT * n.to_usize() {
            break 'fix_problems;
        }
    }
    if is_good {
        Some(d)
    } else {
        None
    }
}

fn does_digraph_have_seymour_vertex(d: &Digraph) -> bool {
    for u in d.iter_verts() {
        let mut found = VertexVec::new(d.n, &false);
        let mut second_nbhd_size = 0;
        for v in d.out_adj_list[u].iter() {
            for w in d.out_adj_list[*v].iter() {
                if !found[*w] && !d.adj[u][*w] {
                    found[*w] = true;
                    second_nbhd_size += 1;
                }
            }
        }
        if d.out_deg[u].at_most(second_nbhd_size) {
            return true
        }
    }
    false
}

pub fn has_seymour_vertex(g: &Graph) -> bool {
    if !must_trivially_have_seymour_vertex(g) {
        for _i in 0..NUM_TESTS {
            if let Some(d) = construct_candidate_orientation(g) {
                if !does_digraph_have_seymour_vertex(&d) {
                    return false
                }
            }
        }
    }
    true
}