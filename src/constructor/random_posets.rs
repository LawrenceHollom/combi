use rand::{rngs::ThreadRng, thread_rng, Rng};

use utilities::*;
use utilities::vertex_tools::*;

use crate::constructor::*;

/**
 * Performs the Fisher-Yates shuffle to produce a permutation u.a.r.
 * If prob is < 1.0, then we fail to perform some swaps, weighting towards the identity.
 */
fn random_permutation(rng: &mut ThreadRng, order: Order, prob: f64) -> VertexVec<Vertex> {
    let mut perm = VertexVec::new_fn(order, |v| v);
    let n = order.to_usize();

    for i in 0..(n-1) {
        if rng.gen_range(0.0..1.0) <= prob {
            let j = i + rng.gen_range(0..(n-i));
            let u = Vertex::of_usize(i);
            let v = Vertex::of_usize(j);
            (perm[u], perm[v]) = (perm[v], perm[u]);
        }
    }

    perm
}

pub fn new_scrambled_order(order: Order, prob: f64) -> VertexVec<VertexVec<bool>> {
    let mut rng = thread_rng();
    let perm = self::random_permutation(&mut rng, order, prob);
    VertexVec::new_fn(order, |u| VertexVec::new_fn(order, |v| perm[u] > perm[v]))
}

pub fn get_ordering_from_weighted_random_intersection(order: Order, k: usize, prob: f64) -> VertexVec<VertexVec<bool>> {
    let mut gt = VertexVec::new_fn(order, 
        |u| VertexVec::new_fn(order, |v| u > v));
    for _i in 0..k-1 {
        let ht = self::new_scrambled_order(order, prob);
        gt = VertexVec::new_fn(order, |u| VertexVec::new_fn(order, |v| gt[u][v] && ht[u][v]))
    }
    gt
}

pub fn new_random_intersection(order: Order, k: usize) -> Poset {
    let gt = get_ordering_from_weighted_random_intersection(order, k, 1.0);
    Poset::of_ordering(gt, Constructor::PosetConstr(PosetConstructor::ChainIntersection(order, k)))
}

/**
 * Take the intersection of k random permutations of chains (the first of which is the identity).
 * However, we retain some correlation between the permutations
 */
pub fn new_correlated_intersection(order: Order, k: usize, prob: f64) -> Poset {
    let gt = get_ordering_from_weighted_random_intersection(order, k, prob);
    Poset::of_ordering(gt, Constructor::PosetConstr(PosetConstructor::CorrelatedIntersection(order, k, prob)))
}