use rand::{rngs::ThreadRng, thread_rng, Rng};

use utilities::*;
use utilities::vertex_tools::*;

use crate::constructor::*;

use super::Poset;

impl Poset {
    // Performs the Fisher-Yates shuffle to produce a permutation u.a.r.
    fn random_permutation(rng: &mut ThreadRng, order: Order) -> VertexVec<Vertex> {
        let mut perm = VertexVec::new_fn(order, |v| v);
        let n = order.to_usize();

        for i in 0..(n-1) {
            let j = i + rng.gen_range(0..(n-i));
            let u = Vertex::of_usize(i);
            let v = Vertex::of_usize(j);
            (perm[u], perm[v]) = (perm[v], perm[u]);
        }

        perm
    }

    pub fn new_scrambled_order(order: Order) -> VertexVec<VertexVec<bool>> {
        let mut rng = thread_rng();
        let perm = Self::random_permutation(&mut rng, order);
        VertexVec::new_fn(order, |u| VertexVec::new_fn(order, |v| perm[u] > perm[v]))
    }

    pub fn new_scrambled_chain(order: Order) -> Poset {
        let gt = Self::new_scrambled_order(order);
        Poset::of_ordering(gt, Constructor::Raw(RawConstructor::Chain(order)))
    }

    pub fn new_random_intersection(order: Order, k: usize) -> Poset {
        let mut gt = Self::new_scrambled_order(order);
        for _i in 0..k-1 {
            let ht = Self::new_scrambled_order(order);
            gt = VertexVec::new_fn(order, |u| VertexVec::new_fn(order, |v| gt[u][v] && ht[u][v]))
        }
        Poset::of_ordering(gt, Constructor::Random(RandomConstructor::ChainIntersection(order, k)))
    }
}