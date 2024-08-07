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
        Poset::of_ordering(gt, Constructor::PosetConstr(PosetConstructor::Chain(order)))
    }

    pub fn new_random_intersection(order: Order, k: usize) -> Poset {
        let mut gt = VertexVec::new_fn(order, 
            |u| VertexVec::new_fn(order, |v| u > v));
        for _i in 0..k-1 {
            let ht = Self::new_scrambled_order(order);
            gt = VertexVec::new_fn(order, |u| VertexVec::new_fn(order, |v| gt[u][v] && ht[u][v]))
        }
        Poset::of_ordering(gt, Constructor::PosetConstr(PosetConstructor::ChainIntersection(order, k)))
    }

    pub fn new_ladder_cap(rungs: usize, floaters: usize) -> Poset {
        let mut rng = thread_rng();
        let order = Order::of_usize(4 + 2 * rungs + floaters);
        let mut gt = VertexVec::new(order, &VertexVec::new(order, &false));

        for rung in 0..(rungs + 1) {
            let x = Vertex::of_usize(2 * rung);
            let y = Vertex::of_usize(2 * rung + 1);
            gt[x][x.incr_by(2)] = true;
            gt[y][y.incr_by(2)] = true;
            gt[y][y.incr_by(1)] = true;
            if rung < rungs {
                gt[x][x.incr_by(5)] = true;
            }
        }
        // Now shove in the extra stuff at the bottom
        for x in (2 * rungs + 4)..(2 * rungs + 4 + floaters) {
            todo!("Add relations between these bottom things and the rungs, and between them and each other.")
        }

        Poset::of_transitive_closure(gt, Constructor::PosetConstr(PosetConstructor::LadderCap(rungs, floaters)))
    }
}