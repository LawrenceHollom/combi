use std::ops::{Add, AddAssign};

use crate::graph::*;

use rand::{rngs::ThreadRng, thread_rng, Rng};
use utilities::vertex_tools::*;

const REPS: usize = 100;

#[derive(Clone, Copy, Debug)]
struct Weight(u32);

#[derive(Clone, Copy, Debug)]
struct Grabbed {
    alice: Weight,
    bob: Weight,
}

impl Add for Weight {
    type Output = Weight;

    fn add(self, rhs: Self) -> Self::Output {
        Weight(self.0 + rhs.0)
    }
}

impl AddAssign for Weight {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl Grabbed {
    const ZERO: Grabbed = Grabbed { alice: Weight(0), bob: Weight(0) };

    fn add_immutable(&self, w: Weight, is_alice_turn: bool) -> Grabbed {
        if is_alice_turn {
            Grabbed { alice: self.alice + w, bob: self.bob }
        } else {
            Grabbed { alice: self.alice, bob: self.bob + w }
        }
    }

    fn has_won(&self, total: Weight, is_alice_turn: bool) -> bool {
        if is_alice_turn {
            self.alice.0 >= (total.0 + 1) / 2
        } else {
            self.bob.0 >= (total.0 + 2) / 2
        }
    }
}

/**
 * TODO: make the weighting smart so there's no immediately-inductable Alice play.
 */
fn get_random_weighting(g: &Graph, rng: &mut ThreadRng) -> VertexVec<Weight> {
    let mut w = VertexVec::new(g.n, &Weight(0));
    for v in g.iter_verts() {
        w[v] = Weight(rng.gen_range(0..g.n.to_usize()) as u32);
    }
    w
}

fn is_complement_connected(g: &Graph, played: VertexSet) -> bool {
    let v = played.not().to_vec();
    let filter = Some(&v);
    g.num_filtered_components(filter) <= 1
}

fn grabbing_game_rec(g: &Graph, w: &VertexVec<Weight>, played: VertexSet, num_grabbed: usize, 
        grabbed: Grabbed, total: Weight, debug: bool) -> bool {
    let is_alice_turn = num_grabbed % 2 == 0;
    let mut does_alice_win = !is_alice_turn;
    'test_verts: for v in g.iter_verts() {
        if !played.has_vert(v) {
            let new_played = played.add_vert_immutable(v);
            if is_complement_connected(g, new_played) {
                // The play is valid.
                let new_grabbed = grabbed.add_immutable(w[v], is_alice_turn);
                if new_grabbed.has_won(total, is_alice_turn) {
                    // The player has grabbed enough to win
                    if debug { 
                        println!("Auto-win; is_alice_turn = {}", is_alice_turn);
                    }
                    does_alice_win = is_alice_turn;
                    break 'test_verts;
                } else {
                    // We need to go deeper.
                    let this_alice_win = grabbing_game_rec(g, w, new_played, num_grabbed + 1, new_grabbed, total, debug);
                    if this_alice_win == is_alice_turn {
                        // This is a winning move for the current player
                        does_alice_win = this_alice_win;
                        break 'test_verts;
                    }
                }
            }
        }
    }
    if debug {
        println!("{:?}, grabbed = {:?}, A_win: {}", played, grabbed, does_alice_win);
    }
    does_alice_win
}

fn sum(w: &VertexVec<Weight>) -> Weight {
    let mut sum = Weight(0);
    for x in w.iter() {
        sum += *x
    }
    sum
}

pub fn can_bob_win_graph_grabbing(g: &Graph) -> bool {
    let mut rng = thread_rng();
    let mut found_good_weighting = false;
    'rep: for _i in 0..REPS {
        let w = get_random_weighting(g, &mut rng);
        let total = sum(&w);
        let played = VertexSet::new(g.n);
        let debug = false;
        if !grabbing_game_rec(g, &w, played, 0, Grabbed::ZERO, total, debug) {
            found_good_weighting = true;
            break 'rep
        }
        println!("Rep complete: w = {:?}, total = {:?}, Alice won.", w, total);
    }
    found_good_weighting
}