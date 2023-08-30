use std::ops::{Add, AddAssign};

use crate::graph::*;

use rand::{rngs::ThreadRng, thread_rng, Rng};
use utilities::vertex_tools::*;

const REPS: usize = 100;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

fn get_random_weighting(g: &Graph, rng: &mut ThreadRng, max_weight: u32) -> VertexVec<Weight> {
    let mut w = VertexVec::new(g.n, &Weight(0));
    for v in g.iter_verts() {
        w[v] = Weight(rng.gen_range(0..=max_weight));
    }
    w
}

fn is_weighting_inductable(g: &Graph, w: &VertexVec<Weight>) -> bool {
    let mut playable = VertexVec::new(g.n, &false);
    let mut parent: VertexVec<Option<Vertex>> = VertexVec::new(g.n, &None);
    let mut max_playable_weight = Weight(0);
    for v in g.iter_verts() {
        if is_complement_connected(g, VertexSet::new(g.n).add_vert_immutable(v)) {
            // The vertex is playable.
            playable[v] = true;
            max_playable_weight = max_playable_weight.max(w[v]);
            if g.deg[v].equals(1) {
                parent[v] = Some(g.adj_list[v][0]);
            }
        }
    }

    let mut is_inductable = true;

    'test_verts: for v in g.iter_verts() {
        if playable[v] && w[v] == max_playable_weight {
            if let Some(u) = parent[v] {
                if w[u] <= w[v] {
                    is_inductable = false;
                    break 'test_verts;
                }
            } else {
                is_inductable = false;
                break 'test_verts;
            }
        }
    }

    is_inductable
}

fn get_random_good_weighting(g: &Graph, rng: &mut ThreadRng, max_weight: u32) -> VertexVec<Weight> {
    loop {
        let w = get_random_weighting(g, rng, max_weight);
        
        if is_weighting_inductable(g, &w) {
            return w
        }
    }
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
    let mut playables = vec![];

    for v in g.iter_verts() {
        if !played.has_vert(v) {
            let new_played = played.add_vert_immutable(v);
            if is_complement_connected(g, new_played) {
                // The play is valid.
                playables.push(v)
            }
        }
    }
    // Sort into decreasing order of weight
    playables.sort_by(|u, v| w[*v].cmp(&w[*u]));

    'test_verts: for v in playables.iter() {
        let new_played = played.add_vert_immutable(*v);
        let new_grabbed = grabbed.add_immutable(w[*v], is_alice_turn);
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

// score_difference = Alice - Bob
fn grabbing_game_scores(g: &Graph, w: &VertexVec<Weight>, debug: bool) -> (Weight, Weight) {
    let mut score = VertexSetVec::new(g.n, &(false, (Weight(0), Weight(0))));
    let nbhds = g.get_open_nbhds();

    for (i, s) in g.iter_vertex_subsets().enumerate() {
        let (reached, (alice_score, bob_score)) = score[s];
        if i == 0 || reached {
            if debug {
                println!("From {:?}", s.to_vec());   
            }
            let mut nbhd = VertexSet::new(g.n);
            let mut cosize = g.n.to_usize();
            if i == 0 {
                nbhd = VertexSet::everything(g.n)
            } else {
                for v in s.iter() {
                    cosize -= 1;
                    nbhd.add_all(nbhds[v]);
                }
                nbhd.remove_all(s);
            }
            for v in nbhd.iter() {
                // These are the vertices that can be added while retaining connectivity
                let new_set = s.add_vert_immutable(v);
                if cosize % 2 == 0 {
                    // Bob's turn
                    let new_score = (alice_score, bob_score + w[v]);
                    if !score[new_set].0 || new_score.1 > score[new_set].1.1 {
                        // This is better for Bob than old strat, so overwrite.
                        score[new_set] = (true, new_score);
                        if debug {
                            println!("Bob better for set {:?}: {:?}", new_set.to_vec(), new_score);
                        }
                    }
                } else {
                    // Alice's turn
                    let new_score = (alice_score + w[v], bob_score);
                    if !score[new_set].0 || new_score.0 > score[new_set].1.0 {
                        score[new_set] = (true, new_score);
                        if debug {
                            println!("Alice better for set {:?}: {:?}", new_set.to_vec(), new_score);
                        }
                    }
                }
            }
        }
    }

    score[VertexSet::everything(g.n)].1
}

pub fn can_bob_win_graph_grabbing(g: &Graph, max_weight: Option<usize>) -> bool {
    if g.min_degree().at_least(2) {
        return false;
    }
    let mut rng = thread_rng();
    let mut found_good_weighting = false;
    let max_weight = max_weight.unwrap_or(g.n.to_usize()) as u32;
    'rep: for i in 0..REPS {
        let w = get_random_good_weighting(g, &mut rng, max_weight);
        let debug = false;
        let scores = grabbing_game_scores(g, &w, debug);
        if scores.1 > scores.0 {
            found_good_weighting = true;
            println!("Found Bob-friendly weighting after {} steps", i);
            break 'rep
        }
    }
    found_good_weighting
}
pub fn print_bob_win_weighting(g: &Graph) {
    let mut max_weight = g.n.to_usize() as u32;
    let mut rng = thread_rng();
    loop {
        let w = get_random_good_weighting(g, &mut rng, max_weight);
        let total = sum(&w);
        let played = VertexSet::new(g.n);
        if !grabbing_game_rec(g, &w, played, 0, Grabbed::ZERO, total, false) {
            println!("Weighting (max_weight = {}): {:?}", max_weight, w);
            println!("  Scores: {:?}", grabbing_game_scores(g, &w, false));
            max_weight -= 1;
        }
    }
}

fn has_corona_like_structure(g: &Graph, set: VertexSet) -> bool {
    let mut is_corona_like = true;
    'test_degs: for v in set.iter() {
        let mut d = 0;
        'test_nbrs: for u in g.adj_list[v].iter() {
            if set.has_vert(*u) {
                d += 1;
                if d >= 4 {
                    break 'test_nbrs;
                }
            }
        }
        if d != 1 && d != 3 {
            is_corona_like = false;
            break 'test_degs;
        }
    }

    if is_corona_like {
        // We actually need to test it properly now.
        let mut internal_adj_list = VertexVec::new(g.n, &vec![]);
        let mut num_one = 0;
        let mut num_three = 0;
        for v in set.iter() {
            let mut d = 0;
            for u in g.adj_list[v].iter() {
                if set.has_vert(*u) {
                    d += 1;
                    internal_adj_list[v].push(*u);
                }
            }
            if d == 1 {
                num_one += 1;
            } else {
                num_three += 1;
            }
        }
        if num_one == num_three {
            'test_cyclicity: for v in set.iter() {
                if internal_adj_list[v].len() == 3 {
                    let mut num_deg_three_nbrs = 0;
                    for u in internal_adj_list[v].iter() {
                        if internal_adj_list[*u].len() == 3 {
                            num_deg_three_nbrs += 1;
                        }
                    }
                    if num_deg_three_nbrs != 2 {
                        is_corona_like = false;
                        break 'test_cyclicity;
                    }
                }
            }
        } else {
            is_corona_like = false
        }
    }

    is_corona_like
}

pub fn has_induced_odd_cycle_corona(g: &Graph) -> bool {
    let mut found_corona = false;
    'search_sets: for set in g.iter_vertex_subsets() {
        let size = set.size();
        if size >= 6 && size % 4 == 2 {
            if has_corona_like_structure(g, set) {
                found_corona = true;
                break 'search_sets;
            }
        }
    }
    found_corona
}