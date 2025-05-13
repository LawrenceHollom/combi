use std::{
    io::Write,
    ops::{Add, AddAssign},
};

use crate::entity::graph::*;

use rand::{rngs::ThreadRng, thread_rng, Rng};
use utilities::{rational::Rational, vertex_tools::*};

const REPS: usize = 100;
const GREEDINESS: usize = 1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct Weight(u32);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Grabbed {
    alice: Weight,
    bob: Weight,
}

impl Add for Weight {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self(self.0 + rhs.0)
    }
}

impl AddAssign for Weight {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl Grabbed {
    const ZERO: Self = Self {
        alice: Weight(0),
        bob: Weight(0),
    };

    #[allow(dead_code)]
    fn new(alice: Weight, bob: Weight) -> Self {
        Self { alice, bob }
    }

    fn add_immutable(&self, w: Weight, is_alice_turn: bool) -> Self {
        if is_alice_turn {
            Self {
                alice: self.alice + w,
                bob: self.bob,
            }
        } else {
            Self {
                alice: self.alice,
                bob: self.bob + w,
            }
        }
    }

    fn has_won(&self, total: Weight, is_alice_turn: bool) -> bool {
        if is_alice_turn {
            self.alice.0 >= total.0.div_ceil(2)
        } else {
            self.bob.0 >= (total.0 + 2) / 2
        }
    }

    fn get(&self, is_alice_turn: bool) -> Weight {
        if is_alice_turn {
            self.alice
        } else {
            self.bob
        }
    }

    fn diff(&self) -> i64 {
        (self.alice.0 as i64) - (self.bob.0 as i64)
    }
}

fn print_weighting(w: &VertexVec<Weight>) {
    print!("Weighting: ");
    for weight in w.iter() {
        print!("{} ", weight.0);
    }
    println!();
}

fn get_random_weighting(
    g: &Graph,
    rng: &mut ThreadRng,
    max_weight: u32,
    zeroes: Option<&VertexVec<bool>>,
) -> VertexVec<Weight> {
    let mut w = VertexVec::new(g.n, &Weight(0));
    for v in g.iter_verts() {
        if zeroes.is_none_or(|zs| !zs[v]) {
            w[v] = Weight(rng.gen_range(0..=max_weight));
        }
    }
    w
}

fn is_weighting_reducable(
    g: &Graph,
    w: &VertexVec<Weight>,
    playable: &VertexVec<bool>,
    parent: &VertexVec<Option<Vertex>>,
) -> bool {
    let mut max_playable_weight = Weight(0);
    let mut is_reducable = false;

    for v in g.iter_verts() {
        if playable[v] {
            max_playable_weight = max_playable_weight.max(w[v]);
        }
    }

    //println!("max_playable_weight: {:?}", max_playable_weight);

    'test_verts: for v in g.iter_verts() {
        if playable[v] && w[v] == max_playable_weight {
            if let Some(u) = parent[v] {
                if w[u] <= w[v] {
                    is_reducable = true;
                    break 'test_verts;
                }
            } else {
                is_reducable = true;
                break 'test_verts;
            }
        }
    }

    is_reducable
}

fn get_random_good_weighting(g: &Graph, rng: &mut ThreadRng, max_weight: u32) -> VertexVec<Weight> {
    let mut playable = VertexVec::new(g.n, &false);
    let mut parent: VertexVec<Option<Vertex>> = VertexVec::new(g.n, &None);
    let mut cocut_coleaf = VertexVec::new(g.n, &false);
    for v in g.iter_verts() {
        if is_complement_connected(g, VertexSet::new(g.n).add_vert_immutable(v)) {
            // The vertex is playable.
            playable[v] = true;
            if g.deg[v].equals(1) {
                parent[v] = Some(g.adj_list[v][0]);
            } else {
                cocut_coleaf[v] = true;
            }
        }
    }

    loop {
        let max_weight = if max_weight <= 1 {
            1
        } else {
            rng.gen_range(2..=max_weight)
        };
        let w = if rng.gen_bool(0.5) {
            get_random_weighting(g, rng, max_weight, None)
        } else if rng.gen_bool(0.1) {
            // Give everything immediately playable weight 0.
            get_random_weighting(g, rng, max_weight, Some(&playable))
        } else {
            // Give cocut non-leaf vertices weight 0.
            get_random_weighting(g, rng, max_weight, Some(&cocut_coleaf))
        };

        if !is_weighting_reducable(g, &w, &playable, &parent) {
            return w;
        } /* else {
              let w2 = playable.iter().map(|play| if *play { Weight(0) });
              if is_weighting_reducable(g, &w2, &playable, &parent) {
                  g.print();
                  g.print_matrix(true);
                  print_weighting(&w2);
                  println!("parents: {:?}", parent);
                  println!("playables: {:?}", playable);
                  panic!("AAAAAAAAAAAAA")
              }
          }*/
    }
}

fn is_complement_connected(g: &Graph, played: VertexSet) -> bool {
    let v = played.not().to_vec();
    let filter = Some(&v);
    g.num_filtered_components(filter) <= 1
}

fn get_playables(g: &Graph, w: &VertexVec<Weight>, played: VertexSet) -> Vec<Vertex> {
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

    if GREEDINESS == 1 {
        // Sort into decreasing order of weight
        playables.sort_by(|u, v| w[*v].cmp(&w[*u]));
    } else if GREEDINESS == 2 {
        let base = g.n.to_usize() as u32;
        let mut goodness = VertexVec::new(g.n, &0);

        // precept 1: score points.
        for (v, w) in w.iter_enum() {
            goodness[v] += 2 * base * base * w.0;
        }

        let mut coleaves = VertexSet::new(g.n);
        let mut parent = VertexVec::new(g.n, &None);
        for v in g.iter_verts() {
            if g.deg[v].equals(1) {
                parent[v] = Some(g.adj_list[v][0]);
            } else {
                coleaves.add_vert(v);
            }
        }
        let is_coleaf_cocutvertex = get_cocutvertices(g, coleaves);
        for v in g.iter_verts() {
            if let Some(u) = parent[v] {
                // precept 2: don't lose points, and play in pairs.
                if w[v] == Weight(0) && w[u] == Weight(0) && is_coleaf_cocutvertex[u] {
                    goodness[v] += base * base;
                }
                // precept 3a: throw the parity with leaves.
                if w[v] == Weight(0) && !is_coleaf_cocutvertex[u] {
                    goodness[v] += 2 * base;
                }
            } else {
                // precept 3b: throw the parity with coleaves.
                goodness[v] += 2 * base;
            }
        }

        // precept 4: play in the minimal coleaf block.
        /*let comps = g.filtered_components(Some(&is_coleaf_cocutvertex));
        let mut comp_sizes = ComponentVec::new(g.n, &0);
        for v in g.iter_verts() {
            if !coleaves.has_vert(v) {
                comp_sizes[comps[parent[v].unwrap()]] += 1;
            } else if is_coleaf_cocutvertex[v] {
                comp_sizes[comps[v]] += 1;
            }
        }
        for v in g.iter_verts() {
            if let Some(u) = parent[v] {
                goodness[u] += base / comp_sizes[comps[u]];
            }
        }*/
        playables.sort_by(|u, v| goodness[*v].cmp(&goodness[*u]));
    }

    playables
}

fn grabbing_game_rec(
    g: &Graph,
    w: &VertexVec<Weight>,
    played: VertexSet,
    num_grabbed: usize,
    grabbed: Grabbed,
    total: Weight,
    debug: bool,
) -> bool {
    let is_alice_turn = num_grabbed % 2 == 0;
    let mut does_alice_win = !is_alice_turn;

    'test_verts: for v in get_playables(g, w, played).iter() {
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
            let this_alice_win =
                grabbing_game_rec(g, w, new_played, num_grabbed + 1, new_grabbed, total, debug);
            if this_alice_win == is_alice_turn {
                // This is a winning move for the current player
                does_alice_win = this_alice_win;
                break 'test_verts;
            }
        }
    }
    if debug {
        println!(
            "{:?}, grabbed = {:?}, A_win: {}",
            played, grabbed, does_alice_win
        );
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
fn grabbing_game_scores(
    g: &Graph,
    w: &VertexVec<Weight>,
    root: Option<Vertex>,
    debug: bool,
    print_strat: bool,
) -> Grabbed {
    let mut score = VertexSetVec::new(g.n, &(None, Grabbed::ZERO));
    let nbhds = g.get_open_nbhds();

    // NB: s is the set of *ungrabbed* vertices, and we move backwards in time.
    for s in g.iter_vertex_subsets() {
        let (back_vertex, grabbed) = score[s];
        if back_vertex.is_some() || s.to_int() == 0 {
            let mut printed_debug_intro = false;
            let mut nbhd = VertexSet::new(g.n);
            let mut cosize = g.n.to_usize();
            if s.to_int() == 0 {
                if let Some(root) = root {
                    nbhd = VertexSet::new(g.n).add_vert_immutable(root)
                } else {
                    nbhd = VertexSet::everything(g.n)
                }
            } else {
                for v in s.iter() {
                    cosize -= 1;
                    nbhd.add_all(nbhds[v]);
                }
                nbhd.remove_all(s);
            }
            for v in nbhd.iter() {
                // These are the vertices that can be ungrabbed while retaining connectivity of ungrabbed set
                let new_set = s.add_vert_immutable(v);
                let is_alice_turn = cosize % 2 == 1;
                let name = if is_alice_turn { "Alice" } else { "Bob" };
                let new_score = grabbed.add_immutable(w[v], is_alice_turn);
                if score[new_set].0.is_none()
                    || new_score.get(is_alice_turn) > score[new_set].1.get(is_alice_turn)
                {
                    // This is better for Bob than old strat, so overwrite.
                    score[new_set] = (Some(v), new_score);
                    if debug && s.to_int() < 300 {
                        if !printed_debug_intro {
                            printed_debug_intro = true;
                            println!("From {} {:?}", s.to_int(), s.to_vec());
                        }
                        println!(
                            "{} better for set {:?}: {:?}",
                            name,
                            new_set.to_vec(),
                            new_score
                        );
                    }
                }
            }
        }
    }

    if print_strat {
        println!("Strategy, in forwards order:");
        let mut s = VertexSet::everything(g.n);
        let mut player = 0;
        while let Some(v) = score[s].0 {
            println!(
                "{} plays {} for {:?}",
                if player == 0 { "Alice" } else { "Bob" },
                v,
                w[v]
            );
            player = (player + 1) % 2;
            s.remove_vert(v);
        }
    }

    score[VertexSet::everything(g.n)].1
}

/**
 * filter (if Some) is true if a vertex should take 0 or 1 weight randomly.
 */
fn get_coleaf_weighting(
    g: &Graph,
    filter: Option<&VertexVec<bool>>,
    max_weight: Weight,
    rng: &mut ThreadRng,
) -> VertexVec<Weight> {
    let mut w = VertexVec::new(g.n, &Weight(0));
    for (v, d) in g.deg.iter_enum() {
        if filter.is_some_and(|f| *f.get(v).unwrap_or(&false)) {
            w[v] = Weight(rng.gen_range(0..=max_weight.0));
        } else if d.more_than(1) {
            w[v] = Weight(rng.gen_range(1..=max_weight.0));
        }
    }
    w
}

pub fn coleaf_weighted_score_difference(g: &Graph) -> Rational {
    let mut rng = thread_rng();
    let w = get_coleaf_weighting(g, None, Weight(1), &mut rng);
    let scores = grabbing_game_scores(g, &w, None, false, false);

    Rational::new(scores.diff())
}

fn get_cutvertices_general(g: &Graph, verts: VertexSet, is_standard_cut: bool) -> VertexVec<bool> {
    let mut out = VertexVec::new(g.n, &false);
    let mut filter = verts.to_vec();
    for v in g.iter_verts() {
        if filter[v] {
            filter[v] = false;
            if g.num_filtered_components(Some(&filter)) > 1 {
                // This is a cutvertex
                if is_standard_cut {
                    out[v] = true;
                }
            } else {
                // This is a cocutvertex
                if !is_standard_cut {
                    out[v] = true;
                }
            }
            filter[v] = true;
        }
    }
    out
}

fn get_cutvertices(g: &Graph, verts: VertexSet) -> VertexVec<bool> {
    get_cutvertices_general(g, verts, true)
}

fn get_cocutvertices(g: &Graph, verts: VertexSet) -> VertexVec<bool> {
    get_cutvertices_general(g, verts, false)
}

pub fn hypothesis_testing(g_in: &Graph) {
    let mut rng = thread_rng();
    let mut adj_list = VertexVec::new(g_in.n, &vec![]);
    for (u, v) in g_in.iter_pairs() {
        if g_in.adj[u][v] {
            adj_list[u].push(v);
            adj_list[v].push(u);
        }
    }

    let is_cutvertex = get_cutvertices(g_in, VertexSet::everything(g_in.n));

    let mut v = Vertex::of_usize(g_in.n.to_usize());
    let mut order = g_in.n.to_usize();
    for u in g_in.iter_verts() {
        if !is_cutvertex[u] {
            adj_list[u].push(v);
            adj_list[v].push(u);
            v.incr_inplace();
            order += 1;
        }
    }
    if order % 2 == 0 {
        let g = Graph::of_adj_list(adj_list, crate::constructor::Constructor::Special);
        let w = get_coleaf_weighting(&g, Some(&is_cutvertex), Weight(1), &mut rng);
        let scores = grabbing_game_scores(&g, &w, None, false, false);
        if scores.diff() < 0 {
            println!("g_in:");
            g_in.print();
            println!("g:");
            g.print();
            print_weighting(&w);
            panic!("Bob won! Bob won!")
        } else {
            for v in g.iter_verts() {
                if g.deg[v].equals(1) {
                    // v is a leaf. Try rooting at v
                    let rooted_score = grabbing_game_scores(&g, &w, Some(v), false, false);
                    if rooted_score.diff() > scores.diff() {
                        println!("Alice prefers leaf-rooted game to original! v = {}", v);
                        println!("g:");
                        g.print();
                        print_weighting(&w);
                        panic!("Hypothesis disproved!");
                    }
                }
            }
        }
    }
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

        if !grabbing_game_rec(g, &w, VertexSet::new(g.n), 0, Grabbed::ZERO, sum(&w), debug) {
            found_good_weighting = true;
            println!("Found Bob-friendly weighting after {} steps", i);
            print_weighting(&w);
            break 'rep;
        }
    }
    found_good_weighting
}

pub fn print_bob_win_weighting(g: &Graph) {
    let mut max_weight = g.n.to_usize() as u32;
    let mut rng = thread_rng();
    let mut w = get_coleaf_weighting(g, None, Weight(1), &mut rng);
    let mut debug = false;
    let mut count = 0;
    loop {
        count += 1;
        if count < 200_000 && count % 1000 == 0 {
            print!("{}k ", count / 1000);
            std::io::stdout().flush().unwrap();
        } else if count % 100_000 == 0 {
            print!("{}.{}M ", count / 1_000_000, (count / 100_000) % 10);
            std::io::stdout().flush().unwrap();
        }
        let total = sum(&w);
        let played = VertexSet::new(g.n);
        if !grabbing_game_rec(g, &w, played, 0, Grabbed::ZERO, total, debug) {
            max_weight = w.max(&Weight(0), Weight::cmp).unwrap().0;
            println!("Weighting (max_weight = {}): {:?}", max_weight, w);
            println!(
                "  Scores: {:?}",
                grabbing_game_scores(g, &w, None, false, false)
            );
            max_weight -= 1;
        }
        w = get_random_good_weighting(g, &mut rng, max_weight);
        debug = false;
    }
}

fn test_num_degs(g: &Graph, set: VertexSet, target_nums: &[usize], allow_bigger: bool) -> bool {
    let mut is_good = true;
    let mut nums = vec![0; target_nums.len()];
    'test_degs: for v in set.iter() {
        let mut d = 0;
        for u in g.adj_list[v].iter() {
            if set.has_vert(*u) {
                d += 1;
                if d >= target_nums.len() && !allow_bigger {
                    is_good = false;
                    break 'test_degs;
                }
            }
        }
        if d < target_nums.len() {
            nums[d] += 1;
            if nums[d] > target_nums[d] {
                return false;
            }
        }
    }

    for (i, target) in target_nums.iter().enumerate() {
        if nums[i] != *target {
            is_good = false;
        }
    }
    is_good
}

fn get_internal_adj_list(g: &Graph, set: VertexSet) -> VertexVec<Vec<Vertex>> {
    let mut internal_adj_list = VertexVec::new(g.n, &vec![]);
    for v in set.iter() {
        for u in g.adj_list[v].iter() {
            if set.has_vert(*u) {
                internal_adj_list[v].push(*u);
            }
        }
    }
    internal_adj_list
}

fn has_corona_like_structure(g: &Graph, set: VertexSet) -> bool {
    let order = set.size();
    let mut is_corona_like = test_num_degs(g, set, &[0, order / 2, 0, order / 2], false);

    if is_corona_like {
        // We actually need to test it properly now.
        let internal_adj_list = get_internal_adj_list(g, set);
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
    }

    is_corona_like
}

fn has_semicorona_like_structure(g: &Graph, set: VertexSet) -> bool {
    let order = set.size();
    let mut is_corona_like = test_num_degs(g, set, &[0, 3, order - 6, 3], false);

    if is_corona_like {
        // STP that each vtx of deg 3 has nbrs of degs 1 and 3, and that it's connected.
        let internal_adj_list = get_internal_adj_list(g, set);
        let mut start_verts = None;
        'test_verts: for v in set.iter() {
            if internal_adj_list[v].len() == 3 {
                let mut has_leaf = false;
                let mut has_cubic = false;
                for u in internal_adj_list[v].iter() {
                    if internal_adj_list[*u].len() == 1 {
                        has_leaf = true;
                    } else if internal_adj_list[*u].len() == 3 {
                        has_cubic = true;
                    } else {
                        start_verts = Some((*u, v));
                    }
                }
                if !(has_leaf && has_cubic) {
                    is_corona_like = false;
                    break 'test_verts;
                }
            }
        }
        if is_corona_like {
            if let Some((x, y)) = start_verts {
                let mut len = 3;
                let mut v = x;
                let mut prev = y;
                while internal_adj_list[v].len() == 2 {
                    if internal_adj_list[v][0] == prev {
                        prev = v;
                        v = internal_adj_list[v][1];
                    } else {
                        prev = v;
                        v = internal_adj_list[v][0];
                    }
                    len += 1;
                }
                if len % 2 == 0 {
                    is_corona_like = false;
                }
            } else if order > 6 {
                is_corona_like = false;
            }
        }
    }

    is_corona_like
}

fn has_filled_semicorona_like_structure(g: &Graph, set: VertexSet) -> bool {
    let order = set.size();
    let mut is_corona_like = test_num_degs(g, set, &[0, 3, 0, order - 4], true);

    if is_corona_like {
        // STP that is non-cutvtx of deg = (order - 3), all of whose nbrs bar one have deg 3.
        let internal_adj_list = get_internal_adj_list(g, set);
        let mut big = None;
        let mut leaf = None;
        'test_verts: for v in set.iter() {
            if internal_adj_list[v].len() == order - 3 {
                big = Some(v);
                for u in internal_adj_list[v].iter() {
                    if internal_adj_list[*u].len() == 1 {
                        if leaf.is_some() {
                            is_corona_like = false;
                            break 'test_verts;
                        }
                        leaf = Some(*u);
                    }
                }
            }
        }

        if is_corona_like {
            if let (Some(big), Some(leaf)) = (big, leaf) {
                let supposed_path = set.remove_vert_immutable(big).remove_vert_immutable(leaf);
                let num_comps = g.num_filtered_components(Some(&supposed_path.to_vec()));
                if num_comps != 1 {
                    is_corona_like = false;
                }
            } else {
                is_corona_like = false;
            }
        }
    }

    is_corona_like
}

fn has_partially_filled_semicorona_like_structure(g: &Graph, set: VertexSet) -> bool {
    let order = set.size();
    let internal_adj_list = get_internal_adj_list(g, set);
    if let Some(centre) = internal_adj_list.arg_max(&vec![], |l1, l2| l1.len().cmp(&l2.len())) {
        if internal_adj_list[centre].len() <= 3 {
            return false;
        }
        let mut leaf = None;
        let mut prongs = vec![];
        let mut prong_leaves = vec![];
        let mut is_corona_like = true;
        let mut num_leaves = 0;
        for v in set.iter() {
            if v != centre && internal_adj_list[v].len() >= 4 {
                return false;
            } else if internal_adj_list[v].len() == 1 {
                num_leaves += 1;
            }
        }
        if num_leaves != 3 {
            return false;
        }
        'test_nbrs: for v in internal_adj_list[centre].iter() {
            if internal_adj_list[*v].len() == 1 {
                if leaf.is_none() {
                    leaf = Some(*v);
                } else {
                    is_corona_like = false;
                    break 'test_nbrs;
                }
            } else if internal_adj_list[*v].len() == 3 {
                let mut leaf = None;
                for u in internal_adj_list[*v].iter() {
                    if internal_adj_list[*u].len() == 1 {
                        if leaf.is_none() {
                            leaf = Some(*u);
                        } else {
                            is_corona_like = false;
                            break 'test_nbrs;
                        }
                    }
                }
                if let Some(leaf) = leaf {
                    prongs.push(*v);
                    prong_leaves.push(leaf);
                }
            } else {
                is_corona_like = false;
                break 'test_nbrs;
            }
        }
        if leaf.is_some() {
            if prongs.len() == 2 {
                // Prongs are adjacent to precisely one leaf and the centre.
                let mut cycle_len = 2;
                let mut v = prongs[0];
                let mut prev = prong_leaves[0];
                'go_round_loop: while v != prongs[1] {
                    let nbrs = &internal_adj_list[v];
                    let mut next = None;
                    if nbrs.len() == 3 {
                        // should be adj to prev and centre and next
                        if (nbrs[0], nbrs[1]) == (prev, centre)
                            || (nbrs[0], nbrs[1]) == (centre, prev)
                        {
                            next = Some(nbrs[2])
                        } else if (nbrs[1], nbrs[2]) == (prev, centre)
                            || (nbrs[1], nbrs[2]) == (centre, prev)
                        {
                            next = Some(nbrs[0])
                        } else if (nbrs[0], nbrs[2]) == (prev, centre)
                            || (nbrs[0], nbrs[2]) == (centre, prev)
                        {
                            next = Some(nbrs[1])
                        }
                    } else if nbrs.len() == 2 {
                        // should be adj to only prev and next
                        if nbrs[0] == prev && nbrs[1] != centre {
                            next = Some(nbrs[1])
                        } else if nbrs[0] != centre && nbrs[1] == prev {
                            next = Some(nbrs[0])
                        }
                    }
                    if let Some(next) = next {
                        prev = v;
                        v = next;
                        cycle_len += 1;
                    } else {
                        is_corona_like = false;
                        break 'go_round_loop;
                    }
                }
                if cycle_len != order - 3 {
                    is_corona_like = false;
                }
            } else {
                is_corona_like = false
            }
        } else {
            is_corona_like = false
        }
        is_corona_like
    } else {
        false
    }
}

// set contains 6 vertices.
fn is_induced_fork(g: &Graph, set: VertexSet) -> bool {
    let mut deg3s = vec![];
    let mut deg2s = vec![];
    let mut leaves = vec![];
    let mut is_fork_like = true;
    let internal_adj_list = get_internal_adj_list(g, set);
    'find_degs: for v in set.iter() {
        if internal_adj_list[v].len() == 1 {
            leaves.push(v)
        } else if internal_adj_list[v].len() == 2 {
            deg2s.push(v)
        } else if internal_adj_list[v].len() == 3 {
            deg3s.push(v)
        } else {
            is_fork_like = false;
            break 'find_degs;
        }
    }

    if deg3s.len() == 1 && deg2s.len() == 2 && leaves.len() == 3 {
        'check_deg_2s: for v in deg2s.iter() {
            let mut nbr_deg_sum = 0;
            for u in internal_adj_list[*v].iter() {
                nbr_deg_sum += internal_adj_list[*u].len()
            }
            if nbr_deg_sum != 4 {
                is_fork_like = false;
                break 'check_deg_2s;
            }
        }
    } else if deg3s.len() == 3 && leaves.len() == 3 {
        'check_deg_3s: for v in deg3s.iter() {
            let mut nbr_deg_sum = 0;
            for u in internal_adj_list[*v].iter() {
                nbr_deg_sum += internal_adj_list[*u].len()
            }
            if nbr_deg_sum != 7 {
                is_fork_like = false;
                break 'check_deg_3s;
            }
        }
    } else {
        is_fork_like = false;
    }

    is_fork_like
}

pub fn has_induced_odd_cycle_corona(g: &Graph) -> bool {
    let mut found_corona = false;
    'search_sets: for set in g.iter_vertex_subsets() {
        let size = set.size();
        if size >= 6 && size % 4 == 2 && has_corona_like_structure(g, set) {
            found_corona = true;
            break 'search_sets;
        }
    }
    found_corona
}

pub fn has_induced_odd_cycle_semicorona(g: &Graph) -> bool {
    let mut found_corona = false;
    'search_sets: for set in g.iter_vertex_subsets() {
        let size = set.size();
        if size >= 6
            && size % 2 == 0
            && (has_semicorona_like_structure(g, set)
                || has_filled_semicorona_like_structure(g, set)
                || has_partially_filled_semicorona_like_structure(g, set))
        {
            found_corona = true;
            break 'search_sets;
        }
    }
    found_corona
}

pub fn has_induced_fork(g: &Graph) -> bool {
    let mut found_fork = false;
    'search_sets: for set in g.iter_vertex_subsets() {
        let size = set.size();
        if size == 6 && is_induced_fork(g, set) {
            found_fork = true;
            break 'search_sets;
        }
    }
    found_fork
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constructor::*;
    use utilities::*;

    fn grab(x: u32, y: u32) -> Grabbed {
        Grabbed::new(Weight(x), Weight(y))
    }

    fn weight(v: Vec<u32>) -> VertexVec<Weight> {
        VertexVec::of_vec(v.iter().map(|x| Weight(*x)).collect::<Vec<Weight>>())
    }

    #[test]
    fn test_grabbing_p3() {
        let g = Graph::new_path(Order::of_usize(3));
        let weights = weight(vec![1, 3, 1]);
        assert_eq!(
            grabbing_game_scores(&g, &weights, None, false, false),
            grab(2, 3)
        );
    }

    #[test]
    fn test_grabbing_p4() {
        let g = Graph::new_path(Order::of_usize(4));
        let weights = weight(vec![0, 2, 3, 1]);
        assert_eq!(
            grabbing_game_scores(&g, &weights, None, false, false),
            grab(3, 3)
        );
    }

    #[test]
    fn test_coleaf_weighted_c5_corona() {
        let g = Constructor::of_string("corona(c(5),k(1))")
            .new_entity()
            .as_owned_graph();
        assert_eq!(coleaf_weighted_score_difference(&g), Rational::new(-1));
    }

    #[test]
    fn test_coleaf_weighted_c4_corona() {
        let g = Constructor::of_string("corona(c(4),k(1))")
            .new_entity()
            .as_owned_graph();
        assert_eq!(coleaf_weighted_score_difference(&g), Rational::new(0));
    }

    #[test]
    fn test_corona_counterexample() {
        let g = Constructor::of_string("corona(c(5),k(1))")
            .new_entity()
            .as_owned_graph();
        let weights = weight(vec![1, 2, 1, 0, 0, 0, 1, 0, 0, 0]);
        assert_eq!(
            grabbing_game_scores(&g, &weights, None, false, false),
            grab(2, 3)
        );
    }
}
