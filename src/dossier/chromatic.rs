use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::time::SystemTime;

use crate::annotations::Annotations;
use crate::entity::graph::*;

use utilities::chromatic_tools::*;
use utilities::vertex_tools::*;

fn can_be_coloured_rec(
    g: &Graph,
    num_colours: usize,
    max_colour_used: usize,
    colour: &mut VertexVec<Option<usize>>,
    next_vert: Vertex,
) -> bool {
    if next_vert.is_n(g.n) {
        true
    } else {
        let mut can_colour = false;
        'test_all_colourings: for col in 0..usize::min(max_colour_used + 2, num_colours) {
            let mut can_use_this_colour = true;
            'test_this_colour: for (i, edge) in g.adj[next_vert].iter_enum() {
                if *edge && colour[i] == Some(col) {
                    // can't colour that
                    can_use_this_colour = false;
                    break 'test_this_colour;
                }
            }
            if can_use_this_colour {
                colour[next_vert] = Some(col);
                if can_be_coloured_rec(
                    g,
                    num_colours,
                    usize::max(max_colour_used, col),
                    colour,
                    next_vert.incr(),
                ) {
                    can_colour = true;
                    break 'test_all_colourings;
                } else {
                    colour[next_vert] = None;
                }
            }
        }
        can_colour
    }
}

fn can_be_coloured(g: &Graph, num_colours: usize) -> bool {
    let mut colour = VertexVec::new(g.n, &None);
    colour[Vertex::ZERO] = Some(0);
    can_be_coloured_rec(g, num_colours, 0, &mut colour, Vertex::ZERO.incr())
}

pub fn is_bipartite(g: &Graph) -> bool {
    can_be_coloured(g, 2)
}

pub fn chromatic_number(g: &Graph) -> u32 {
    let mut num_colours = 1;
    'find_chromatic_number: loop {
        if can_be_coloured(g, num_colours) {
            break 'find_chromatic_number;
        } else {
            num_colours += 1;
        }
    }
    num_colours as u32
}

pub fn bipartite_side_difference(g: &Graph) -> u32 {
    if !is_bipartite(g) {
        panic!("G must be bipartite to apply BipartiteSideDifference!")
    } else if !g.is_connected() {
        panic!("G must be connected to apply BipartiteSideDifference!")
    } else {
        let mut colour = VertexVec::new(g.n, &None);
        colour[Vertex::ZERO] = Some(0);
        can_be_coloured_rec(g, 2, 0, &mut colour, Vertex::ZERO.incr());
        let mut num_red: i32 = 0;
        let mut num_blue: i32 = 0;
        for v in g.iter_verts() {
            match colour[v] {
                Some(0) => num_red += 1,
                Some(1) => num_blue += 1,
                Some(_) | None => panic!("It's all gone wrong!"),
            }
        }
        (num_red - num_blue).unsigned_abs()
    }
}

/**
 * k: number of colours
 * fast_mode: stop the simulation as soon as we know that a player has won.
 *      If this is false, then we create a complete simulation of the game, even following
 *      winning and losing positions through to the end.
 * can_b_play_duds: if true, then colour k-1 is the 'dud' colour, can only be played
 *      by B, and does not effect the surrounding vertices.
 * must_be_connected: if true then we are playing the connected version of the game.
 */
struct ChromaticParameters {
    k: usize,
    fast_mode: bool,
    can_b_play_duds: bool,
    must_be_connected: bool,
    alice_greedy: bool,
}

/**
 * Run one step of the recursive chromatic game algo where Alice uses a
 * greedy strategy - she colours the vertex with fewest available colours
 * with the minimal indexed colour that is legal to play there.
 */
fn alice_greedy_algo_chromatic_game_step(
    g: &Graph,
    ann: &mut Annotations,
    params: &ChromaticParameters,
    max_colour_used: usize,
    coder: &Coder,
    config: Config,
    history: &mut HashMap<Config, bool>,
    fixed_verts: VertexSet,
    should_find_reps: bool,
    num_cold: usize,
) -> bool {
    // It's Alice's turn and she's using the greedy algo.
    let mut max_num_colours_adj = None;
    let mut best_vert = None;
    let mut best_colour = None;
    for v in g.iter_verts() {
        if coder.get_colour(config, &v).is_none() {
            let mut num_colours_adj = 0;
            let mut colour_found = vec![false; params.k];
            for u in g.adj_list[v].iter() {
                if let Some(c) = coder.get_colour(config, u) {
                    if !colour_found[c] {
                        colour_found[c] = true;
                        num_colours_adj += 1;
                    }
                }
            }
            if num_colours_adj >= params.k {
                // This vertex cannot be coloured.
                return false;
            }
            if max_num_colours_adj.is_none_or(|max| num_colours_adj > max) {
                let mut min_non_adj_colour = 0;
                while colour_found[min_non_adj_colour] {
                    min_non_adj_colour += 1;
                }
                max_num_colours_adj = Some(num_colours_adj);
                best_vert = Some(v);
                best_colour = Some(min_non_adj_colour);
            }
        }
    }
    match (best_vert, best_colour) {
        (Some(vert), Some(colour)) => {
            let max_col = max_colour_used.max(colour);
            let new_config = coder.play_move(config, vert, colour);
            let fixed_verts = fixed_verts.add_vert_immutable(vert);
            alice_wins_chromatic_game_rec(
                g,
                ann,
                params,
                max_col,
                coder,
                new_config,
                history,
                fixed_verts,
                should_find_reps,
                num_cold + 1,
            )
        }
        (_, _) => false,
    }
}

/**
 * Is it the case that every vertex either
 * - is coloured, or
 * - has #(uncoloured_nbrs) + #(distinct adjacent colours) < k
 * (If yes then it is impossible for Breaker to win.)
 */
fn is_every_vertex_safe(
    g: &Graph,
    params: &ChromaticParameters,
    coder: &Coder,
    config: Config,
) -> bool {
    for v in g.iter_verts() {
        if coder.get_colour(config, &v).is_none() {
            let mut num_distinct_colours = 0;
            let mut num_uncoloured = 0;
            let mut colour_found = vec![false; params.k];
            for u in g.adj_list[v].iter() {
                if let Some(c) = coder.get_colour(config, u) {
                    if !colour_found[c] {
                        colour_found[c] = true;
                        num_distinct_colours += 1;
                    }
                } else {
                    num_uncoloured += 1;
                }
            }
            if num_distinct_colours + num_uncoloured >= params.k {
                // This vertex is unsafe, and could be made uncolourable.
                return false;
            }
        }
    }
    true
}

/**
 * Recursively test if Alice can win the chromatic game. Parameters:
 *
 * params: see above.
 * max_colour_used: highest numbered colour used so far
 * config: the state of the board (i.e. how the graph is coloured so far)
 * history: a record of what we already know about winning states
 * fixed_verts: used to get vertex class representatives in the early stages.
 * num_cols: how many vertices have already been coloured
 */
fn alice_wins_chromatic_game_rec(
    g: &Graph,
    ann: &mut Annotations,
    params: &ChromaticParameters,
    max_colour_used: usize,
    coder: &Coder,
    config: Config,
    history: &mut HashMap<Config, bool>,
    fixed_verts: VertexSet,
    should_find_reps: bool,
    num_cold: usize,
) -> bool {
    if g.n.at_least(35) && num_cold <= 15 {
        println!("Step! {}", num_cold);
    }
    if g.n.at_most(num_cold) {
        // G is coloured, so Alice has won
        true
    } else {
        let wlog_index = coder.get_wlog_index(config, params.can_b_play_duds);
        match (
            history.get(&wlog_index),
            num_cold % 2 == 0,
            params.alice_greedy,
        ) {
            (Some(alice_win), _, _) => *alice_win,
            (None, true, true) => alice_greedy_algo_chromatic_game_step(
                g,
                ann,
                params,
                max_colour_used,
                coder,
                config,
                history,
                fixed_verts,
                should_find_reps,
                num_cold,
            ),
            (None, alice_turn, _) => {
                if is_every_vertex_safe(g, params, coder, config) {
                    // No vertex can be made uncolourable.
                    return true;
                }
                let mut alice_win = !alice_turn;
                let mut is_something_playable = false;
                let col_cap = if params.can_b_play_duds && (alice_turn || g.n.at_most(num_cold + 1))
                {
                    (max_colour_used + 2).min(params.k - 1)
                } else {
                    (max_colour_used + 2).min(params.k)
                };
                let mut next_should_find_reps = should_find_reps;
                let reps = if should_find_reps {
                    let reps = ann.get_representatives(g, fixed_verts);
                    if reps.union(&fixed_verts).is_everything() {
                        next_should_find_reps = false;
                    }
                    reps
                } else {
                    fixed_verts.not()
                };

                'test_verts: for v in reps.iter() {
                    if coder.get_colour(config, &v).is_none() {
                        let mut can_be_cold = false;
                        let mut is_adj_condition_good = true;
                        if params.must_be_connected {
                            is_adj_condition_good = false;
                            'test_adj: for w in g.adj_list[v].iter() {
                                if coder.get_colour(config, w).is_some() {
                                    is_adj_condition_good = true;
                                    break 'test_adj;
                                }
                            }
                        }
                        if !is_adj_condition_good {
                            continue 'test_verts;
                        }
                        for c in 0..col_cap {
                            let mut can_use_c = true;
                            let is_this_a_dud =
                                params.can_b_play_duds && c == params.k - 1 && !alice_turn;
                            if !is_this_a_dud {
                                'test_c: for u in g.adj_list[v].iter() {
                                    if coder.get_colour(config, u) == Some(c) {
                                        can_use_c = false;
                                        break 'test_c;
                                    }
                                }
                            }
                            if can_use_c {
                                if !is_this_a_dud {
                                    // if the only valid play is a dud, then B wins.
                                    is_something_playable = true;
                                    can_be_cold = true;
                                }
                                let new_config = coder.play_move(config, v, c);
                                let max_col = max_colour_used.max(c);
                                let sub_alice_win = alice_wins_chromatic_game_rec(
                                    g,
                                    ann,
                                    params,
                                    max_col,
                                    coder,
                                    new_config,
                                    history,
                                    fixed_verts.add_vert_immutable(v),
                                    next_should_find_reps,
                                    num_cold + 1,
                                );
                                if sub_alice_win == alice_turn {
                                    // This is a winning strategy for this player.
                                    alice_win = sub_alice_win;
                                    if params.fast_mode {
                                        break 'test_verts;
                                    }
                                }
                            }
                        }
                        if !can_be_cold {
                            // This vertex cannot be coloured, so Bob wins.
                            alice_win = false;
                            break 'test_verts;
                        }
                    }
                }
                if !is_something_playable {
                    // Nothing can be played, so Bob wins.
                    alice_win = false
                }
                // Record for posterity.
                history.insert(wlog_index, alice_win);
                alice_win
            }
        }
    }
}

fn get_chromatic_game_history(
    g: &Graph,
    ann: &mut Annotations,
    params: &ChromaticParameters,
    history: &mut HashMap<Config, bool>,
    coder: &Coder,
) {
    let mut alice_wins = false;
    'test_verts: for v in ann.weak_representatives().iter() {
        let config = coder.play_move(coder.get_start_config(), v, 0);
        if alice_wins_chromatic_game_rec(
            g,
            ann,
            params,
            0,
            coder,
            config,
            history,
            VertexSet::of_vert(g.n, v),
            true,
            1,
        ) {
            alice_wins = true;
            history.insert(config, true);
            if params.fast_mode {
                break 'test_verts;
            }
        } else {
            history.insert(config, false);
        }
    }
    history.insert(coder.get_start_config(), alice_wins);
}

pub fn maker_wins_chromatic_game(
    g: &Graph,
    ann: &mut Annotations,
    k: usize,
    must_be_connected: bool,
    print_strategy: bool,
) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        true
    } else {
        let coder = Coder::new(g.n, k);
        let mut history = HashMap::new();
        let params = ChromaticParameters {
            k,
            fast_mode: true,
            can_b_play_duds: false,
            must_be_connected,
            alice_greedy: false,
        };
        get_chromatic_game_history(g, ann, &params, &mut history, &coder);
        if print_strategy {
            coder.print_history(&history)
        }
        *history.get(&coder.get_start_config()).unwrap()
    }
}

pub fn alice_greedy_wins_chromatic_game(g: &Graph, ann: &mut Annotations, k: usize) -> bool {
    let coder = Coder::new(g.n, k);
    let mut history = HashMap::new();
    let params = ChromaticParameters {
        k,
        fast_mode: true,
        can_b_play_duds: false,
        must_be_connected: false,
        alice_greedy: true,
    };
    get_chromatic_game_history(g, ann, &params, &mut history, &coder);
    *history.get(&coder.get_start_config()).unwrap()
}

pub fn print_chromatic_game_strategy(
    g: &Graph,
    ann: &mut Annotations,
    k: usize,
    must_be_connected: bool,
) {
    let _ = maker_wins_chromatic_game(g, ann, k, must_be_connected, true);
}

pub fn chromatic_game_strong_monotonicity(g: &Graph, ann: &mut Annotations) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    let coders: Vec<Coder> = (0..=greedy).map(|k| Coder::new(g.n, k)).collect();
    let mut histories: Vec<HashMap<Config, bool>> = (0..=greedy).map(|_| HashMap::new()).collect();
    for k in 2..=greedy {
        let params = ChromaticParameters {
            k,
            fast_mode: false,
            can_b_play_duds: false,
            must_be_connected: false,
            alice_greedy: false,
        };
        get_chromatic_game_history(g, ann, &params, &mut histories[k], &coders[k]);
    }

    let mut is_monot = true;

    'monot_test: for k in 2..greedy {
        for (lo_config, a_lo_win) in histories[k].iter() {
            if *a_lo_win {
                let hi_config = coders[k].increase_k(&coders[k + 1], *lo_config);
                if let Some(a_hi_win) = histories[k + 1].get(&hi_config) {
                    if !*a_hi_win {
                        is_monot = false;
                        print!(
                            "Gotcha. (k = {}) (win = {:?})  config: ",
                            k,
                            histories[k].get(lo_config)
                        );
                        coders[k].print(*lo_config);
                        print!(
                            "High:   (k = {}) (win = {:?}) config: ",
                            k + 1,
                            histories[k + 1].get(&hi_config)
                        );
                        coders[k + 1].print(hi_config);
                        println!("    ~~~~  {}-HISTORY  ~~~~", k);
                        coders[k].print_history(&histories[k]);
                        println!("    ~~~~  {}-HISTORY  ~~~~", k + 1);
                        coders[k + 1].print_history(&histories[k + 1]);
                        break 'monot_test;
                    }
                }
            }
        }
    }

    is_monot
}

/**
 * Tests whether there is ever a situation in the B-duds game where B's
 * only winning move is to play a dud.
 */
pub fn can_chormatic_game_dud_unique_win(g: &Graph, ann: &mut Annotations) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    for k in 2..greedy {
        let coder = Coder::new(g.n, k);
        let mut history = HashMap::new();
        let params = ChromaticParameters {
            k,
            fast_mode: false,
            can_b_play_duds: true,
            must_be_connected: false,
            alice_greedy: false,
        };
        get_chromatic_game_history(g, ann, &params, &mut history, &coder);
        for (config, a_win) in history.iter() {
            // need that the only winning move at some vertex is k-1.
            if !*a_win {
                for v in g.iter_verts() {
                    if coder.get_colour(*config, &v).is_none() {
                        let mut is_v_dud_win_only = false;
                        let mut is_some_other_move_playable = false;
                        let dud_config =
                            coder.get_wlog_index(coder.play_move(*config, v, k - 1), true);
                        if let Some(false) = history.get(&dud_config) {
                            is_v_dud_win_only = true;
                            'test_all_other_cols: for col in 0..(k - 1) {
                                let new_config =
                                    coder.get_wlog_index(coder.play_move(*config, v, col), true);
                                if let Some(a_win) = history.get(&new_config) {
                                    is_some_other_move_playable = true;
                                    if !*a_win {
                                        is_v_dud_win_only = false;
                                        break 'test_all_other_cols;
                                    }
                                }
                            }
                        }
                        if is_v_dud_win_only && is_some_other_move_playable {
                            coder.print_history(&history);
                            println!("Found a dud win only place! k = {}. Vertex {}", k, v);
                            coder.print(*config);
                            return true;
                        }
                    }
                }
            }
        }
    }
    false
}

pub fn alice_wins_chromatic_game_with_duds(g: &Graph, ann: &mut Annotations, k: usize) -> bool {
    let coder = Coder::new(g.n, k);
    let mut history = HashMap::new();
    let params = ChromaticParameters {
        k,
        fast_mode: true,
        can_b_play_duds: true,
        must_be_connected: false,
        alice_greedy: false,
    };
    get_chromatic_game_history(g, ann, &params, &mut history, &coder);
    *history.get(&coder.get_start_config()).unwrap()
}

#[derive(Copy, Clone)]
struct Subwins {
    a_lo: bool,
    b_lo: bool,
    a_hi: bool,
    b_hi: bool,
}

impl Subwins {
    const FALSE: Self = Self {
        a_lo: false,
        b_lo: false,
        a_hi: false,
        b_hi: false,
    };
}

impl fmt::Debug for Subwins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn digify(b: bool) -> usize {
            if b {
                1
            } else {
                0
            }
        }
        write!(
            f,
            "a: {}{}, b: {}{}",
            digify(self.a_lo),
            digify(self.a_hi),
            digify(self.b_lo),
            digify(self.b_hi)
        )
    }
}

pub fn is_some_subset_non_monotone(g: &Graph, ann: &mut Annotations, k: usize) -> bool {
    let lo_coder = Coder::new(g.n, k);
    let mut lo_history = HashMap::new();
    let mut params = ChromaticParameters {
        k,
        fast_mode: false,
        can_b_play_duds: false,
        must_be_connected: false,
        alice_greedy: false,
    };
    get_chromatic_game_history(g, ann, &params, &mut lo_history, &lo_coder);
    let hi_coder = Coder::new(g.n, k + 1);
    let mut hi_history = HashMap::new();
    params.k = k + 1;
    get_chromatic_game_history(g, ann, &params, &mut hi_history, &hi_coder);
    let num_subsets = 1 << g.n.to_usize();
    let mut subset_wins = vec![Subwins::FALSE; num_subsets];
    for (config, a_win) in lo_history.iter() {
        let vertex_set_code = lo_coder.vertex_set(*config).to_int() as usize;
        if *a_win {
            subset_wins[vertex_set_code].a_lo = true;
        } else {
            subset_wins[vertex_set_code].b_lo = true;
        }
    }
    for (config, a_win) in hi_history.iter() {
        let vertex_set_code = hi_coder.vertex_set(*config).to_int() as usize;
        if *a_win {
            subset_wins[vertex_set_code].a_hi = true;
        } else {
            subset_wins[vertex_set_code].b_hi = true;
        }
    }

    let mut is_all_monot = true;
    for vertex_set in 0..num_subsets {
        let set = VertexSet::of_int(vertex_set as u128, g.n);
        let subwins = subset_wins[vertex_set];
        //print!("{:?} ", subwins);
        //set.print();
        if subwins.a_lo && !subwins.a_hi {
            is_all_monot = false;
            print!("Failure by A at ");
            set.print();
        }
        if subwins.b_hi && !subwins.b_lo {
            is_all_monot = false;
            print!("Failure by B at ");
            set.print();
        }
    }
    !is_all_monot
}

// Alice's greedy strategy is to play vertices in decreasing order of degree.
pub fn alice_greedy_lower_bound(g: &Graph) -> usize {
    let mut degs = g.degree_sequence().to_owned();
    degs.sort();
    for (k, d) in degs.iter().rev().enumerate() {
        if d.to_usize() < 2 * k {
            return 2 * k;
        }
    }
    g.n.to_usize()
}

pub fn game_chromatic_number(g: &Graph, ann: &mut Annotations) -> u32 {
    let mut k = 1;
    loop {
        if maker_wins_chromatic_game(g, ann, k, false, false) {
            return k as u32;
        }
        k += 1;
    }
}

pub fn connected_game_chromatic_number(g: &Graph, ann: &mut Annotations) -> u32 {
    let mut k = 1;
    loop {
        if maker_wins_chromatic_game(g, ann, k, true, false) {
            return k as u32;
        }
        k += 1
    }
}

pub fn game_chromatic_colour_monotone(g: &Graph, ann: &mut Annotations) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    let mut alice_prev_winner = false;
    for k in 1..greedy {
        let alice_wins = maker_wins_chromatic_game(g, ann, k, false, false);
        print!("{}", if alice_wins { 'A' } else { 'B' });
        std::io::stdout().flush().unwrap();

        if alice_prev_winner && !alice_wins {
            println!("[win!]");
            return false;
        }
        alice_prev_winner = alice_wins;
        if k == greedy - 2 && !alice_wins {
            // As Alice wins at greedy, we need an Alice win here to have time to flip.
            println!("[drop]");
            return true;
        }
    }
    println!();
    true
}

pub fn print_game_chromatic_table(g: &Graph, ann: &mut Annotations) {
    let delta = g.max_degree().to_usize();
    let mut checkpoint_time;
    println!("Greedy: {}", alice_greedy_lower_bound(g));
    for k in 1..=(delta + 1) {
        checkpoint_time = SystemTime::now();
        if maker_wins_chromatic_game(g, ann, k, false, false) {
            print!("{}: Alice", k);
        } else {
            print!("{}: Bob", k);
        }
        println!(", t: {}", checkpoint_time.elapsed().unwrap().as_millis());
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use utilities::*;

    #[test]
    fn test_chi_1() {
        assert_eq!(chromatic_number(&Graph::test_graph(1)), 4);
    }

    #[test]
    fn test_chi_2() {
        assert_eq!(chromatic_number(&Graph::test_graph(2)), 3);
    }

    #[test]
    fn test_chi_3() {
        assert_eq!(chromatic_number(&Graph::test_graph(3)), 2);
    }

    #[test]
    fn test_chi_e10() {
        assert_eq!(chromatic_number(&Graph::new_empty(Order::of_usize(10))), 1);
    }

    fn test_chi_g(g: &Graph) -> u32 {
        let mut ann = Annotations::new(g);
        game_chromatic_number(g, &mut ann)
    }

    #[test]
    fn test_game_1() {
        assert_eq!(test_chi_g(&Graph::test_graph(1)), 4);
    }

    #[test]
    fn test_game_2() {
        assert_eq!(test_chi_g(&Graph::test_graph(2)), 3);
    }

    #[test]
    fn test_game_3() {
        assert_eq!(test_chi_g(&Graph::test_graph(3)), 4);
    }

    #[test]
    fn test_game_e10() {
        assert_eq!(test_chi_g(&Graph::new_empty(Order::of_usize(10))), 1);
    }

    #[test]
    fn test_game_k10() {
        assert_eq!(test_chi_g(&Graph::new_complete(Order::of_usize(10))), 10);
    }

    #[test]
    fn test_game_p4() {
        assert_eq!(test_chi_g(&Graph::new_path(Order::of_usize(4))), 3);
    }
}
