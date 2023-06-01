use std::collections::HashMap;
use std::io::Write;
use std::time::SystemTime;

use crate::annotations::Annotations;
use crate::graph::*;

use utilities::*;
use utilities::vertex_tools::*;

fn can_be_coloured_rec(g: &Graph, num_colours: usize, max_colour_used: usize, colour: &mut VertexVec<Option<usize>>, next_vert: Vertex) -> bool {
    if next_vert.is_n(g.n) {
        true
    } else {
        let mut can_colour = false;
        'test_all_colourings: for col in 0..usize::min(max_colour_used + 2, num_colours) {
            let mut can_use_this_colour = true;
            'test_this_colour: for (i, edge) in g.adj[next_vert].iter_enum() {
                if *edge && colour[i] == Some(col) { // can't colour that
                    can_use_this_colour = false;
                    break 'test_this_colour;
                }
            }
            if can_use_this_colour {
                colour[next_vert] = Some(col);
                if can_be_coloured_rec(g, num_colours, usize::max(max_colour_used, col), colour, next_vert.incr()) {
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

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Config(u128);

struct Coder {
    n: Order,
    k: usize,
    pows: VertexVec<u128>,
}

impl Coder {
    pub fn new(n: Order, k: usize) -> Coder {
        let mut pows = VertexVec::new(n, &0);
        for (i, v) in n.iter_verts().enumerate() {
            pows[v] = ((k + 1) as u128).pow(i as u32);
        }
        Coder { n, k, pows }
    }
    
    fn get_colour(&self, config: Config, u: &Vertex) -> Option<usize> {
        let col = ((config.0 / self.pows[*u]) % ((self.k + 1) as u128)) as usize;
        if col == self.k {
            None
        } else {
            Some(col)
        }
    }

    fn play_move(&self, config: Config, v: Vertex, col: usize) -> Config {
        Config(config.0 - ((self.k - col) as u128 * self.pows[v]))
    }

    fn increase_k(&self, other: &Coder, config: Config) -> Config {
        let mut out = other.get_start_config();
        for v in self.n.iter_verts() {
            if let Some(col) = self.get_colour(config, &v) {
                out = other.play_move(out, v, col);
            }
        }
        out
    }

    fn get_start_config(&self) -> Config {
        let mut config = 0;
        for pow in self.pows.iter() {
            config += *pow * self.k as u128;
        }
        Config(config)
    }

    // Returns an index with every wlog assumption made that we can make.
    fn get_wlog_index(&self, config: Config, fix_last_col: bool) -> Config {
        let mut map = vec![None; self.k + 1];
        map[self.k] = Some(self.k);
        if fix_last_col {
            map[self.k - 1] = Some(self.k - 1);
        }
        let mut wlog_index = 0;
        let mut next_col = 0;

        for pow in self.pows.iter() {
            let digit = ((config.0 / pow) % (self.k + 1) as u128) as usize;
            if map[digit].is_none() {
                map[digit] = Some(next_col);
                next_col += 1;
            }
            wlog_index += map[digit].unwrap() as u128 * pow;
        }
        Config(wlog_index)
    }

    fn print(&self, config: Config) {
        for v in self.n.iter_verts() {
            print!("{} ", self.get_colour(config, &v).map_or("-".to_owned(), |col| col.to_string()));
        }
        println!();
    }
}

/**
 * Recursively test if Alice can win the chromatic game. Parameters:
 * 
 * k: number of colours
 * max_colour_used: highest numbered colour used so far
 * config: the state of the board (i.e. how the graph is coloured so far)
 * history: a record of what we already know about winning states
 * fixed_verts: used to get vertex class representatives in the early stages.
 * num_cols: how many vertices have already been coloured
 * fast_mode: stop the simulation as soon as we know that a player has won.
 *      If this is false, then we create a complete simulation of the game, even following
 *      winning and losing positions through to the end.
 * can_b_play_duds: if true, then colour k-1 is the 'dud' colour, can only be played
 *      by B, and does not effect the surrounding vertices.
 */
fn alice_wins_chromatic_game_rec(g: &Graph, ann: &mut Annotations, k: usize, max_colour_used: usize, coder: &Coder,
                config: Config, history: &mut HashMap<Config, bool>, fixed_verts: VertexSet, should_find_reps: bool, 
                num_cold: usize, fast_mode: bool, can_b_play_duds: bool) -> bool {
    if g.n.at_least(30) && num_cold <= 15 {
        println!("Step! {}", num_cold);
    }
    if g.n.at_most(num_cold) {
        // G is coloured, so Alice has won
        true
    } else {
        let wlog_index = coder.get_wlog_index(config, can_b_play_duds);
        match history.get(&wlog_index) {
            Some(alice_win) => *alice_win,
            None => {
                let alice_turn = num_cold % 2 == 0;
                let mut alice_win = !alice_turn;
                let mut is_something_playable = false;
                let col_cap = if can_b_play_duds && (alice_turn || g.n.at_most(num_cold + 1)) {
                        (max_colour_used + 2).min(k - 1)
                    } else {
                        (max_colour_used + 2).min(k)
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
                        for c in 0..col_cap {
                            let mut can_use_c = true;
                            let is_this_a_dud = can_b_play_duds && c == k - 1 && !alice_turn;
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
                                let sub_alice_win = alice_wins_chromatic_game_rec(g, ann, k, max_col, 
                                    coder, new_config, history, fixed_verts.add_vert_immutable(v), next_should_find_reps,
                                    num_cold + 1, fast_mode, can_b_play_duds);
                                if sub_alice_win == alice_turn {
                                    // This is a winning strategy for this player.
                                    alice_win = sub_alice_win;
                                    if fast_mode {
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

fn print_history(history: &HashMap<Config, bool>, coder: &Coder) {
    let mut configs: Vec<Config> = history.keys().cloned().collect();
    configs.sort();
    for config in configs {
        let v = history.get(&config).unwrap();
        print!("{}: ", if *v { "A" } else { "B" });
        coder.print(config);
    }
}

fn get_chromatic_game_history(g: &Graph, ann: &mut Annotations, k: usize, can_b_play_duds: bool,
        fast_mode: bool, history: &mut HashMap<Config, bool>, coder: &Coder) {
    let mut alice_wins = false;
    'test_verts: for v in ann.weak_representatives().iter() {
        let config = coder.play_move(coder.get_start_config(), v, 0);
        if alice_wins_chromatic_game_rec(g, ann, k, 0, &coder, config, history, 
                VertexSet::of_vert(g.n, v), true, 1, 
                fast_mode, can_b_play_duds) {
            alice_wins = true;
            history.insert(config, true);
            if fast_mode {
                break 'test_verts;
            }
        } else {
            history.insert(config, false);
        }
    }
    history.insert(coder.get_start_config(), alice_wins);
}

pub fn alice_wins_chromatic_game(g: &Graph, ann: &mut Annotations, k: usize, print_strategy: bool) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        return true;
    }
    let coder = Coder::new(g.n, k);
    let mut history = HashMap::new();
    get_chromatic_game_history(g, ann, k, false, true, &mut history, &coder);
    if print_strategy {
        print_history(&history, &coder)
    }
    *history.get(&coder.get_start_config()).unwrap()
}

pub fn print_chromatic_game_strategy(g: &Graph, ann: &mut Annotations, k: usize) {
    let _ = alice_wins_chromatic_game(g, ann, k, true);
}

pub fn chromatic_game_strong_monotonicity(g: &Graph, ann: &mut Annotations) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    let coders: Vec<Coder> = (0..=greedy).map(|k| Coder::new(g.n, k)).collect();
    let mut histories: Vec<HashMap<Config, bool>> = (0..=greedy).map(|_| HashMap::new()).collect(); 
    for k in 2..=greedy {
        /*for v in ann.weak_representatives().iter() {
            let config = coders[k].play_move(coders[k].get_start_config(), v, 0);
            let win = alice_wins_chromatic_game_rec(g, ann, k, 0, &coders[k], config,
                &mut histories[k], VertexSet::of_vert(g.n, v), true, 1, 
                false, false);
            histories[k].insert(config, win);
        }*/
        get_chromatic_game_history(g, ann, k, false, false, 
            &mut histories[k], &coders[k]);
    }

    let mut is_monot = true;

    'monot_test: for k in 2..greedy {
        for (lo_config, a_lo_win) in histories[k].iter() {
            if *a_lo_win {
                let hi_config = coders[k].increase_k(&coders[k+1], *lo_config);
                if let Some(a_hi_win) = histories[k+1].get(&hi_config) {
                    if !*a_hi_win {
                        is_monot = false;
                        print!("Gotcha. (k = {}) (win = {:?})  config: ", k, histories[k].get(lo_config));
                        coders[k].print(*lo_config);
                        print!("High:   (k = {}) (win = {:?}) config: ", k+1, histories[k+1].get(&hi_config));
                        coders[k+1].print(hi_config);
                        println!("    ~~~~  {}-HISTORY  ~~~~", k);
                        print_history(&histories[k], &coders[k]);
                        println!("    ~~~~  {}-HISTORY  ~~~~", k+1);
                        print_history(&histories[k+1], &coders[k+1]);
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
        get_chromatic_game_history(g, ann, k, true, false, &mut history, &coder);
        for (config, a_win) in history.iter() {
            // need that the only winning move at some vertex is k-1.
            if !*a_win {
                for v in g.iter_verts() {
                    if coder.get_colour(*config, &v).is_none() {
                        let mut is_v_dud_win_only = false;
                        let mut is_some_other_move_playable = false;
                        let dud_config = coder.get_wlog_index(coder.play_move(*config, v, k - 1), true);
                        match history.get(&dud_config) {
                            Some(false) => {
                                is_v_dud_win_only = true;
                                'test_all_other_cols: for col in 0..(k-1) {
                                    let new_config = coder.get_wlog_index(coder.play_move(*config, v, col), true);
                                    if let Some(a_win) = history.get(&new_config) {
                                        is_some_other_move_playable = true;
                                        if !*a_win {
                                            is_v_dud_win_only = false;
                                            break 'test_all_other_cols;
                                        }
                                    }
                                }
                            }
                            Some(true) | None => (),
                        }
                        if is_v_dud_win_only && is_some_other_move_playable {
                            print_history(&history, &coder);
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
    get_chromatic_game_history(g, ann, k, true, true, &mut history, &coder);
    *history.get(&coder.get_start_config()).unwrap()
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
        if alice_wins_chromatic_game(g, ann, k, false) {
            return k as u32;
        }
        k += 1;
    }
}

pub fn game_chromatic_colour_monotone(g: &Graph, ann: &mut Annotations) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    let mut alice_prev_winner = false;
    for k in 1..greedy {
        let alice_wins = alice_wins_chromatic_game(g, ann, k, false);
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
        if alice_wins_chromatic_game(g, ann, k as usize, false) {
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