use std::collections::HashMap;

use crate::graph::*;
use crate::annotations::*;

use utilities::vertex_tools::*;
use utilities::chromatic_tools::*;

use super::chromatic::*;

fn maker_wins_grundy_game_rec_fast(g: &Graph, ann: &mut Annotations, k: usize, config: Config, coder: &Coder,
        num_cold: usize, history: &mut HashMap<Config, bool>, fixed_verts: VertexSet, should_find_reps: bool) -> bool {
    if g.n.at_most(num_cold) {
        true
    } else {
        let wlog_index = coder.get_wlog_index(config, false);
        match history.get(&wlog_index) {
            Some(maker_win) => *maker_win,
            None => {
                let is_maker_turn = num_cold % 2 == 0;
                let mut maker_win = !is_maker_turn;
                let mut can_something_be_cold = false;
                let mut is_something_unplayable = false;
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
                'test_verts: for u in reps.iter() {
                    if coder.get_colour(config, &u).is_none() {
                        let mut colour_set = 0_u128;
                        for v in g.adj_list[u].iter() {
                            if let Some(c) = coder.get_colour(config, v) {
                                colour_set |= 1 << c as u128;
                            }
                        }
                        let mut min_colour = 0;
                        while (colour_set >> min_colour) % 2 == 1 {
                            min_colour += 1;
                        }
                        if min_colour < k {
                            can_something_be_cold = true;
                            let new_fixed_verts = fixed_verts.add_vert_immutable(u);
                            let new_config = coder.play_move(config, u, min_colour);
                            let sub_maker_win = maker_wins_grundy_game_rec_fast(g, ann, k, new_config, 
                                coder, num_cold + 1, history, new_fixed_verts, next_should_find_reps);
                            if sub_maker_win == is_maker_turn {
                                maker_win = sub_maker_win;
                                break 'test_verts;
                            }
                        } else {
                            is_something_unplayable = true;
                            break 'test_verts;
                        }
                    }
                }
                if !can_something_be_cold || is_something_unplayable {
                    maker_win = false;
                }
                history.insert(wlog_index, maker_win);
                maker_win
            }
        }
    }
}

pub fn does_maker_win_grundy_game(g: &Graph, ann: &mut Annotations, k: usize) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        true
    } else {
        let coder = Coder::new(g.n, k);
        let mut history = HashMap::new();
        maker_wins_grundy_game_rec_fast(g, ann, k, coder.get_start_config(), &coder, 0, 
            &mut history, VertexSet::new(g.n), true)
    }
}

pub fn game_grundy_number(g: &Graph, ann: &mut Annotations) -> u32 { 
    let mut k = 1;
    while !does_maker_win_grundy_game(g, ann, k) {
        k += 1;
    }
    k as u32
}