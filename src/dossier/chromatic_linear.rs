
use crate::graph::*;

use utilities::vertex_tools::*;
use utilities::chromatic_tools::*;

fn maker_wins_linear_chromatic_game_rec(g: &Graph, k: usize, max_colour_used: usize, config: Config, 
        coder: &Coder, next_vert: Vertex, is_maker_turn: bool) -> bool {
    if next_vert.is_n(g.n) {
        // everything is coloured.
        true
    } else {
        let mut maker_win = !is_maker_turn;
        let col_cap = (max_colour_used + 2).min(k);
        let mut can_be_coloured = false;
        'test_cols: for c in 0..col_cap {
            let mut can_use_c = true;
            'test_c: for u in g.adj_list[next_vert].iter() {
                if coder.get_colour(config, u) == Some(c) {
                    can_use_c = false;
                    break 'test_c;
                }
            }
            if can_use_c {
                can_be_coloured = true;
                let new_config = coder.play_move(config, next_vert, c);
                let new_max_colour = max_colour_used.max(c);
                let sub_maker_win = maker_wins_linear_chromatic_game_rec(g, k, new_max_colour, 
                    new_config, coder, next_vert.incr(), !is_maker_turn);
                if sub_maker_win == is_maker_turn {
                    maker_win = sub_maker_win;
                    break 'test_cols;
                }
            }
        }
        if !can_be_coloured {
            maker_win = false;
        }
        maker_win
    }
}

pub fn does_maker_win_linear_chromatic_game(g: &Graph, k: usize) -> bool {
    let coder = Coder::new(g.n, k);
    let v = Vertex::ZERO;
    let config = coder.play_move(coder.get_start_config(), v, 0);
    maker_wins_linear_chromatic_game_rec(g, k, 0, config, &coder, v.incr(), false)
}

pub fn linear_game_chromatic_number(g: &Graph) -> u32 {
    let mut k = 0;
    while !does_maker_win_linear_chromatic_game(g, k) {
        k += 1;
    }
    k as u32
}