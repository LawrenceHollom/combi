use crate::graph::*;

use utilities::chromatic_tools::*;

fn maker_wins_grundy_game_rec(g: &Graph, k: usize, config: Config, coder: &Coder,
        num_cold: usize) -> bool {
    if g.n.at_most(num_cold) {
        true
    } else {
        let is_maker_turn = num_cold % 2 == 0;
        let mut maker_win = !is_maker_turn;
        let mut can_something_be_cold = false;
        'test_verts: for u in g.iter_verts() {
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
                    let new_config = coder.play_move(config, u, min_colour);
                    let sub_maker_win = maker_wins_grundy_game_rec(g, k, new_config, coder, num_cold + 1);
                    if sub_maker_win == is_maker_turn {
                        maker_win = sub_maker_win;
                        break 'test_verts;
                    }
                }
            }
        }
        if !can_something_be_cold {
            maker_win = false;
        }
        maker_win
    }
}

pub fn does_maker_win_grundy_game(g: &Graph, k: usize) -> bool {
    let coder = Coder::new(g.n, k);
    maker_wins_grundy_game_rec(g, k, coder.get_start_config(), &coder, 0)
}

pub fn game_grundy_number(g: &Graph) -> u32 { 
    let mut k = 1;
    while !does_maker_win_grundy_game(g, k) {
        k += 1;
    }
    k as u32
}