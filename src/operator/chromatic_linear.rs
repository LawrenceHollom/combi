
use crate::graph::*;

use utilities::*;
use utilities::vertex_tools::*;
use utilities::chromatic_tools::*;

fn maker_wins_linear_chromatic_game_rec(g: &Graph, k: usize, max_colour_used: usize, config: Config, next_vert: usize) -> bool {
    true
}

pub fn does_maker_win_linear_chromatic_game(g: &Graph, k: usize) -> bool {
    let coder = Coder::new(g.n, k);
    let v = Vertex::ZERO;
    let mut config = coder.play_move(coder.get_start_config(), v, 0);
    maker_wins_linear_chromatic_game_rec(g, k, 0, config, v.incr())
}

pub fn linear_game_chromatic_number(g: &Graph) -> u32 {
    let mut k = 0;
    while !does_maker_win_linear_chromatic_game(g, k) {
        k += 1;
    }
    k
}