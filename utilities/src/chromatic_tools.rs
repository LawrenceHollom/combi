use std::collections::HashMap;

use crate::vertex_tools::*;
use crate::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Config(u128);

pub struct Coder {
    n: Order,
    k: usize,
    pows: VertexVec<u128>,
}

impl Coder {
    pub fn new(n: Order, k: usize) -> Coder {
        if (n.to_usize() as f64) * (k as f64 + 1.0).log2() >= 127.0 {
            panic!("Parameters too large for chromatic_tools! (n, k) = ({}, {})", n, k);
        }
        let mut pows = VertexVec::new(n, &0);
        for (i, v) in n.iter_verts().enumerate() {
            pows[v] = ((k + 1) as u128).pow(i as u32);
        }
        Coder { n, k, pows }
    }
    
    pub fn get_colour(&self, config: Config, u: &Vertex) -> Option<usize> {
        let col = ((config.0 / self.pows[*u]) % ((self.k + 1) as u128)) as usize;
        if col == self.k {
            None
        } else {
            Some(col)
        }
    }

    pub fn play_move(&self, config: Config, v: Vertex, col: usize) -> Config {
        Config(config.0 - ((self.k - col) as u128 * self.pows[v]))
    }

    pub fn increase_k(&self, other: &Coder, config: Config) -> Config {
        let mut out = other.get_start_config();
        for v in self.n.iter_verts() {
            if let Some(col) = self.get_colour(config, &v) {
                out = other.play_move(out, v, col);
            }
        }
        out
    }

    pub fn vertex_set(&self, config: Config) -> VertexSet {
        let mut out = VertexSet::new(self.n);
        for v in self.n.iter_verts() {
            match self.get_colour(config, &v) {
                Some(_) => out.add_vert(v),
                None => (),
            }
        }
        out
    }

    pub fn get_start_config(&self) -> Config {
        let mut config = 0;
        for pow in self.pows.iter() {
            config += *pow * self.k as u128;
        }
        Config(config)
    }

    pub fn config_of_set_monochrome(&self, set: VertexSet, col: usize) -> Config {
        let mut config = self.get_start_config();
        for v in set.iter() {
            config = self.play_move(config, v, col);
        }
        config
    }

    // Returns an index with every wlog assumption made that we can make.
    pub fn get_wlog_index(&self, config: Config, fix_last_col: bool) -> Config {
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

    pub fn print(&self, config: Config) {
        for v in self.n.iter_verts() {
            print!("{} ", self.get_colour(config, &v).map_or("-".to_owned(), |col| col.to_string()));
        }
        println!();
    }

    fn get_name_from_bool(&self, player: bool) -> &str {
        if player { "Maker" } else { "Breaker" }
    }

    fn get_current_name(&self, move_num: usize) -> &str {
        self.get_name_from_bool(move_num % 2 == 0)   
    }

    fn print_indent(&self, move_num: usize) {
        let indent = String::from_utf8(vec![b'.'; move_num]).unwrap();
        print!("{}", indent);
    }

    fn print_history_print_one_turn(&self, history: &HashMap<Config, bool>, is_maker_pov: bool,
            config: Config, move_num: usize, v: Vertex, col: usize, max_played_colour: usize) {
        let player = self.get_current_name(move_num);
        let new_config = self.play_move(config, v, col);
        let wlog_config = self.get_wlog_index(new_config, false);
        let winner_name = self.get_name_from_bool(*history.get(&wlog_config).unwrap());
        print!("{} plays colour {} at {}. {} wins. Config: ", player, col, v, winner_name);
        self.print(new_config);
        self.print_history_rec(history, is_maker_pov, new_config, move_num + 1, max_played_colour);
    }

    fn print_history_rec(&self, history: &HashMap<Config, bool>, is_maker_pov: bool, 
            config: Config, move_num: usize, max_played_colour: usize) {
        let is_maker_turn = move_num % 2 == 0;
        let is_pov_turn = is_maker_pov == is_maker_turn;
        let colour_cap = if move_num == 0 { 1 } else { (max_played_colour + 2).min(self.k) };
        let mut considered_moves: Vec<(Vertex, usize)> = vec![];
        // If it's pov's turn then we only need to print one winning move.
        for v in self.n.iter_verts() {
            if self.get_colour(config, &v).is_none() {
                for col in 0..colour_cap {
                    let new_config = self.play_move(config, v, col);
                    let new_config_wlog = self.get_wlog_index(new_config, false);
                    if let Some(maker_win) = history.get(&new_config_wlog) {
                        if *maker_win == is_maker_turn && is_pov_turn {
                            // This can be played as it is winning.
                            considered_moves.push((v, col));
                        } else if !is_pov_turn {
                            // We need to consider everything.
                            self.print_indent(move_num);
                            self.print_history_print_one_turn(history, is_maker_pov,
                                config, move_num, v, col, max_played_colour.max(col));
                        }
                    }
                }
            }
        }
        if is_pov_turn {
            if considered_moves.len() > 0 {
                self.print_indent(move_num);
                print!("{} available moves. ", considered_moves.len());
                let picked_move = considered_moves[0];
                let max_played_colour = max_played_colour.max(picked_move.1);
                self.print_history_print_one_turn(history, is_maker_pov, config, 
                    move_num, picked_move.0, picked_move.1, max_played_colour);
            } else {
                self.print_indent(move_num);
                println!("No moves saved for {}", self.get_current_name(move_num));
            }
        }
    }

    pub fn print_history(&self, history: &HashMap<Config, bool>) {
        let start_config = self.get_start_config();
        let maker_wins = *history.get(&start_config).unwrap();
        println!("Printing history! {} wins.", self.get_name_from_bool(maker_wins));
        self.print_history_rec(history, maker_wins, start_config, 0, 0)
    }
}