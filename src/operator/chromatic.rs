use std::io::Write;
use std::time::SystemTime;

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

#[derive(Clone, Copy)]
struct Config(usize);

struct Coder {
    _n: Order,
    k: usize,
    num_configs: usize,
    pows: VertexVec<usize>,
}

impl Coder {
    pub fn new(n: Order, k: usize) -> Coder {
        let mut pows = VertexVec::new(n, &0);
        for (i, v) in n.iter_verts().enumerate() {
            pows[v] = (k + 1).pow(i as u32);
        }
        let num_configs = (k + 1).pow(n.to_usize() as u32);
        Coder { _n: n, k, num_configs, pows }
    }
    
    fn get_colour(&self, config: Config, u: &Vertex) -> Option<usize> {
        let col = (config.0 / self.pows[*u]) % (self.k + 1);
        if col == self.k {
            None
        } else {
            Some(col)
        }
    }

    fn _is_alice_turn(&self, config: Config) -> bool {
        // Need to count how many verts have been played
        let mut num_played = 0;
        for pow in self.pows.iter() {
            if (config.0 / pow) % (self.k + 1) != self.k {
                num_played += 1;
            }
        }
        num_played % 2 == 0
    }

    fn play_move(&self, config: Config, v: Vertex, col: usize) -> Config {
        Config(config.0 - ((self.k - col) * self.pows[v]))
    }

    fn _incr_colour(&self, config: Config, pos: Vertex) -> (Config, Vertex) {
        let config_out = Config(config.0 + self.pows[pos]);
        let mut pos_out = pos;
        while !pos_out.is_n(self._n) && (config_out.0 / self.pows[pos_out]) % (self.k + 1) == 0 {
            pos_out.incr_inplace();
        }
        (config_out, pos_out)
    }

    fn get_start_config(&self) -> Config {
        let mut config = 0;
        for pow in self.pows.iter() {
            config += *pow * self.k;
        }
        Config(config)
    }

    // Returns an index with every wlog assumption made that we can make.
    fn get_wlog_index(&self, config: Config) -> Config {
        let mut map = vec![None; self.k + 1];
        map[self.k] = Some(self.k);
        let mut wlog_index = 0;
        let mut next_col = 0;

        for pow in self.pows.iter() {
            let digit = (config.0 / pow) % (self.k + 1);
            if map[digit].is_none() {
                map[digit] = Some(next_col);
                next_col += 1;
            }
            wlog_index += map[digit].unwrap() * pow;
        }
        Config(wlog_index)
    }

    fn _print(&self, config: Config) {
        for v in self._n.iter_verts() {
            print!("{} ", self.get_colour(config, &v).map_or("-".to_owned(), |col| col.to_string()));
        }
        println!();
    }
}

fn alice_wins_chromatic_game_fast_rec(g: &Graph, k: usize, max_colour_used: usize, coder: &Coder,
                config: Config, history: &mut Vec<Option<bool>>, num_cold: usize) -> bool {
    if g.n.at_most(num_cold) {
        // G is coloured, so Alice has won
        true
    } else {
        let wlog_index = coder.get_wlog_index(config);
        match history[wlog_index.0] {
            Some(alice_win) => alice_win,
            None => {
                let mut alice_win = num_cold % 2 == 1;
                let mut is_something_playable = false;
                let col_cap = (max_colour_used + 2).min(k);

                'test_verts: for v in g.iter_verts() {
                    if coder.get_colour(config, &v).is_none() {
                        for c in 0..col_cap {
                            let mut can_use_c = true;
                            'test_c: for u in g.adj_list[v].iter() {
                                if coder.get_colour(config, u) == Some(c) {
                                    can_use_c = false;
                                    break 'test_c;
                                }
                            }
                            if can_use_c {
                                is_something_playable = true;
                                let new_config = coder.play_move(config, v, c);
                                let max_col = max_colour_used.max(c);
                                let sub_alice_win = alice_wins_chromatic_game_fast_rec(g, k, max_col, coder, new_config, history, num_cold + 1);
                                if sub_alice_win != alice_win {
                                    // This is a winning strategy for this player.
                                    alice_win = sub_alice_win;
                                    break 'test_verts;
                                }
                            }
                        }
                    }
                }
                if !is_something_playable {
                    // Nothing can be played, so Bob wins.
                    alice_win = false
                }
                // Record for posterity.
                history[wlog_index.0] = Some(alice_win);
                alice_win
            }
        }
    }
}

fn alice_wins_chromatic_game_fast(g: &Graph, k: usize) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        return true;
    }
    let coder = Coder::new(g.n, k);
    // This could be smaller as we'll assume configs start with None or Some(0).
    let mut history = vec![None; coder.num_configs];
    for v in g.iter_verts() {
        let config = coder.play_move(coder.get_start_config(), v, 0);
        if alice_wins_chromatic_game_fast_rec(g, k, 0, &coder, config, &mut history, 1) {
            return true;
        }
    }
    false
}

fn _alice_wins_chromatic_game_array(g: &Graph, k: usize) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        return true;
    }
    let coder = Coder::new(g.n, k);
    // Conceivably want this to be a hashmap as so many of these don't work.
    let mut alice_wins = vec![false; coder.num_configs];
    let mut config = Config(0);
    let mut pos = g.n.to_max_vertex();
    
    let mut back_adj = VertexVec::new(g.n, &vec![]);
    for v in g.iter_verts() {
        for u in g.adj_list[v].iter() {
            if *u > v {
                back_adj[v].push(*u);
            }
        }
    }

    'loop_configs: loop {
        if config.0 >= coder.num_configs {
            break 'loop_configs;
        }
        
        // Test if the config is valid at pos.
        let mut is_valid = true;
        let col = coder.get_colour(config, &pos);
        if col.is_some() {
            'test_back_adj: for u in back_adj[pos].iter() {
                if coder.get_colour(config, u) == col {
                    is_valid = false;
                    break 'test_back_adj;
                }
            }
        }
        if is_valid && pos.is_zero() {
            // We've actually got something that we can think about.
            let is_alice_turn = coder._is_alice_turn(config);
            let mut found_winner = false;
            let mut found_move = false;
            let mut found_unplayed = false;

            'test_plays: for v in g.iter_verts() {
                if coder.get_colour(config, &v).is_none() {
                    found_unplayed = true;
                    let mut colours_hit = 0;
                    for u in g.adj_list[v].iter() {
                        match coder.get_colour(config, u) {
                            Some(col) => colours_hit |= 1 << col,
                            None => (),
                        }
                    }

                    for col in 0..k {
                        if (colours_hit >> col) % 2 == 0 {
                            // This colour hasn't been hit, so is valid here.
                            found_move = true;

                            let new_config = coder.play_move(config, v, col);
                            if alice_wins[new_config.0] == is_alice_turn {
                                found_winner = true;
                                break 'test_plays;
                            }
                        }
                    }
                }
            }

            if !found_unplayed || (found_move && found_winner == is_alice_turn) {
                // This is winning for Alice
                alice_wins[config.0] = true;
            }
            (config, pos) = coder._incr_colour(config, pos);
        } else if is_valid {
            pos.decr_inplace();
        } else {
            (config, pos) = coder._incr_colour(config, pos);
        }
    }
    alice_wins[coder.get_start_config().0]
}

fn _alice_wins_chromatic_game_rec(g: &Graph, k: usize, max_colour_used: usize, 
        colour: &mut VertexVec<Option<usize>>, num_cold: usize) -> bool {
    if g.n.at_most(num_cold) {
        // G is coloured, so Alice has won
        true
    } else {
        let mut alice_win = num_cold % 2 == 1;
        let mut is_something_playable = false;
        let col_cap = (max_colour_used + 2).min(k);

        'test_verts: for v in g.iter_verts() {
            if colour[v].is_none() {
                for c in 0..col_cap {
                    let mut can_use_c = true;
                    'test_c: for u in g.adj_list[v].iter() {
                        if colour[*u] == Some(c) {
                            can_use_c = false;
                            break 'test_c;
                        }
                    }
                    if can_use_c {
                        is_something_playable = true;
                        colour[v] = Some(c);
                        let max_col = max_colour_used.max(c);
                        let sub_alice_win = _alice_wins_chromatic_game_rec(g, k, max_col, colour, num_cold + 1);
                        if sub_alice_win != alice_win {
                            // This is a winning strategy for this player.
                            alice_win = sub_alice_win;
                            colour[v] = None;
                            break 'test_verts;
                        }
                        colour[v] = None;
                    }
                }
            }
        }
        if is_something_playable {
            alice_win
        } else {
            // Nothing can be played, so Bob wins.
            false
        }
    }
}

// if (k+1)th smallest degree is <k then Alice can certainly win for k colours.
pub fn alice_greedy_lower_bound(g: &Graph) -> usize {
    let mut degs = g.degree_sequence().to_owned();
    degs.sort();
    for (k, d) in degs.iter().rev().enumerate() {
        if d.to_usize() < k {
            return k;
        }
    }
    return g.n.to_usize();
}

pub fn _alice_wins_chromatic_game_slow(g: &Graph, k: usize) -> bool {
    if k >= alice_greedy_lower_bound(g) {
        return true;
    }
    let mut colour = VertexVec::new(g.n, &None);
    for v in g.iter_verts() {
        colour[v] = Some(0);
        if _alice_wins_chromatic_game_rec(g, k, 0, &mut colour, 1) {
            return true;
        }
        colour[v] = None;
    }
    false
}

pub fn alice_wins_chromatic_game(g: &Graph, k: usize) -> bool {
    alice_wins_chromatic_game_fast(g, k)
}

pub fn game_chromatic_number(g: &Graph) -> u32 {
    let mut k = 1;
    loop {
        if alice_wins_chromatic_game(g, k) {
            return k as u32;
        }
        k += 1;
    }
}

pub fn game_chromatic_colour_monotone(g: &Graph) -> bool {
    let greedy = alice_greedy_lower_bound(g);
    let mut alice_prev_winner = false;
    for k in 1..greedy {
        let alice_wins = alice_wins_chromatic_game(g, k);
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

pub fn print_game_chromatic_table(g: &Graph) {
    let delta = g.max_degree();
    let mut checkpoint_time;
    println!("Greedy: {}", alice_greedy_lower_bound(g));
    for k in 1..=(delta + 1) {
        checkpoint_time = SystemTime::now();
        /*print!("Slow:  ");
        if alice_wins_chromatic_game_slow(g, k as usize) {
            print!("{}: Alice", k);
        } else {
            print!("{}: Bob", k);
        }
        println!(", t: {}", checkpoint_time.elapsed().unwrap().as_millis());
        checkpoint_time = SystemTime::now();
        print!("Array: ");
        if alice_wins_chromatic_game_array(g, k as usize) {
            print!("{}: Alice", k);
        } else {
            print!("{}: Bob", k);
        }
        println!(", t: {}", checkpoint_time.elapsed().unwrap().as_millis());
        checkpoint_time = SystemTime::now();
        print!("Fast:  ");*/
        if alice_wins_chromatic_game_fast(g, k as usize) {
            print!("{}: Alice", k);
        } else {
            print!("{}: Bob", k);
        }
        println!(", t: {}", checkpoint_time.elapsed().unwrap().as_millis());
    }
}

#[cfg(test)]
mod tests {
    use crate::graph::*;
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
}