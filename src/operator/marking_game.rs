use std::collections::HashMap;

use crate::graph::*;

use utilities::vertex_tools::*;

fn marking_game_number_rec(g: &Graph, marked: VertexSet, num_marked: usize, must_be_connected: bool,
        history: &mut HashMap<VertexSet, usize>) -> usize {
    if g.n.at_most(num_marked) {
        0
    } else {
        match history.get(&marked) {
            Some(score) => *score,
            None => {
                let is_maker_turn = num_marked % 2 == 0;
                let mut best_remaining_score = if is_maker_turn { usize::MAX } else { 0 };
                'iter_verts: for v in g.iter_verts() {
                    if !marked.has_vert(v) {
                        if must_be_connected && num_marked >= 1 {
                            let mut is_adjacency_good = false;
                            'iter_adj: for w in g.adj_list[v].iter() {
                                if marked.has_vert(*w) {
                                    is_adjacency_good = true;
                                    break 'iter_adj;
                                }
                            }
                            if !is_adjacency_good {
                                continue 'iter_verts;
                            }
                        }
                        //try marking u.
                        let mut num_adj_marked = 0;
                        for u in g.adj_list[v].iter() {
                            if marked.has_vert(*u) {
                                num_adj_marked += 1;
                            }
                        }
                        let new_marked = marked.add_vert_immutable(v);
                        let remaining_score = marking_game_number_rec(g, new_marked, num_marked + 1,
                            must_be_connected, history).max(num_adj_marked);
                        if is_maker_turn && remaining_score < best_remaining_score {
                            best_remaining_score = remaining_score;
                        } else if !is_maker_turn && remaining_score > best_remaining_score {
                            best_remaining_score = remaining_score;
                        }
                    }
                }
                history.insert(marked, best_remaining_score);
                best_remaining_score
            }
        }
    }
}

pub fn marking_game_number(g: &Graph, must_be_connected: bool) -> u32 {
    let mut history = HashMap::new();
    (marking_game_number_rec(g, VertexSet::new(g.n), 0, must_be_connected, 
        &mut history) + 1) as u32
}

pub fn print_marking_game_strat(g: &Graph, must_be_connected: bool) {
    use utilities::chromatic_tools::*;
    let mut history = HashMap::new();
    let empty_set = VertexSet::new(g.n);
    marking_game_number_rec(g, empty_set, 0, must_be_connected, &mut history);
    let winning_num = *history.get(&empty_set).unwrap();
    let mut chromatic_history_maker = HashMap::new();
    let mut chromatic_history_breaker = HashMap::new();
    let coder = Coder::new(g.n, 1);
    for (set, score) in history.iter() {
        chromatic_history_maker.insert(coder.config_of_set_monochrome(*set, 0), *score <= winning_num);
        chromatic_history_breaker.insert(coder.config_of_set_monochrome(*set, 0), *score < winning_num);
    }
    println!("Maker wins with {} markings; marking game number = {}", winning_num, winning_num + 1);
    println!("\n  MAKER'S STRATEGY FOR MARKING NUMBER <= {}\n", winning_num);
    coder.print_history(&chromatic_history_maker);
    println!("\n  BREAKER'S STRATEGY FOR MARKING NUMBER >= {}\n", winning_num);
    coder.print_history(&chromatic_history_breaker);
}