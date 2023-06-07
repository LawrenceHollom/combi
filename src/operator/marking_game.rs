use std::collections::HashMap;

use crate::graph::*;

use utilities::vertex_tools::*;

fn marking_game_number_rec(g: &Graph, marked: VertexSet, num_marked: usize, 
        history: &mut HashMap<VertexSet, usize>) -> usize {
    if g.n.at_most(num_marked) {
        0
    } else {
        match history.get(&marked) {
            Some(score) => *score,
            None => {
                let is_maker_turn = num_marked % 2 == 0;
                let mut best_remaining_score = if is_maker_turn { usize::MAX } else { 0 };
                for v in g.iter_verts() {
                    if !marked.has_vert(v) {
                        //try marking u.
                        let mut num_adj_marked = 0;
                        for u in g.adj_list[v].iter() {
                            if marked.has_vert(*u) {
                                num_adj_marked += 1;
                            }
                        }
                        let new_marked = marked.add_vert_immutable(v);
                        let remaining_score = marking_game_number_rec(g, new_marked, num_marked + 1,
                            history).max(num_adj_marked);
                        if is_maker_turn && remaining_score < best_remaining_score {
                            best_remaining_score = remaining_score;
                        } else if !is_maker_turn && remaining_score > best_remaining_score {
                            best_remaining_score = remaining_score;
                        }
                    }
                }
                marked.print();
                history.insert(marked, best_remaining_score);
                best_remaining_score
            }
        }
    }
}

pub fn marking_game_number(g: &Graph) -> u32 {
    let mut history = HashMap::new();
    (marking_game_number_rec(g, VertexSet::new(g.n), 0, &mut history) + 1) as u32
}