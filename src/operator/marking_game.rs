use std::collections::HashMap;

use crate::graph::*;

use utilities::vertex_tools::*;

fn marking_game_number_rec(g: &Graph, marked: VertexSet, num_marked: usize, 
        history: &mut HashMap<VertexSet, usize>, current_score: usize, min_final_score: usize) -> usize {
    if g.n.at_most(num_marked) {
        current_score
    } else {
        match history.get(&marked) {
            Some(score) => *score,
            None => {
                let is_maker_turn = num_marked % 2 == 0;
                let mut running_score = current_score;
                let mut final_score = if is_maker_turn { usize::MAX } else { 0 };
                let mut running_min_final_score = min_final_score;
                'test_verts: for v in g.iter_verts() {
                    if !marked.has_vert(v) {
                        //try marking u.
                        let mut num_adj_marked = 0;
                        for u in g.adj_list[v].iter() {
                            if marked.has_vert(*u) {
                                num_adj_marked += 1;
                            }
                        }
                        // We have to mark it at some point.
                        running_score = running_score.max(num_adj_marked);
                        if is_maker_turn && num_adj_marked > running_min_final_score {
                            // maker can give up as better has been done.
                            break 'test_verts;
                        } else {
                            let new_marked = marked.add_vert_immutable(v);
                            let resulting_score = marking_game_number_rec(g, new_marked, num_marked + 1,
                                history, running_score, running_min_final_score);
                            if is_maker_turn && resulting_score < running_min_final_score {
                                running_min_final_score = resulting_score;
                            }
                            if is_maker_turn && resulting_score < final_score {
                                final_score = resulting_score;
                            } else if !is_maker_turn && resulting_score > final_score {
                                final_score = resulting_score;
                            }
                        }
                    }
                }
                history.insert(marked, final_score);
                final_score
            }
        }
    }
}

pub fn marking_game_number(g: &Graph) -> u32 {
    let mut history = HashMap::new();
    (marking_game_number_rec(g, VertexSet::new(g.n), 0, &mut history, 
        0, usize::MAX) + 1) as u32
}