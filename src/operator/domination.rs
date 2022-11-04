use crate::graph::*;

use utilities::*;

fn can_dominate_rec(g: &Graph, dominator: &mut Vec<bool>, last_pick: usize, remaining_picks: usize) -> bool {
    let n = g.n.to_usize();
    if remaining_picks == 0 {
        // actually test if dominator is a dominating set
        let mut is_dominating = true;
        for i in 0..n {
            if !dominator[i] {
                is_dominating &= g.adj_list[i].iter().any(|val| dominator[*val]);
            }
        }
        is_dominating
    } else {
        let mut can_dominate = false;
        'test_next_element: for i in (last_pick+1)..(n-remaining_picks+1) {
            dominator[i] = true;
            if can_dominate_rec(g, dominator, i, remaining_picks - 1) {
                can_dominate = true;
                break 'test_next_element;
            }
            dominator[i] = false;
        }
        can_dominate
    }
}

pub fn domination_number(g: &Graph) -> u32 {
    let mut number = 1;
    let n = g.n.to_usize();
    let mut dominator = vec![false; g.n.to_usize()];
    'find_domination_number: loop {
        for i in 0..(n-number+1) {
            dominator[i] = true;
            if can_dominate_rec(g, &mut dominator, i, number - 1) {
                break 'find_domination_number;
            }
            dominator[i] = false;
        }
        number += 1;
    }
    number as u32
}

pub fn total_domination_game_length(g: &Graph) -> u32 {
    let n = g.n.to_usize();

    //Check that every parent of a leaf has degree 2.
    for i in 0..n {
        if g.deg[i].equals(1) {
            let parent = g.adj_list[i][0];
            if !g.deg[parent].equals(2) {
                return 0;
            }
        }
    }

    let num_states = pow(2, n as u64) as usize;
    let mut turns_left: Vec<usize> = vec![n+1; num_states];
    let mut num_dominated: Vec<usize> = vec![0; num_states];
    let mut dominators_turn: Vec<bool> = vec![false; num_states];

    // Compute the number of totally dominated vertices of every configuration
    for i in 0..num_states {
        let mut dominated = vec![false; n];
        let mut sta = i;
        let mut num_played = 0;
        for v in 0..n {
            if sta % 2 == 1 {
                // v is in the set, so dominates
                num_played += 1;
                for j in g.adj_list[v].iter() {
                    dominated[*j] = true;
                }
            }
            sta /= 2;
        }
        num_dominated[i] = dominated.iter().map(|x| if *x { 1 } else { 0 }).sum();        
        dominators_turn[i] = num_played % 2 == 0;
    }

    // If no playable move, set to 0.
    for i in 0..num_states {
        let mut sta = i;
        let mut can_be_added = false;
        let mut diff_to_add_v = 1;
        for _v in 0..n {
            if sta % 2 == 0 {
                // v is not in the set - can we add it?
                if num_dominated[i + diff_to_add_v] > num_dominated[i] {
                    can_be_added = true;
                }
            }
            diff_to_add_v *= 2;
            sta /= 2;
        }
        if !can_be_added {
            // No vertex can be added, so there are no turns left.
            turns_left[i] = 0;
        }
    }

    // Only needs one iter as any set is processed after all its proper supersets.
    for i in (0..num_states).rev() {
        if turns_left[i] != 0 {
            let mut sta = i;
            let mut diff_to_add_v = 1;
            let mut optimal = if dominators_turn[i] { n } else { 0 };

            for _v in 0..n {
                if sta % 2 == 0 {
                    if num_dominated[i + diff_to_add_v] > num_dominated[i] {
                        // v can played
                        let child = turns_left[i + diff_to_add_v];
                        if child != n+1 && ((dominators_turn[i] && child < optimal) 
                                || (!dominators_turn[i] && child > optimal)) {
                            optimal = child;
                        }
                    }
                }
                diff_to_add_v *= 2;
                sta /= 2;
            }
            
            turns_left[i] = optimal + 1;
        }
    }
    /*
    for i in 0..num_states {
        let mut sta = i;
        for _v in 0..n {
            print!("{}", sta % 2);
            sta /= 2;
        }
        println!(": {}", turns_left[i]);
    }*/

    turns_left[0] as u32
}