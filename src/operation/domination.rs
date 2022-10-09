use crate::graph::*;

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