use crate::graph::*;

fn largest_rec(g: &Graph, target_edgeness: bool, picks: &mut Vec<usize>, num_made: usize, remaining_picks: usize) -> bool {
    if remaining_picks == 0 {
        true
    } else {
        let n = g.n.to_usize();
        let mut out = false;
        'find_next: for next in (picks[num_made - 1] + 1)..=(n-remaining_picks) {
            let mut is_valid = true;
            'test_next: for pick in picks.iter() {
                if g.adj[*pick][next] != target_edgeness {
                    is_valid = false;
                    break 'test_next;
                }
            }
            if is_valid {
                picks[num_made] = next;
                out = largest_rec(g, target_edgeness, picks, num_made + 1, remaining_picks - 1);
                if out {
                    break 'find_next;
                }
            }
        }
        out
    }
}

fn largest(g: &Graph, target_edgeness: bool) -> u32 {
    let n = g.n.to_usize();
    let mut out = 1;
    'test_size: for size in 2..=n {
        let mut progress = false;
        'first_pick: for first_pick in 0..=(n-size) {
            if largest_rec(g, target_edgeness, &mut vec![first_pick; n], 1, size-1) {
                out = size;
                progress = true;
                break 'first_pick;
            }
        }
        if !progress {
            break 'test_size;
        }
    }
    out as u32
}

pub fn largest_clique(g: &Graph) -> u32 {
    largest(g, true)
}

pub fn independence_number(g: &Graph) -> u32 {
    largest(g, false)
}