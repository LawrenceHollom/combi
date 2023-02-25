use crate::graph::*;

use utilities::*;
use utilities::rational::*;

fn get_nbhd_code(g: &Graph, v: usize) -> u128 {
    let mut dominion = 1 << v;
    for u in g.adj_list[v].iter() {
        dominion |= 1 << *u;
    }
    dominion
}

// use u128s to do domination considerations.
fn min_dominator_bfs(g: &Graph, dominator: &mut Vec<bool>, best_dominator: &mut Vec<bool>, 
        num_picked: usize, last_pick: usize, best_set: usize, lower_bound: Option<usize>,
        max_nbrs: &Vec<usize>, predominations: u128) -> usize {
    let n = g.n.to_usize();
    if num_picked >= best_set {
        return best_set;
    }
    if lower_bound.map_or(false, |x| best_set < x) {
        // We've found a dominating set below the lower bound, so give up.
        return best_set;
    }
    let everything = (1_u128 << n) - 1;
    let mut dominion = predominations;
    for (v, is_dom) in dominator.iter().enumerate() {
        if *is_dom {
            dominion |= get_nbhd_code(g, v);
        }
    }
    if dominion == everything {
        // We cover everything, so this is a dominating set setting a new record.
        // Store it in best_dominator
        for (i, dom) in dominator.iter().enumerate() {
            best_dominator[i] = *dom;
        }
        if n >= 50 {
            // Keep track of output when n is large
            println!("Found better! {}", num_picked);
        }
        return num_picked;
    }

    let mut hittable = 0;
    // Test that it's still possible to hit everything.
    for i in (last_pick + 1)..n {
        hittable |= get_nbhd_code(g, i);
    }
    if (hittable | dominion) ^ everything != 0 {
        // Cannot possibly dominate. Give up.
        return best_set;
    }

    // Add another vertex.
    // First test which vertices are worth adding; i.e. not superceded by any others.
    // subdominia[v] is stuff covered by v not in dominion already.
    let mut subdominia = vec![0; n];

    for vert in (last_pick + 1)..n {
        subdominia[vert] = get_nbhd_code(g, vert) & !dominion;
    }

    let mut new_best_set = best_set;
    
    let mut minimax_undominated_nbr = n - 1;
    for u in 0..n {
        if (dominion >> u) % 2 == 0 && max_nbrs[u] < minimax_undominated_nbr {
            minimax_undominated_nbr = max_nbrs[u];
        }
    }

    for vert in (last_pick + 1)..=minimax_undominated_nbr {
        let mut is_worth_adding = true;
        'test_if_worth_it: for u in (last_pick + 1)..n {
            if subdominia[vert] == 0 {
                // vert would add nothing; give up immediately
                is_worth_adding = false;
                break 'test_if_worth_it;
            }
            if u != vert && subdominia[vert] & !subdominia[u] == 0 && (subdominia[u] != subdominia[vert] || u < vert) {
                // u is better than v
                is_worth_adding = false;
                break 'test_if_worth_it;
            }
        }
        if is_worth_adding {
            dominator[vert] = true;
            let value = min_dominator_bfs(g, dominator, best_dominator, num_picked + 1, 
            vert, new_best_set, lower_bound, max_nbrs, predominations);
            if value < new_best_set {
                new_best_set = value;
            }
            dominator[vert] = false;
        }
    }

    new_best_set
}

fn dominate_greedy(g: &Graph, predominations: u128) -> usize {
    let n = g.n.to_usize();

    let mut dominion = predominations;
    let mut dominator = vec![false; n];
    let mut gamma = 0;
    let everything = (1_u128 << n) - 1;

    while dominion != everything {
        let mut max_cover = 0;
        let mut vert = n;
        for v in 0..n {
            if !dominator[v] {
                let mut cover_size = 0;
                if (dominion >> v) % 2 == 0 {
                    cover_size += 1;
                }
                for u in g.adj_list[v].iter() {
                    if (dominion >> *u) % 2 == 0 {
                        cover_size += 1;
                    }
                }
                if cover_size > max_cover {
                    max_cover = cover_size;
                    vert = v;
                }
            }
        }
        // Add vertex.
        dominator[vert] = true;
        gamma += 1;
        dominion |= get_nbhd_code(g, vert);
    }

    gamma
}

fn get_max_nbrs(g: &Graph) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut max_nbrs = vec![0; n];
    for u in 0..n {
        for v in g.adj_list[u].iter() {
            if *v > max_nbrs[u] {
                max_nbrs[u] = *v;
            }
        }
        if u > max_nbrs[u] { 
            max_nbrs[u] = u;
        }
    }
    max_nbrs
}

pub fn domination_number_with_predominations(g: &Graph, predominations: u128) -> u32 {
    let n = g.n.to_usize();
    let mut dominator = vec![false; n];
    let mut best_dominator = vec![false; n];
    let mut number = dominate_greedy(g, predominations);
    if n >= 60 {
        // Too big to ever realistically finish, so might as well fail verbosely
        println!("Starting! Greedy: {}", number);
    }
    let h = g.order_by_nbhd();
    let max_nbrs = get_max_nbrs(&h);
    for i in 0..=max_nbrs[0] {
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, &mut best_dominator, 1, i, number, None, &max_nbrs, predominations);
        if this_number < number {
            number = this_number;
        }
        dominator[i] = false;
    }

    number as u32
}

pub fn domination_number(g: &Graph) -> u32 {
    domination_number_with_predominations(g, 0)
}

pub fn is_domination_number_at_least(g: &Graph, lower_bound: usize) -> bool {
    let n = g.n.to_usize();
    let mut dominator = vec![false; n];
    let mut best_dominator = vec![false; n];
    let mut number = dominate_greedy(g, 0);

    let h = g.order_by_nbhd();
    let max_nbrs = get_max_nbrs(&h);
    'test_start: for i in 0..=max_nbrs[0] {
        if number < lower_bound {
            break 'test_start;
        }
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, &mut best_dominator, 1, i, number, Some(lower_bound), &max_nbrs, 0);
        if this_number < number {
            number = this_number;
        }
        dominator[i] = false;
    }
    number >= lower_bound
}

pub fn domination_redundancy(g: &Graph) -> Rational {
    let n = g.n.to_usize();
    let mut dominator = vec![false; n];
    let mut best_dominator = vec![false; n];
    let mut number = dominate_greedy(g, 0);

    let h = g.order_by_nbhd();
    let max_nbrs = get_max_nbrs(&h);
    for i in 0..=max_nbrs[0] {
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, &mut best_dominator, 1, i, number + 1, None, &max_nbrs, 0);
        if this_number < number {
            number = this_number;
        }
        dominator[i] = false;
    }
    // now compute the domination redundancy with the best_dominator
    let mut dominations = 0;
    for (i, is_dominating) in best_dominator.iter().enumerate() {
        if *is_dominating {
            dominations += g.deg[i].to_usize() + 1;
        }
    }
    Rational::new_fraction(dominations, n) - Rational::ONE
}

fn edge_domination_dfs(g: &Graph, dominator: &mut Vec<bool>, num_picked: usize, min_pick: usize, best_set: usize, lossless: bool) -> usize {
    let n = g.n.to_usize();
    if num_picked >= best_set {
        // We've already picked too much.
        return best_set;
    }

    //println!("edge_domination_dfs, num_picked: {}, min_pick: {}, best_set: {}, dominator: {:?}", num_picked, min_pick, best_set, dominator);

    // Is it dominating?
    let mut could_ever_dominate = true;
    let mut is_dominating = true;

    let mut pickable = vec![true; n];

    for u in 0..n {
        if dominator[u] {
            pickable[u] = false;
            if lossless {
                for v in g.adj_list[u].iter() {
                    pickable[*v] = false;
                }
            }
        }
    }

    'test_domination: for u in 0..n {
        if !dominator[u] {
            for v in g.adj_list[u].iter() {
                if !dominator[*v] {
                    is_dominating = false;
                    if (u < min_pick && *v < min_pick) || (lossless && !pickable[u] && !pickable[*v]) {
                        could_ever_dominate = false;
                        break 'test_domination;
                    } else if u >= min_pick {
                        break 'test_domination;
                    }
                }
            }
        }
    }

    if lossless && could_ever_dominate {
        'find_isolated_lossless: for u in 0..n {
            if pickable[u] {
                let mut bad = true;
                'test_nbrs: for v in g.adj_list[u].iter() {
                    if pickable[*v] || dominator[*v] {
                        bad = false;
                        break 'test_nbrs;
                    }
                }
                if bad {
                    could_ever_dominate = false;
                    break 'find_isolated_lossless;
                }
            }
        }
    }

    if is_dominating {
        return num_picked;
    } else if !could_ever_dominate {
        return best_set;
    }
    
    let mut vert = min_pick;

    while vert < n && !pickable[vert] {
        vert += 1;
    }

    if vert == n {
        // end of the road. 
        return best_set;
    }

    // It's not dominating and we can pick more. First try not picking anything.
    let mut number = edge_domination_dfs(g, dominator, num_picked, vert + 1, best_set, lossless);
    
    // Now try picking something.
    for u in g.adj_list[vert].iter() {
        if *u > vert && pickable[*u] {
            dominator[*u] = true;
            dominator[vert] = true;
            //println!("Picked {}~{}; calling.", vert, *u);
            let value = edge_domination_dfs(g, dominator, num_picked + 1, vert + 1, number, lossless);
            if value < number {
                number = value;
            }

            dominator[*u] = false;
            dominator[vert] = false;
        }
    }

    number
}

pub fn edge_domination_number(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let mut dominator = vec![false; n];
    edge_domination_dfs(g, &mut dominator, 0, 0, n/2, false) as u32
}

pub fn has_regular_lossless_edge_dominator(g: &Graph) -> bool {
    if !g.is_regular() {
        panic!("This function is for regular graphs only!")
    } else {
        let n = g.n.to_usize();
        let mut dominator = vec![false; n];
        let delta = g.deg[0].to_usize();
        let denom = if delta % 2 == 0 { 2 * delta - 1} else { 2 * (2 * delta - 1) };
        if n % denom != 0 {
            panic!("Lossless edge domination only possible for Delta={} when n divisible by {}", delta, denom);
        } else {
            let gamma_e = edge_domination_dfs(g, &mut dominator, 0, 0, n, true);
            gamma_e == delta * n / (2 * (2 * delta - 1))
        }
    }
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
                if sta % 2 == 0 && num_dominated[i + diff_to_add_v] > num_dominated[i] {
                    // v can played
                    let child = turns_left[i + diff_to_add_v];
                    if child != n+1 && ((dominators_turn[i] && child < optimal) 
                            || (!dominators_turn[i] && child > optimal)) {
                        optimal = child;
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