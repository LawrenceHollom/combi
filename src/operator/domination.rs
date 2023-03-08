use crate::graph::*;

use utilities::*;
use utilities::rational::*;
use utilities::vertex_tools::*;

fn get_nbhd_code(g: &Graph, v: Vertex) -> VertexSet {
    let mut dominion = VertexSet::new();
    dominion.add_vert(v);
    for u in g.adj_list[v].iter() {
        dominion.add_vert(*u);
    }
    dominion
}

// use u128s to do domination considerations.
fn min_dominator_bfs(g: &Graph, dominator: &mut VertexVec<bool>, best_dominator: &mut VertexVec<bool>, 
        num_picked: usize, last_pick: Vertex, best_set: usize, lower_bound: Option<usize>,
        max_nbrs: &VertexVec<Vertex>, predominations: VertexSet) -> usize {
    if num_picked >= best_set {
        return best_set;
    }
    if lower_bound.map_or(false, |x| best_set < x) {
        // We've found a dominating set below the lower bound, so give up.
        return best_set;
    }
    let everything = VertexSet::everything(g.n);
    let mut dominion = predominations;
    for (v, is_dom) in dominator.iter_enum() {
        if *is_dom {
            dominion.add_all(get_nbhd_code(g, v));
        }
    }
    if dominion == everything {
        // We cover everything, so this is a dominating set setting a new record.
        // Store it in best_dominator
        for (i, dom) in dominator.iter_enum() {
            best_dominator[i] = *dom;
        }
        if g.n.at_least(50) {
            // Keep track of output when n is large
            println!("Found better! {}", num_picked);
        }
        return num_picked;
    }

    let mut hittable = VertexSet::new();
    // Test that it's still possible to hit everything.
    for i in last_pick.incr().iter_from(g.n) {
        hittable.add_all(get_nbhd_code(g, i));
    }
    if (hittable.union(&dominion)).xor(&everything).is_nonempty() {
        // Cannot possibly dominate. Give up.
        return best_set;
    }

    // Add another vertex.
    // First test which vertices are worth adding; i.e. not superceded by any others.
    // subdominia[v] is stuff covered by v not in dominion already.
    let mut subdominia: VertexVec<VertexSet> = VertexVec::new(g.n, &VertexSet::new());

    for vert in last_pick.incr().iter_from(g.n) {
        subdominia[vert] = get_nbhd_code(g, vert).inter(&dominion.not());
    }

    let mut new_best_set = best_set;
    
    let mut minimax_undominated_nbr = g.n.to_max_vertex();
    for u in g.n.iter_verts() {
        if !dominion.has_vert(u) && max_nbrs[u] < minimax_undominated_nbr {
            minimax_undominated_nbr = max_nbrs[u];
        }
    }

    for vert in last_pick.incr().iter_from_to_incl(minimax_undominated_nbr) {
        let mut is_worth_adding = true;
        'test_if_worth_it: for u in last_pick.incr().iter_from(g.n) {
            if subdominia[vert].is_empty() {
                // vert would add nothing; give up immediately
                is_worth_adding = false;
                break 'test_if_worth_it;
            }
            if u != vert && subdominia[vert].inter(&subdominia[u].not()).is_empty() 
                    && (subdominia[u] != subdominia[vert] || u < vert) {
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

fn dominate_greedy(g: &Graph, predominations: VertexSet, best_dominator: &mut VertexVec<bool>) -> usize {
    let mut dominion = predominations;
    let mut dominator = VertexVec::new(g.n, &false);
    let mut gamma = 0;
    let everything = VertexSet::everything(g.n);

    while dominion != everything {
        let mut max_cover = 0;
        let mut vert = None;
        for v in g.n.iter_verts() {
            if !dominator[v] {
                let mut cover_size = 0;
                if !dominion.has_vert(v) {
                    cover_size += 1;
                }
                for u in g.adj_list[v].iter() {
                    if !dominion.has_vert(*u) {
                        cover_size += 1;
                    }
                }
                if cover_size > max_cover {
                    max_cover = cover_size;
                    vert = Some(v);
                }
            }
        }
        // Add vertex.
        match vert {
            Some(vert) => {
                dominator[vert] = true;
                gamma += 1;
                dominion.add_all(get_nbhd_code(g, vert));
            }
            None => panic!("Well, shit.")
        }
    }

    for i in g.n.iter_verts() {
        best_dominator[i] = dominator[i];
    }

    gamma
}

fn get_max_nbrs(g: &Graph) -> VertexVec<Vertex> {
    let mut max_nbrs = VertexVec::new_fn(g.n, |v| v);
    for u in g.n.iter_verts() {
        for v in g.adj_list[u].iter() {
            if *v > max_nbrs[u] {
                max_nbrs[u] = *v;
            }
        }
    }
    max_nbrs
}

fn compute_domination_number_and_set(g: &Graph, predominations: VertexSet, best_dominator: &mut VertexVec<bool>) -> u32 {
    let mut dominator = VertexVec::new(g.n, &false);
    let mut number = dominate_greedy(g, predominations, best_dominator);
    if g.n.at_least(60) {
        // Too big to ever realistically finish, so might as well fail verbosely
        println!("Starting! Greedy: {}", number);
    }
    let (h, ordering_inv) = g.order_by_nbhd();

    // Fix predominations to look at ordering_inv.
    let mut h_predominations = VertexSet::new();
    for i in g.n.iter_verts() {
        if predominations.has_vert(i) {
            h_predominations.add_vert(ordering_inv[i]);
        }
    }

    let max_nbrs = get_max_nbrs(&h);
    let mut first_unpredominated = Vertex::ZERO;
    let mut was_greedy_best = true;
    while h_predominations.has_vert(first_unpredominated) {
        first_unpredominated.incr_inplace();
    }
    for i in Vertex::ZERO.iter_from_to_incl(max_nbrs[first_unpredominated]) {
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, best_dominator, 1, i, number, None, &max_nbrs, h_predominations);
        if this_number < number {
            was_greedy_best = false;
            number = this_number;
        }
        dominator[i] = false;
    }

    if !was_greedy_best {
        // We need to pull best_dominator back again.
        let dom_sta = best_dominator.to_owned();
        for i in g.n.iter_verts() {
            best_dominator[i] = dom_sta[ordering_inv[i]];
        }
    }

    number as u32
}

pub fn domination_number_with_predominations(g: &Graph, predominations: VertexSet) -> u32 {
    let mut best_dominator = VertexVec::new(g.n, &false);
    compute_domination_number_and_set(g, predominations, &mut best_dominator)
}

pub fn domination_number(g: &Graph) -> u32 {
    domination_number_with_predominations(g, VertexSet::new())
}

pub fn print_random_dominator(g: &Graph) {
    let mut best_dominator = VertexVec::new(g.n, &false);
    let (h, ordering_inv) = g.randomly_permute_vertices();
    
    let mut ordering = VertexVec::new(g.n, &Vertex::ZERO);
    for i in g.n.iter_verts() {
        ordering[ordering_inv[i]] = i;
    }

    let _ = compute_domination_number_and_set(&h, VertexSet::new(), &mut best_dominator);
    let mut dominator_verts: Vec<Vertex> = 
        best_dominator.iter_enum()
                    .filter(|(_i, x)| **x)
                    .map(|(i, _x)| ordering[i])
                    .collect();
    dominator_verts.sort();
    println!("{:?}", dominator_verts);
}

pub fn is_domination_number_at_least(g: &Graph, lower_bound: usize) -> bool {
    let mut dominator = VertexVec::new(g.n, &false);
    let mut best_dominator =  VertexVec::new(g.n, &false);
    let mut number = dominate_greedy(g, VertexSet::new(), &mut best_dominator);

    let (h, _ordering_inv) = g.order_by_nbhd();
    let max_nbrs = get_max_nbrs(&h);
    'test_start: for i in Vertex::ZERO.iter_from_to_incl(max_nbrs[Vertex::ZERO]) {
        if number < lower_bound {
            break 'test_start;
        }
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, &mut best_dominator, 1, i, 
            number, Some(lower_bound), &max_nbrs, VertexSet::new());
        if this_number < number {
            number = this_number;
        }
        dominator[i] = false;
    }
    number >= lower_bound
}

pub fn domination_redundancy(g: &Graph) -> Rational {
    let mut dominator = VertexVec::new(g.n, &false);
    let mut best_dominator = VertexVec::new(g.n, &false);
    let mut number = dominate_greedy(g, VertexSet::new(), &mut best_dominator);

    let (h, _ordering_inv) = g.order_by_nbhd();
    let max_nbrs = get_max_nbrs(&h);
    for i in Vertex::ZERO.iter_from_to_incl(max_nbrs[Vertex::ZERO]) {
        dominator[i] = true;
        let this_number = min_dominator_bfs(&h, &mut dominator, &mut best_dominator, 1, i, 
            number + 1, None, &max_nbrs, VertexSet::new());
        if this_number < number {
            number = this_number;
        }
        dominator[i] = false;
    }
    // now compute the domination redundancy with the best_dominator
    let mut dominations = 0;
    for (i, is_dominating) in best_dominator.iter_enum() {
        if *is_dominating {
            dominations += g.deg[i].to_usize() + 1;
        }
    }
    Rational::new_fraction(dominations, g.n.to_usize()) - Rational::ONE
}

fn edge_domination_dfs(g: &Graph, dominator: &mut VertexVec<bool>, num_picked: usize, min_pick: Vertex, best_set: usize, lossless: bool) -> usize {
    if num_picked >= best_set {
        // We've already picked too much.
        return best_set;
    }

    //println!("edge_domination_dfs, num_picked: {}, min_pick: {}, best_set: {}, dominator: {:?}", num_picked, min_pick, best_set, dominator);

    // Is it dominating?
    let mut could_ever_dominate = true;
    let mut is_dominating = true;

    let mut pickable = VertexVec::new(g.n, &true);

    for u in g.n.iter_verts() {
        if dominator[u] {
            pickable[u] = false;
            if lossless {
                for v in g.adj_list[u].iter() {
                    pickable[*v] = false;
                }
            }
        }
    }

    'test_domination: for u in g.n.iter_verts() {
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
        'find_isolated_lossless: for u in g.n.iter_verts() {
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

    while !vert.is_n(g.n) && !pickable[vert] {
        vert.incr_inplace();
    }

    if vert.is_n(g.n) {
        // end of the road. 
        return best_set;
    }

    // It's not dominating and we can pick more. First try not picking anything.
    let mut number = edge_domination_dfs(g, dominator, num_picked, vert.incr(), best_set, lossless);
    
    // Now try picking something.
    for u in g.adj_list[vert].iter() {
        if *u > vert && pickable[*u] {
            dominator[*u] = true;
            dominator[vert] = true;
            //println!("Picked {}~{}; calling.", vert, *u);
            let value = edge_domination_dfs(g, dominator, num_picked + 1, vert.incr(), number, lossless);
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
    let mut dominator = VertexVec::new(g.n, &false);
    edge_domination_dfs(g, &mut dominator, 0, Vertex::ZERO, g.n.to_usize()/2, false) as u32
}

pub fn has_regular_lossless_edge_dominator(g: &Graph) -> bool {
    if !g.is_regular() {
        panic!("This function is for regular graphs only!")
    } else {
        let n = g.n.to_usize();
        let mut dominator = VertexVec::new(g.n, &false);
        let delta = g.deg[Vertex::ZERO].to_usize();
        let denom = if delta % 2 == 0 { 2 * delta - 1} else { 2 * (2 * delta - 1) };
        if n % denom != 0 {
            panic!("Lossless edge domination only possible for Delta={} when n divisible by {}", delta, denom);
        } else {
            let gamma_e = edge_domination_dfs(g, &mut dominator, 0, Vertex::ZERO, n, true);
            gamma_e == delta * n / (2 * (2 * delta - 1))
        }
    }
}

pub fn total_domination_game_length(g: &Graph) -> u32 {
    let n = g.n.to_usize();

    //Check that every parent of a leaf has degree 2.
    for i in g.n.iter_verts() {
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
        let mut dominated = VertexVec::new(g.n, &false);
        let mut sta = i;
        let mut num_played = 0;
        for v in g.n.iter_verts() {
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
        for _v in g.n.iter_verts(){
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

            for _v in g.n.iter_verts(){
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

    turns_left[0] as u32
}

#[cfg(test)]
mod tests {
    use crate::graph::*;
    use super::*;

    #[test]
    fn test_domination_1() {
        assert_eq!(domination_number(&Graph::test_graph(1)), 2);
    }

    #[test]
    fn test_domination_2() {
        assert_eq!(domination_number(&Graph::test_graph(2)), 5);
    }

    #[test]
    fn test_domination_3() {
        assert_eq!(domination_number(&Graph::test_graph(3)), 3);
    }
}