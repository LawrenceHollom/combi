use crate::entity::poset::*;

use utilities::vertex_tools::*;
use utilities::rational::*;

fn iterate_extensions_rec(p: &Poset, placed: VertexSet, num_placed: usize, gt_count: &mut VertexVec<VertexVec<u64>>) -> u64{
    if num_placed == p.order.to_usize() {
        return 1;
    }
    let mut count = 0;
    for v in p.iter_verts() {
        if !placed.has_vert(v) {
            let mut can_place = true;
            'test_coverees: for u in p.covered_by[v].iter() {
                if !placed.has_vert(u) {
                    can_place = false;
                    break 'test_coverees;
                }
            }
            if can_place {
                // sub_count is how many extensions there are with v placed.
                let new_placed = placed.add_vert_immutable(v);
                let sub_count = iterate_extensions_rec(p, new_placed, num_placed + 1, gt_count);
                for u in p.iter_verts() {
                    if u != v && !placed.has_vert(u) {
                        // We need to count this contribution to P(u > v)
                        gt_count[u][v] += sub_count;
                    }
                }
                count += sub_count;
            }
        }
    }
    count
}

pub fn print_relation_probabilities(p: &Poset) {
    // Iterate through every linear extension of the poset
    let mut gt_count = VertexVec::new(p.order, &VertexVec::new(p.order, &0));
    let count = iterate_extensions_rec(p, VertexSet::new(p.order), 0, &mut gt_count);
    println!("There are {} linear extensions of the poset.", count);
    println!("Counts of how many times we have u > v (u = y_axis, v = x_axis):");
    let rows = gt_count.iter().map(
        |row| ("Row!", row.to_vec_of_strings())
    ).collect::<Vec<(&str, VertexVec<String>)>>();
    print_vertex_table(rows);
    let mut max_balance = 0;
    let mut best_pair = (Vertex::ZERO, Vertex::ZERO);
    for (u, v) in p.iter_pairs() {
        let this_count = gt_count[u][v].min(count - gt_count[u][v]);
        if this_count > max_balance {
            max_balance = this_count;
            best_pair = (u, v);
        }
    }
    let balance = (max_balance as f64) / (count as f64);
    println!("A maximally balanced pair ({} / {} ~ {:.4}): {:?}", max_balance, count, balance, best_pair);
}

pub fn balance_constant(p: &Poset) -> Rational {
    let mut gt_count = VertexVec::new(p.order, &VertexVec::new(p.order, &0));
    let count = iterate_extensions_rec(p, VertexSet::new(p.order), 0, &mut gt_count);
    let mut balance: u64 = 0;
    for (u, v) in p.iter_pairs() {
        let this_balance = gt_count[u][v].min(count - gt_count[u][v]);
        balance = balance.max(this_balance);
    }
    Rational::new_fraction(balance as usize, count as usize)
}

pub fn num_linear_extensions(p: &Poset) -> u32 {
    let mut gt_count = VertexVec::new(p.order, &VertexVec::new(p.order, &0));
    let count = iterate_extensions_rec(p, VertexSet::new(p.order), 0, &mut gt_count);
    count as u32
}

fn count_extensions_up_to_cap_rec(p: &Poset, placed: VertexSet, num_placed: usize, cap: u64) -> u64 {
    if num_placed == p.order.to_usize() {
        return 1;
    }
    let mut count = 0;
    for v in p.iter_verts() {
        if !placed.has_vert(v) {
            let mut can_place = true;
            'test_coverees: for u in p.covered_by[v].iter() {
                if !placed.has_vert(u) {
                    can_place = false;
                    break 'test_coverees;
                }
            }
            if can_place {
                // sub_count is how many extensions there are with v placed.
                // We can finish if we find at least new_cap extensions.
                let new_cap = cap - count;
                let new_placed = placed.add_vert_immutable(v);
                let sub_count = count_extensions_up_to_cap_rec(p, new_placed, num_placed + 1, new_cap);
                count += sub_count;
                if count >= cap {
                    // If we've exceeded the cap, then no need to go further.
                    return cap
                }
            }
        }
    }
    count
}

pub fn is_num_extensions_less_than(p: &Poset, k: usize) -> bool {
    count_extensions_up_to_cap_rec(p, VertexSet::new(p.order), 0, k as u64) < k as u64
}

/**
 * Prints some heuristics about whether we expect the poset to be balanced or not.
 */
pub fn print_heuristics(p: &Poset) {
    println!("Comparably balanced pairs:");
    for (u, v) in p.iter_pairs() {
        if p.incomparable(u, v) && p.downsets[u].size() + p.upsets[v].size() == p.upsets[u].size() + p.downsets[v].size() {
            println!("({}, {})", u, v)
        }
    }
    let mut width_3_levels = 0;
    let mut h_count = vec![0; p.height];
    for v in p.iter_verts() {
        let h = p.heights[v];
        h_count[h] += 1;
        if h_count[h] == 3 {
            width_3_levels += 1;
        }
    }
    println!("{} levels of width 3", width_3_levels);
}

/**
 * A pair of vertices are equitable if they have the same difference in sizes between 
 * their upsets and downsets
 */
pub fn is_heuristically_balanced(p: &Poset) -> bool {
    for (u, v) in p.iter_pairs() {
        if p.incomparable(u, v) && p.downsets[u].size() + p.upsets[v].size() == p.upsets[u].size() + p.downsets[v].size() {
            return true
        }
    }
    false
}