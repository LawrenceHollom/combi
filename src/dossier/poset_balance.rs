use crate::entity::poset::*;

use utilities::vertex_tools::*;
use utilities::rational::*;

fn iterate_extensions_rec(p: &Poset, placed: VertexSet, num_placed: usize, gt_count: &mut VertexVec<VertexVec<u64>>) -> u64 {
    if num_placed == p.order.to_usize() {
        return 1;
    }
    let mut count = 0;
    for v in p.iter_verts() {
        if !placed.has_vert(v) {
            let mut can_place = true;
            'test_coverees: for u in p.lower_covers[v].iter() {
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
    let names: Vec<String> = p.order.iter_verts().map(|v| v.to_string()).collect();
    let rows = gt_count.iter().enumerate().map(
        |(i, row)| (names[i].as_str(), row.to_vec_of_strings())
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
            'test_coverees: for u in p.lower_covers[v].iter() {
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

#[derive(Debug)]
struct NStructure {
    top_left: Vertex,
    top_right: Vertex,
    bottom_left: Vertex,
    // bottom_right: Vertex,
}

fn attach_cap(p: &Poset) -> Poset {
    let n_struct = self::find_maximal_n(p).expect("Can only call balance_as_cap on posets with maximal N!");
    let order = p.order.incr_by(4);
    let mut gt = VertexVec::new(order, &VertexVec::new(order, &false));
    for (x, y) in p.order.iter_pairs() {
        gt[x][y] = p.gt[x][y];
        gt[y][x] = p.gt[y][x];
    }

    // Attach the N-struct.
    let tr = order.to_max_vertex();
    let tl = tr.decr();
    let br = tl.decr();
    let bl = br.decr();
    gt[tr][br] = true;
    gt[tl][bl] = true;
    gt[tl][br] = true;
    gt[bl][n_struct.top_left] = true;
    gt[br][n_struct.top_right] = true;
    gt[bl][n_struct.top_right] = true;
    gt[tr][n_struct.top_left] = true;
    gt[br][n_struct.bottom_left] = true;

    // Construct our new poset, with the ladder attached.
    Poset::of_transitive_closure(gt, crate::constructor::Constructor::Special)
}

/**
 * Assuming that P has a maximal N, we attach the start of a ladder to
 * this N so that that top bit can't count as unbalanced, and don't
 * count the new top elements.
 * We construct a new poset Q with a bit of the ladder attached.
 */
pub fn balance_as_cap(p: &Poset) -> Rational {
    let q = attach_cap(p);

    let mut gt_count = VertexVec::new(q.order, &VertexVec::new(q.order, &0));
    let count = iterate_extensions_rec(&q, VertexSet::new(q.order), 0, &mut gt_count);
    // println!("Counted as cap; there are {} extensions.", count);

    let mut balance: u64 = 0;
    // Now check for balancedness, but ignore the top two vertices.
    for (u, v) in p.order.incr_by(2).iter_pairs() {
        let this_balance = gt_count[u][v].min(count - gt_count[u][v]);
        balance = balance.max(this_balance);
    }
    Rational::new_fraction(balance as usize, count as usize)
}

/**
 * Get the most balanced pair, considering only those pairs in which one
 * element is minimal in P.
 */
pub fn balance_with_min(p: &Poset) -> Rational {
    let mut gt_count = VertexVec::new(p.order, &VertexVec::new(p.order, &0));
    let count = iterate_extensions_rec(p, VertexSet::new(p.order), 0, &mut gt_count);
    let mut balance = 0;
    for (u, v) in p.iter_pairs() {
        // We only care if one of u and v is minimal.
        if p.downsets[u].is_empty() || p.downsets[v].is_empty() {
            let this_balance = gt_count[u][v].min(count - gt_count[u][v]);
            balance = balance.max(this_balance);
        }
    }
    Rational::new_fraction(balance as usize, count as usize)
}

pub fn print_cap_balance(p: &Poset) {
    let q = attach_cap(p);
    
    println!("\n ~~~~~~~~ ORIGINAL POSET ~~~~~~~~ \n");
    p.print_hasse();
    println!("\n ~~~~~~~~ CAPPED POSET ~~~~~~~~ \n");
    q.print_hasse();
    
    print_relation_probabilities(&q);
}

/**
 * Finds a maximal N in p if it exists, and returns it.
 */
fn find_maximal_n(p: &Poset) -> Option<NStructure> {
    let mut maximal_elements = vec![];
    for v in p.iter_verts() {
        if p.upsets[v].is_empty() {
            maximal_elements.push(v);
        }
    }
    if maximal_elements.len() != 2 {
        return None
    }
    let x = maximal_elements[0];
    let y = maximal_elements[1];
    let inter = p.lower_covers[x].inter(&p.lower_covers[y]);
    let sides = p.lower_covers[x].xor(&p.lower_covers[y]);
    if inter.is_empty() {
        return None
    }
    for v in inter.iter() {
        for u in sides.iter() {
            if p.incomparable(u, v) {
                if p.gt[x][u] {
                    return Some(NStructure {
                        top_left: x,
                        top_right: y,
                        bottom_left: u,
                        // bottom_right: v,
                    })
                } else {
                    return Some(NStructure {
                        top_left: y,
                        top_right: x,
                        bottom_left: u,
                        // bottom_right: v,
                    })
                }
            }
        }
    }
    None
}

/**
 * Tests if there are precisely two maximal elements and that, with
 * some of the elements they cover, they form an N.
 */
pub fn has_maximal_n(p: &Poset) -> bool {
    find_maximal_n(p).is_some()
}