use crate::graph::*;

use utilities::edge_tools::*;
use utilities::vertex_tools::*;

fn not(colour: u8) -> u8 {
    if colour == 1 { 2 } else { 1 }
}

fn opn_min(a: Option<usize>, b: Option<usize>) -> Option<usize> {
    match (a, b) {
        (Some(x), Some(y)) => Some(x.min(y)),
        (Some(x), None) | (None, Some(x)) => Some(x),
        (None, None) => None,
    }
}

fn minimax_forest_analyse_rec(g: &Graph, colours: &mut EdgeVec<u8>, next_vert: Vertex, other_end: &mut EdgeVec<Option<Edge>>,
        path_len: &mut EdgeVec<usize>, max_len_here: usize, best_len: Option<usize>, edges: &Vec<Edge>,
        part_two_max_len: Option<usize>, long_path_count: usize, long_path_cap: Option<usize>) -> Option<usize> {
    let local_cols: Vec<u8> = edges.iter().map(|e| colours.get(*e)).collect();
    
    let deg = g.deg[next_vert];

    // if all three edges around next_vert all have same colour, fail.
    // Maybe should never happen??
    if deg.equals(3) && local_cols[0] == local_cols[1] && local_cols[1] == local_cols[2] && local_cols[0] != 0 {
        return best_len;
    }

    let mut i = 0;
    let mut j = 0;
    
    'find_i_and_j: for (k, l) in [(0, 1), (1, 2), (2, 0)] {
        if deg.at_most(k) || deg.at_most(l) {
            // Degree is too small, so skip this case.
            continue;
        } else if local_cols[k] != 0 && (deg.at_most(2) || local_cols[k] == local_cols[l]) {
            i = k;
            j = l;
            break 'find_i_and_j;
        }
    }

    let mut value = best_len;

    if i != j && local_cols[i] == local_cols[j] {
        if other_end.get(edges[i]).map_or(false, |x| x == edges[j]) {
            // if two have same colour but already in same component, then fail.
            return best_len;
        }
        // if two have same colour but not in same component, then merge components.
        let i_len = path_len.get(edges[i]);
        let j_len = path_len.get(edges[j]);
        if part_two_max_len.map_or(false, |x| i_len + j_len > x) && local_cols[i] == 2 {
            // The colouring is bad as we have a part-two comp that is too long.
            return best_len;
        }
        
        let i_end = other_end.get(edges[i]).unwrap();
        let j_end = other_end.get(edges[j]).unwrap();
        other_end.set(edges[i], None);
        other_end.set(edges[j], None);

        path_len.set(i_end, i_len + j_len);
        path_len.set(j_end, i_len + j_len);
        other_end.set(i_end, Some(j_end));
        other_end.set(j_end, Some(i_end));

        let new_max_len = max_len_here.max(i_len + j_len);
        let new_long_path = if i_len + j_len >= 4 { 1 } else { 0 };
        
        value = opn_min(value, minimax_forest_set_colours_rec(g, colours, next_vert.incr(), other_end, 
            path_len, new_max_len, best_len, part_two_max_len,
            long_path_count + new_long_path, long_path_cap));

        // set all values back to original.
        other_end.set(edges[i], Some(i_end));
        other_end.set(edges[j], Some(j_end));
        other_end.set(i_end, Some(edges[i]));
        other_end.set(j_end, Some(edges[j]));
        path_len.set(i_end, i_len);
        path_len.set(j_end, j_len);
    } else {
        // Don't need to change or test anything. Just recurse.
        value = opn_min(value, minimax_forest_set_colours_rec(g, colours, next_vert.incr(), other_end, 
            path_len, max_len_here, best_len, part_two_max_len, long_path_count, long_path_cap));
    }
    return value;
}

fn minimax_forest_set_colours_rec(g: &Graph, colours: &mut EdgeVec<u8>, next_vert: Vertex, other_end: &mut EdgeVec<Option<Edge>>,
        path_len: &mut EdgeVec<usize>, max_len_here: usize, best_len: Option<usize>,
        part_two_max_len: Option<usize>, long_path_count: usize, long_path_cap: Option<usize>) -> Option<usize> {
    if long_path_cap.map_or(false, |cap| long_path_count > cap) {
        // There are too many long paths. Fail.
        return best_len;
    }
    if next_vert.is_n(g.n) {
        return Some(best_len.map_or(max_len_here, |x| x.min(max_len_here)));
    }
    if best_len.map_or(false, |x| max_len_here >= x) {
        // We've already failed, so give up.
        return best_len;
    }
    let edges: Vec<Edge> = g.adj_list[next_vert].iter().map(|x| Edge::of_pair(next_vert, *x)).collect();
    let local_cols: Vec<u8> = edges.iter().map(|e| colours.get(*e)).collect();
    let mut value = best_len;
    // ISSUE: why does the part_two_max_len clause speed things up so much??
    let possibilities = if next_vert.is_zero() || part_two_max_len == Some(1) {
        vec![[1, 1, 2], [1, 2, 1], [2, 1, 1]]
    } else {
        vec![[1, 1, 2], [1, 2, 1], [2, 1, 1], [1, 2, 2], [2, 1, 2], [2, 2, 1]]
    };
    for new_cols in possibilities {
        let mut new_cols_good = true;
        'test_new_cols: for (i, local_col) in local_cols.iter().enumerate() {
            if *local_col == not(new_cols[i]) {
                new_cols_good = false;
                break 'test_new_cols;
            }
        }
        if new_cols_good {
            // This colouring is compatible. 
            for (i, e) in edges.iter().enumerate() {
                colours.set(*e, new_cols[i]);
            }
            value = opn_min(value, minimax_forest_analyse_rec(g, colours, next_vert, other_end, path_len, max_len_here, value, &edges, 
                part_two_max_len, long_path_count, long_path_cap));
            // revert colours to original.
            for (i, e) in edges.iter().enumerate() {
                colours.set(*e, local_cols[i]);
            }
        }
    }
    value
}

fn minimax_forest_len(g: &Graph, part_two_max_len: Option<usize>, long_path_cap: Option<usize>) -> Option<usize> {
    for d in g.deg.iter() {
        if !d.at_most(3) {
            panic!("This only applies to subcubic graphs! (If subcubic then improve code.)");
        }
    }
    let (g, _ordering_inv) = g.order_by_nbhd();

    let mut colours = EdgeVec::new(&g.adj_list, 0);
    let mut other_end = EdgeVec::new_fn(&g.adj_list, |x| Some(x));
    let mut path_len = EdgeVec::new(&g.adj_list, 1);

    minimax_forest_set_colours_rec(&g, &mut colours, Vertex::ZERO, &mut other_end, &mut path_len, 
        1, None, part_two_max_len, 0, long_path_cap)
}

pub fn thomassen_check(g: &Graph, long_path_cap: Option<usize>) -> u32 {
    // Safe to unwrap as there are no caps
    minimax_forest_len(g, None, long_path_cap).unwrap() as u32
}

pub fn edge_partition_forest_and_matching(g: &Graph) -> bool {
    minimax_forest_len(g, Some(1), None).is_some()
}

pub fn count_bipartite_edge_bisections(g: &Graph) -> u32 {
    let mut num_good = 0;
    let indexer = EdgeSetIndexer::new(&g.adj_list);
    'test_edgeset: for blue_edges in g.iter_edge_sets() {
        // Check if this is bipartite and strictly subcubic.
        for v in g.n.iter_verts() {
            if g.deg[v].at_least(4) {
                panic!("Graph must be subcubic!")
            }
            let mut num_red = 0;
            let mut num_blue = 0;
            for u in g.adj_list[v].iter() {
                if blue_edges.has_edge(Edge::of_pair(v, *u), &indexer) {
                    num_blue += 1;
                } else {
                    num_red += 1;
                }
            }
            if num_red > 2 || num_blue > 2 {
                continue 'test_edgeset;
            }
        }

        // Blue-edge vertex-2-colourable
        if !g.flood_fill_two_colourable(&blue_edges, &indexer) {
            continue 'test_edgeset;
        }

        // Red-edge vertex-2-colourable
        if !g.flood_fill_two_colourable(&blue_edges.inverse(&indexer), &indexer) {
            continue 'test_edgeset;
        }

        num_good += 1;
    }
    num_good
}