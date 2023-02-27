use crate::graph::*;

use utilities::edge_tools::*;

fn not(colour: u8) -> u8 {
    if colour == 1 { 2 } else { 1 }
}

fn minimax_analyse_rec(g: &Graph, colours: &mut EdgeVec<u8>, next_vert: usize, other_end: &mut EdgeVec<Option<Edge>>,
        path_len: &mut EdgeVec<usize>, max_len_here: usize, best_len: usize, edges: &Vec<Edge>,
        long_path_count: usize, long_path_cap: Option<usize>) -> usize {
    let local_cols: Vec<u8> = edges.iter().map(|e| colours.get(*e)).collect();
            
    // if all three edges around next_vert all have same colour, fail.
    // Maybe should never happen??
    if local_cols[0] == local_cols[1] && local_cols[1] == local_cols[2] && local_cols[0] != 0 {
        return best_len;
    }
    
    for (i, j) in [(0, 1), (1, 2), (2, 0)] {
        if local_cols[i] != 0 && local_cols[i] == local_cols[j] {
            if other_end.get(edges[i]).map_or(false, |x| x == edges[j]) {
                // if two have same colour but already in same component, then fail.
                return best_len;
            } else {
                // if two have same colour but not in same component, then merge components.
                let i_end = other_end.get(edges[i]).unwrap();
                let j_end = other_end.get(edges[j]).unwrap();
                other_end.set(edges[i], None);
                other_end.set(edges[j], None);
                let i_len = path_len.get(edges[i]);
                let j_len = path_len.get(edges[j]);
                path_len.set(i_end, i_len + j_len);
                path_len.set(j_end, i_len + j_len);
                let new_max_len = max_len_here.max(i_len + j_len);
                other_end.set(i_end, Some(j_end));
                other_end.set(j_end, Some(i_end));
                let mut value = best_len;
                let new_long_path = if i_len + j_len >= 4 { 1 } else { 0 };
                value = value.min(minimax_set_colours_rec(g, colours, next_vert + 1, other_end, 
                    path_len, new_max_len, best_len, long_path_count + new_long_path, long_path_cap));
                // set all values back to original.
                other_end.set(edges[i], Some(i_end));
                other_end.set(edges[j], Some(j_end));
                other_end.set(i_end, Some(edges[i]));
                other_end.set(j_end, Some(edges[j]));
                path_len.set(i_end, i_len);
                path_len.set(j_end, j_len);
                return value;
            }
        }
    }

    return best_len;
}

fn minimax_set_colours_rec(g: &Graph, colours: &mut EdgeVec<u8>, next_vert: usize, other_end: &mut EdgeVec<Option<Edge>>,
        path_len: &mut EdgeVec<usize>, max_len_here: usize, best_len: usize,
        long_path_count: usize, long_path_cap: Option<usize>) -> usize {
    let n = g.n.to_usize();
    if long_path_cap.map_or(false, |cap| long_path_count > cap) {
        // There are too many long paths. Fail.
        return best_len;
    }
    if next_vert == n {
        return best_len.min(max_len_here);
    }
    if max_len_here >= best_len {
        // We've already failed, so give up.
        return best_len;
    }
    let edges: Vec<Edge> = g.adj_list[next_vert].iter().map(|x| Edge::of_pair(next_vert, *x)).collect();
    let local_cols: Vec<u8> = edges.iter().map(|e| colours.get(*e)).collect();
    let mut value = best_len;
    let possibilities = if next_vert == 0 {
        vec![[1, 1, 2], [1, 2, 1], [2, 1, 1]]
    } else {
        vec![[1, 1, 2], [1, 2, 1], [2, 1, 1], [1, 2, 2], [2, 1, 2], [2, 2, 1]]
    };
    for new_cols in possibilities {
        if local_cols[0] != not(new_cols[0]) && local_cols[1] != not(new_cols[1]) && local_cols[2] != not(new_cols[2]) {
            // This colouring is compatible. 
            for i in 0..3 {
                colours.set(edges[i], new_cols[i]);
            }
            value = value.min(minimax_analyse_rec(g, colours, next_vert, other_end, path_len, max_len_here, value, &edges, long_path_count, long_path_cap));
            // revert colours to original.
            for i in 0..3 {
                colours.set(edges[i], local_cols[i])
            }
        }
    }
    value
}

pub fn minimax_len(g: &Graph) -> u32 {
    for d in g.deg.iter() {
        if !d.equals(3) {
            panic!("Thomassen only applies to cubic graphs! (If subcubic then improve code.)");
        }
    }
    let (g, _ordering_inv) = g.order_by_nbhd();
    let n = g.n.to_usize();

    let mut colours = EdgeVec::new(&g.adj_list, 0);
    let mut other_end = EdgeVec::new_fn(&g.adj_list, |x| Some(x));
    let mut path_len = EdgeVec::new(&g.adj_list, 1);

    minimax_set_colours_rec(&g, &mut colours, 0, &mut other_end, &mut path_len, 
        1, n, 0, Some(2)) as u32
}