use crate::graph::*;

fn max_induced_forest_rec(g: &Graph, forest: &mut Vec<bool>, comp: &mut Vec<usize>, 
        prev_vert: usize, size: u32, max_size: u32) -> u32 {
    let n = g.n.to_usize();
    let max_vert = (size + (n as u32) - max_size - 1) as usize;
    if max_vert <= prev_vert {
        return 0;
    }
    let mut best_size = size;
    for v in (prev_vert + 1)..n.min(max_vert + 1) {
        // Try adding vertex v. Only bad if it creates a cycle.
        let mut comp_found = vec![false; n];
        let mut is_vertex_good = true;
        'test_nbrs: for w in g.adj_list[v].iter() {
            let mut x = comp[*w];
            while comp[x] != x {
                x = comp[x];
            }
            if comp_found[x] {
                is_vertex_good = false;
                break 'test_nbrs;
            } else {
                comp_found[x] = true;
            }
        }

        if is_vertex_good {
            // adjust the union-find; it doesn't make a cycle
            let mut new_comp = comp.to_owned(); // cri.
            for w in g.adj_list[v].iter() {
                if forest[*w] {
                    let mut x = new_comp[*w];
                    while new_comp[x] != x {
                        let sta = new_comp[x];
                        new_comp[x] = v;
                        x = sta;
                    }
                    new_comp[x] = v;
                }
            }
            forest[v] = true;
            let this_size = max_induced_forest_rec(g, forest, &mut new_comp, v, size + 1, max_size);
            if this_size > best_size {
                best_size = this_size;
            }
            forest[v] = false;
        }
    }
    best_size
}

pub fn max_induced_forest(g: &Graph) -> u32 {
    let mut size = 0;
    let n = g.n.to_usize();
    let mut forest = vec![false; n];
    let mut comp = vec![0; n];
    for (i, i_comp) in comp.iter_mut().enumerate() {
        *i_comp = i;
    }

    'test_start_vertex: for v in 0..n {
        if (n - v) < size as usize {
            break 'test_start_vertex;
        }
        forest[v] = true;
        let this_size = max_induced_forest_rec(g, &mut forest, &mut comp, v, 1, size);
        if this_size > size {
            size = this_size;
        }
        forest[v] = false;
    }
    size
}