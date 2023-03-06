use crate::graph::*;

use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use queues::*;
use std::collections::HashSet;

fn factorial(x: usize) -> usize {
    if x <= 1 {
        1
    } else {
        x * factorial(x - 1)
    }
}

fn find_interval_coloring_rec(g: &Graph, vert_ordering: &Vec<Vertex>, colored: &mut VertexVec<bool>, 
        colors: &mut EdgeVec<Option<i32>>, num_colored: usize) -> Option<EdgeVec<Option<i32>>> {
    if num_colored == g.n.to_usize() {
        Some(colors.to_owned())
    } else {
        let node = vert_ordering[num_colored];
        colored[node] = true;
        let deg = g.deg[node].to_usize() as i32;
        let mut min_col = i32::MAX;
        let mut max_col = i32::MIN;
        let mut used_colors = HashSet::new();
        let mut edges_to_color = vec![];
        let mut is_proper = true;
        'check_cols: for v in g.adj_list[node].iter() {
            let e = Edge::of_pair(*v, node);
            match colors[e] {
                Some(col) => {
                    if used_colors.contains(&col) {
                        is_proper = false;
                        break 'check_cols;
                    }
                    used_colors.insert(col);
                    if col < min_col {
                        min_col = col;
                    }
                    if col > max_col {
                        max_col = col;
                    }
                }
                None => edges_to_color.push(e),
            }
        }
        if !is_proper {
            // The coloring is already improper, which is bad.
            return None;
        }
        if max_col - min_col >= deg {
            // The interval is too wide; we cannot colour it.
            return None;
        }
        if min_col == i32::MAX && max_col == i32::MIN {
            // There are no edges. Force the start to be 0
            max_col = deg - 1;
            min_col = 0;
        }
        for start in (max_col - deg + 1)..(min_col + 1) {
            // start is the start of the interval of colors we are to use.
            // Iterate over colorings of the remaining edges. For now we do the naive method
            let mut available_colors = vec![];
            for col in start..(start+deg) {
                if !used_colors.contains(&col) {
                    available_colors.push(col);
                }
            }
            let d = edges_to_color.len();
            if d == 0 {
                let coloring = find_interval_coloring_rec(g, vert_ordering, colored, colors, num_colored + 1);
                if coloring.is_some() {
                    return coloring;
                }
            } else {
                let mut perm_code = vec![0; d];
                for _perm_num in 0..factorial(d) {
                    let mut index = d-1;
                    while perm_code[index] > index {
                        perm_code[index] = 0;
                        perm_code[index - 1] += 1;
                        index -= 1;
                    }

                    // Now generate the actual permutation from the perm code
                    let mut picked = vec![false; d];
                    let mut perm = vec![0; d];
                    for i in (0..d).rev() {
                        let next = perm_code[i];
                        let mut count = 0;
                        'find_elt: for (j, is_picked) in picked.iter_mut().enumerate() {
                            if !*is_picked {
                                if count == next {
                                    perm[i] = j;
                                    *is_picked = true;
                                    break 'find_elt;
                                }
                                count += 1;
                            }
                        }
                    }

                    // Maybe we now have a permutation of length equal to the num of edges to color?
                    for i in 0..d {
                        colors[edges_to_color[i]] = Some(available_colors[perm[i]]);
                    }
                    let coloring = find_interval_coloring_rec(g, vert_ordering, colored, colors, num_colored + 1);
                    if coloring.is_some() {
                        return coloring;
                    }
                    for i in 0..d {
                        colors[edges_to_color[i]] = None;
                    }
                    perm_code[d-1] += 1;
                }
            }
        }
        None
    }
}

fn get_min_and_max_col(coloring: EdgeVec<Option<i32>>) -> (i32, i32) {
    let mut min_col = i32::MAX;
    let mut max_col = i32::MIN;
    for col in coloring.iter() {
        match *col {
            Some(col) => {
                if col < min_col {
                    min_col = col
                }
                if col > max_col {
                    max_col = col;
                }
            }
            None => (),
        }
    }
    (min_col, max_col)
}

fn get_vert_ordering(g: &Graph) -> Vec<Vertex> {
    let mut bfs = queue![];
    let mut visited = VertexVec::new(g.n, &false);
    let mut vert_ordering = vec![];

    for i in g.n.iter_verts() {
        if !visited[i] {
            let _ = bfs.add(i);
            visited[i] = true;
            while bfs.size() > 0 {
                match bfs.remove() {
                    Ok(node) => {
                        vert_ordering.push(node);
                        for v in g.adj_list[node].iter() {
                            if !visited[*v] {
                                let _ = bfs.add(*v);
                                visited[*v] = true;
                            }
                        }
                    }
                    Err(_e) => (),
                }
            }
        }
    }
    vert_ordering
}

pub fn print_interval_coloring(g: &Graph) {
    let vert_ordering = get_vert_ordering(g);
    println!("Vert ordering: {:?}", vert_ordering);

    let mut colors = EdgeVec::new(&g.adj_list, None);
    match find_interval_coloring_rec(g, &vert_ordering, &mut VertexVec::new(g.n, &false), &mut colors, 0) {
        Some(coloring) => {
            let (min_col, max_col) = get_min_and_max_col(coloring);
            for (e, color) in coloring.iter_enum() {
                match *color {
                    Some(col) => println!("{} : {}", e, col - min_col),
                    None => (),
                }
            }
            println!("Number of colours: {}", (max_col - min_col + 1));
        }
        None => println!("No interval coloring found")
    }
}

pub fn num_interval_colors(g: &Graph) -> u32 {
    let vert_ordering = get_vert_ordering(g);

    let mut colors = EdgeVec::new(&g.adj_list, None);
    match find_interval_coloring_rec(g, &vert_ordering, &mut VertexVec::new(g.n, &false), &mut colors, 0) {
        Some(coloring) => {
            let (min_col, max_col) = get_min_and_max_col(coloring);
            (max_col - min_col + 1) as u32
        }
        None => 0
    }
}

pub fn has_interval_coloring(g: &Graph) -> bool {
    // Run a bfs to get the ordering of the vertices
    let vert_ordering = get_vert_ordering(g);

    let extreme_col = (g.n.to_usize() * g.n.to_usize()) as i32;
    let mut colors = EdgeVec::new(&g.adj_list, None);
    find_interval_coloring_rec(g, &vert_ordering, &mut VertexVec::new(g.n, &false), &mut colors, 0).is_some()
}