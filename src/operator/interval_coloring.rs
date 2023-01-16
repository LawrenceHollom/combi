use crate::graph::*;

use queues::*;
use std::collections::HashSet;

fn code_edge(u: usize, v: usize) -> usize {
    if u > v {
        code_edge(v, u)
    } else { // u < v
        ((v * (v - 1)) / 2) + u
    }
}

fn decode_edge(e: usize) -> (usize, usize) {
    let f = e as f64;
    let v = (0.5 * (1.0 + (8.0 * f + 1.0).sqrt())).floor() as usize;
    let u = e - ((v * (v - 1)) / 2);
    (u, v)
}

fn factorial(x: usize) -> usize {
    if x <= 1 {
        1
    } else {
        x * factorial(x - 1)
    }
}

fn find_interval_coloring_rec(g: &Graph, vert_ordering: &Vec<usize>, colored: &mut Vec<bool>, 
        colors: &mut Vec<i32>, num_colored: usize) -> Option<Vec<i32>> {
    let n = g.n.to_usize();
    let extreme_col = (n * n) as i32;
    if num_colored == n {
        return Some(colors.to_owned());
    } else {
        let node = vert_ordering[num_colored];
        colored[node] = true;
        let deg = g.deg[node].to_usize() as i32;
        let mut min_col = extreme_col;
        let mut max_col = -extreme_col;
        let mut used_colors = HashSet::new();
        let mut edges_to_color = vec![];
        let mut is_proper = true;
        'check_cols: for v in g.adj_list[node].iter() {
            let e = code_edge(node, *v);
            if colors[e] < extreme_col {
                if used_colors.contains(&colors[e]) {
                    is_proper = false;
                    break 'check_cols;
                }
                used_colors.insert(colors[e]);
                if colors[e] < min_col {
                    min_col = colors[e];
                }
                if colors[e] > max_col {
                    max_col = colors[e];
                }
            } else {
                edges_to_color.push(e);
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
        if min_col == extreme_col && max_col == -extreme_col {
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
                        'find_elt: for j in 0..d {
                            if !picked[j] {
                                if count == next {
                                    perm[i] = j;
                                    picked[j] = true;
                                    break 'find_elt;
                                }
                                count += 1;
                            }
                        }
                    }

                    // Maybe we now have a permutation of length equal to the num of edges to color?
                    for i in 0..d {
                        colors[edges_to_color[i]] = available_colors[perm[i]];
                    }
                    let coloring = find_interval_coloring_rec(g, vert_ordering, colored, colors, num_colored + 1);
                    if coloring.is_some() {
                        return coloring;
                    }
                    for i in 0..d {
                        colors[edges_to_color[i]] = extreme_col;
                    }
                    perm_code[d-1] += 1;
                }
            }
        }
        None
    }
}

fn get_vert_ordering(g: &Graph) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut bfs = queue![];
    let mut visited = vec![false; n];
    let mut vert_ordering = vec![];

    for i in 0..n {
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
    let n = g.n.to_usize();
    let vert_ordering = get_vert_ordering(g);
    println!("Vert ordering: {:?}", vert_ordering);

    let extreme_col = (n * n) as i32;
    let m = code_edge(n-1, n);
    let mut colors = vec![extreme_col; m];
    match find_interval_coloring_rec(g, &vert_ordering, &mut vec![false; n], &mut colors, 0) {
        Some(coloring) => {
            let mut min_col = extreme_col;
            let mut max_col = -extreme_col;
            for col in coloring.iter() {
                if *col < min_col {
                    min_col = *col
                }
                if *col > max_col && *col != extreme_col {
                    max_col = *col;
                }
            }
            for (e, color) in coloring.iter().enumerate() {
                if *color != extreme_col {
                    let (u, v) = decode_edge(e);
                    println!("{} ~ {} : {}", u, v, *color - min_col);
                }
            }
            println!("Number of colours: {}", (max_col - min_col + 1));
        }
        None => println!("No interval coloring found")
    }
}

pub fn num_interval_colors(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let vert_ordering = get_vert_ordering(g);

    let extreme_col = (n * n) as i32;
    let m = code_edge(n-1, n);
    let mut colors = vec![extreme_col; m];
    match find_interval_coloring_rec(g, &vert_ordering, &mut vec![false; n], &mut colors, 0) {
        Some(coloring) => {
            let mut min_color = extreme_col;
            let mut max_color = -extreme_col;
            for color in coloring.iter() {
                if *color != extreme_col {
                    if *color > max_color {
                        max_color = *color;
                    }
                    if *color < min_color {
                        min_color = *color;
                    }
                }
            }
            (max_color - min_color + 1) as u32
        }
        None => 0
    }
}

pub fn has_interval_coloring(g: &Graph) -> bool {
    // Run a bfs to get the ordering of the vertices
    let n = g.n.to_usize();
    let vert_ordering = get_vert_ordering(g);

    let extreme_col = (n * n) as i32;
    let m = code_edge(n-1, n);
    let mut colors = vec![extreme_col; m];
    find_interval_coloring_rec(g, &vert_ordering, &mut vec![false; n], &mut colors, 0).is_some()
}