extern crate queues;

use crate::graph::*;

use queues::*;

fn components(g: &Graph) -> Vec<usize> {
    let n = g.n.to_usize();
    let mut comp: Vec<usize> = vec![n; n];
    let mut q: Queue<usize> = queue![];

    for i in 0..n {
        if comp[i] == n {
            comp[i] = i;
            let _ = q.add(i);
            'flood_fill: loop {
                match q.remove() {
                    Ok(node) => {
                        for j in g.adj_list[node].iter() {
                            if comp[*j] == n {
                                comp[*j] = i;
                                let _ = q.add(*j);
                            }
                        }
                    },
                    Err(_err) => break 'flood_fill,
                }
            }
        }
    }

    comp
}

pub fn largest_component(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let comps = components(g);

    let mut sizes: Vec<u32> = vec![0; n];

    for comp in comps.iter() {
        sizes[*comp] += 1;
    }

    let mut max = 0;
    for size in sizes.iter() {
        if *size > max {
            max = *size;
        }
    }

    max
}

pub fn is_connected(g: &Graph) -> bool {
    largest_component(g) == g.n.to_usize() as u32
}

pub fn num_components(g: &Graph) -> u32 {
    let n = g.n.to_usize();
    let comps = components(g);

    let mut is_comp: Vec<bool> = vec![false; n];
    let mut num_comps: u32 = 0;

    for comp in comps.iter() {
        if !is_comp[*comp] {
            num_comps += 1;
        }
        is_comp[*comp] = true;
    }

    num_comps
}