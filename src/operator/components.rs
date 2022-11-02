extern crate queues;

use crate::graph::*;

use queues::*;

pub fn largest_component(g: &Graph) -> u32 {
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

    let mut sizes: Vec<u32> = vec![0; n];

    for i in 0..n {
        sizes[comp[i]] += 1;
    }

    let mut max = 0;
    for i in 0..n {
        if sizes[i] > max {
            max = sizes[i];
        }
    }

    max
}