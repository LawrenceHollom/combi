use crate::graph::*;
use rand::prelude::SliceRandom;

pub fn new_bowties(scale: usize, degree: Degree) -> Graph {
    let d = degree.to_usize();
    let mult = ((d % 2) + 1) * (2 * d - 1);
    let n = scale * mult;
    let num_bowties = scale * (d / (2 - (d % 2)));
    let mut rng = thread_rng();

    let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];
    
    for i in 0..num_bowties {
        adj_list[2 * i].push(2 * i + 1);
        adj_list[2 * i + 1].push(2 * i);
    }

    /*
    let threads = 2 * (d - 1);
    let mut vert = 2 * num_bowties;
    let mut count = 0;
    let mut vert_pars = vec![vec![0; num_bowties]; threads];
    for i in 0..threads {
        vert_pars[i] = (0..num_bowties).collect();
        vert_pars[i].shuffle(&mut rng);
    }

    for part in 0..threads {
        for i in 0..num_bowties {
            let bowtie = vert_pars[part][i];
            let u = 2 * bowtie + (part % 2);
            adj_list[vert].push(u);
            adj_list[u].push(vert);
            count += 1;
            if count % d == 0 {
                vert += 1;
            }
        }
    }*/

    let num_ends = num_bowties * 2 * (d - 1);
    let mut ordering: Vec<usize> = (0..num_ends).collect();
    'find_good_shuffle: loop {
        ordering.shuffle(&mut rng);
        let mut is_good = true;
        let mut last_part = vec![num_ends; num_ends];
        'test_goodness: for (i, v) in ordering.iter().enumerate() {
            let part = i / (d - 1);
            let vert = *v / d;
            if last_part[vert] == part {
                is_good = false;
                break 'test_goodness;
            }
            last_part[vert] = part;
        }
        if is_good {
            break 'find_good_shuffle;
        }
    }

    for (i, v) in ordering.iter().enumerate() {
        let u = *v / (d - 1);
        let vert = (i / d) + 2 * num_bowties;
        adj_list[u].push(vert);
        adj_list[vert].push(u);
    }

    Graph::of_adj_list(adj_list, Random(RandomConstructor::Bowties(scale, degree)))
}