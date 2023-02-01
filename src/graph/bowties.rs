use crate::graph::*;
use rand::prelude::SliceRandom;

pub fn new_bowties(scale: usize) -> Graph {
    let n = scale * 10;
    let num_bowties = scale * 3;
    let mut rng = thread_rng();

    let mut vert_pars = vec![vec![0; num_bowties]; 4];
    for i in 0..4 {
        vert_pars[i] = (0..num_bowties).collect();
        vert_pars[i].shuffle(&mut rng);
    }
    let mut vert = 2 * num_bowties;
    let mut count = 0;
    let mut adj_list: Vec<Vec<usize>> = vec![vec![]; n];
    
    for i in 0..num_bowties {
        adj_list[2 * i].push(2 * i + 1);
        adj_list[2 * i + 1].push(2 * i);
    }

    for part in 0..4 {
        for i in 0..num_bowties {
            let bowtie = vert_pars[part][i];
            let u = 2 * bowtie + (part % 2);
            adj_list[vert].push(u);
            adj_list[u].push(vert);
            count += 1;
            if count % 3 == 0 {
                vert += 1;
            }
        }
    }

    Graph::of_adj_list(adj_list, Random(RandomConstructor::Bowties(scale)))
}