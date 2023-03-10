use crate::graph::*;
use rand::{prelude::SliceRandom, thread_rng};

use super::*;
use utilities::vertex_tools::*;

pub fn new_bowties(scale: usize, degree: Degree) -> Graph {
    let d = degree.to_usize();
    let mult = ((d % 2) + 1) * (2 * d - 1);
    let n = scale * mult;
    let order = Order::of_usize(n);
    let num_bowties = scale * (d / (2 - (d % 2)));
    let mut rng = thread_rng();

    let mut adj_list: VertexVec<Vec<Vertex>> = VertexVec::new(order, &vec![]);
    
    for i in 0..num_bowties {
        adj_list[Vertex::of_usize(2 * i)].push(Vertex::of_usize(2 * i + 1));
        adj_list[Vertex::of_usize(2 * i + 1)].push(Vertex::of_usize(2 * i));
    }

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
        let u = Vertex::of_usize(*v / (d - 1));
        let vert = Vertex::of_usize((i / d) + 2 * num_bowties);
        adj_list[u].push(vert);
        adj_list[vert].push(u);
    }

    Graph::of_adj_list(adj_list, Constructor::Random(RandomConstructor::Bowties(scale, degree)))
}