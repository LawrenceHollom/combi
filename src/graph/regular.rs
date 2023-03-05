use utilities::*;
use crate::graph::*;
use rand::prelude::SliceRandom;

pub fn new_from_degree_sequence(deg_seq: &Vec<Degree>, is_regular: bool) -> Graph {
    let n = deg_seq.len();
    let mut sum = 0;
    for d in deg_seq.iter() {
        if d.at_least(n) {
            panic!("Order must be larger than max degree!");
        }
        sum += d.to_usize();
    }
    if sum % 2 == 1 {
        panic!("Sum of degrees must be even");
    }
    let mut rng = thread_rng();
    let mut pairings: Vec<usize> = vec![];
    for (i, d) in deg_seq.iter().enumerate() {
        for _j in 0..d.to_usize() {
            pairings.push(i);
        }
    }
    let mut adj_list: VertexVec<Vec<Vertex>>;

    'find_good_shuffle: loop {
        adj_list = VertexVec::new(Order::of_usize(n), &vec![]);
        pairings.shuffle(&mut rng);
        let mut is_good = true;
        'test_shuffle: for i in 0..(sum / 2) {
            let u = Vertex::of_usize(pairings[i]);
            let v = Vertex::of_usize(pairings[i + (sum / 2)]);
            if u == v || adj_list[u].contains(&v) {
                is_good = false;
                break 'test_shuffle;
            }
            adj_list[u].push(v);
            adj_list[v].push(u);
        }
        if is_good {
            break 'find_good_shuffle;
        }
    }

    let constructor = if is_regular {
        Random(RandomConstructor::Regular(Order::of_usize(n), deg_seq[0]))
    } else {
        Random(RandomConstructor::DegreeSequence(deg_seq.to_owned()))
    };
    Graph::of_adj_list(adj_list, constructor)
}

pub fn new_regular(order: &Order, degree: &Degree) -> Graph {
    let n = order.to_usize();
    let deg_seq = vec![*degree; n];
    self::new_from_degree_sequence(&deg_seq, true)
}