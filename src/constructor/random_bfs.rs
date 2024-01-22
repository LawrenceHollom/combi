use rand::{thread_rng, Rng};
use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_bfs(n: Order, width: usize, density: f64) -> Graph {
    let order = n.to_usize();
    let mut rng = thread_rng();
    let mut adj = vec![vec![false; order]; order];
    for v in 0..order {
        let mut num_edges_added = 0;
        let mut num_attempts = 0;
        'add_edges: while num_edges_added < (width as f64 * density) as usize {
            num_attempts += 1;
            let u = if rng.gen_bool(0.5) {
                let diff = rng.gen_range(1..=width);
                if diff <= v {
                    v - diff
                } else {
                    v
                }
            } else {
                v + rng.gen_range(1..=width)
            };
            if u != v && n.more_than(u) && !adj[u][v] {
                adj[u][v] = true;
                adj[v][u] = true;
                num_edges_added += 1;
                num_attempts = 0;
            }
            if num_attempts >= 100 {
                break 'add_edges;
            }
        }
    }

    let mut adj_list = VertexVec::new(n, &vec![]);

    for i in 0..order {
        for j in 0..order {
            if adj[i][j] {
                adj_list[Vertex::of_usize(i)].push(Vertex::of_usize(j));
            }
        }
    }

    Graph::of_adj_list(adj_list, Constructor::Random(crate::constructor::RandomConstructor::BFSOptimal(n, width, density)))
}