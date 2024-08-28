use std::{ops::{Index, IndexMut}, u32};

use utilities::{edge_tools::*, vertex_tools::*};

use queues::*;

use crate::entity::graph::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Colour {
    Red,
    Blue,
}

impl Colour {
    fn of_bool(x: bool) -> Colour {
        use Colour::*;
        if x {
            Red
        } else {
            Blue
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct ColourPair {
    r: u32,
    b: u32,
}

impl ColourPair {
    fn new() -> ColourPair {
        ColourPair {
            r: u32::MAX,
            b: u32::MAX,
        }
    }
}

impl Index<Colour> for ColourPair {
    type Output = u32;

    fn index(&self, col: Colour) -> &Self::Output {
        match col {
            Colour::Red => &self.r,
            Colour::Blue => &self.b,
        }
    }
}

impl IndexMut<Colour> for ColourPair {
    fn index_mut(&mut self, col: Colour) -> &mut Self::Output {
        match col {
            Colour::Red => &mut self.r,
            Colour::Blue => &mut self.b,
        }
    }
}

/**
 * *chef's kiss*
 */
fn cube_antipode(g: &Graph, v: Vertex) -> Vertex {
    v.xor(g.n.to_max_vertex())
}

/**
 * Run a flood-fill to find the minimal number of flips to the antipode.
 * Can use efficient algorithms because all "distances" are 0 or 1.
 */
fn distance_to_antipode(g: &Graph, indexer: &EdgeIndexer, v: Vertex, reds: EdgeSet) -> u32 {
    use Colour::*;
    let target = self::cube_antipode(g, v);
    let mut q = queue![(v, Blue), (v, Red)];
    let mut next_q = queue![];
    let mut dist = VertexVec::new(g.n, &ColourPair::new());
    loop {
        while let Ok((u, col)) = q.remove() {
            let u_dist = dist[u][col];
            if u == target {
                return u_dist
            }
            for w in g.adj_list[u].iter() {
                let e = Edge::of_pair(u, *w);
                let e_col = Colour::of_bool(reds.has_edge(e, indexer));
                let dist_step = if e_col == col { 0 } else { 1 };
                let found_dist = u_dist + dist_step;
                if found_dist < dist[*w][e_col] {
                    // This is a new best distance, so should be recorded.
                    dist[*w][e_col] = found_dist;
                    if dist_step == 0 {
                        // Still in the same component, so put it in q.
                        if *w == target {
                            // We can guarantee that this is an optimal path, so can return early.
                            return found_dist;
                        }
                        let _ = q.add((*w, e_col));
                    } else {
                        // We don't want to see this until we're ready for it.
                        let _ = next_q.add((*w, e_col));
                    }
                }
            }
        }
        q = next_q;
        next_q = queue![];
    }
}

/**
 * Must be given a hypercube as input.
 * We then iterate over edge colourings, and decide whether they are good or bad.
 * The vertices are adjacent by binary digits (excellent!)
 */
pub fn partition_colourings(g: &Graph) {
    let indexer = EdgeIndexer::new(&g.adj_list);
    for reds in g.iter_edge_sets() {
        let mut total_antipode_distance = 0;
        for v in g.n.div(2).iter_verts() {
            total_antipode_distance += distance_to_antipode(g, &indexer, v, reds)
        }
        println!("Total antipode distance = {}", total_antipode_distance);
    }
}