use rand::{thread_rng, Rng, rngs::ThreadRng};
use utilities::*;

use super::*;
use utilities::vertex_tools::*;

pub fn new_semiregular(n: Order, avg_deg: f64, exponent: f64) -> Graph {
    let mut rng = thread_rng();
    let mut p = avg_deg;
    if p < 1.0 {
        println!("p is supposed to be average deg, but you've clearly entered a prob so we'll interpret it as such.");
        p *= (n.to_usize() - 1) as f64;
    }
    let target_edges = (p * (n.to_usize() as f64) / 2.0).floor() as usize;

    let mut adj = VertexVec::new(n, &VertexVec::new(n, &false));
    let mut degs = VertexVec::new(n, &Degree::ZERO);

    // Add a spanning tree.
    for (i, v) in n.iter_verts().enumerate().skip(1) {
        let u = Vertex::of_usize(rng.gen_range(0..i));
        adj[u][v] = true;
        adj[v][u] = true;
        degs[u].incr_inplace();
        degs[v].incr_inplace();
    }
    let mut placed_edges = n.to_usize() - 1;

    fn weight(d: Degree, exponent: f64) -> f64 {
        1.0 / ((d.to_usize() as f64).powf(exponent))
    }

    let mut total_weight = 0.0;
    for v in n.iter_verts() {
        total_weight += weight(degs[v], exponent);
    }

    fn sample(rng: &mut ThreadRng, total_weight: f64, degs: &VertexVec<Degree>, exponent: f64) -> Vertex {
        let sample = rng.gen_range(0.0..total_weight);
        let mut cum = 0.0;
        let mut v = Vertex::ZERO;
        while cum <= sample {
            cum += weight(degs[v], exponent);
            v.incr_inplace();
        }
        v.decr()
    }

    while placed_edges < target_edges {
        // Sample two vertices, weighted by 1/degree.
        let u = sample(&mut rng, total_weight, &degs, exponent);
        let v = sample(&mut rng, total_weight, &degs, exponent);
        if u != v && !adj[u][v] {
            // We have a pair! Link them up!
            adj[u][v] = true;
            adj[v][u] = true;
            total_weight -= weight(degs[u], exponent);
            total_weight -= weight(degs[v], exponent);
            degs[u].incr_inplace();
            degs[v].incr_inplace();
            total_weight += weight(degs[u], exponent);
            total_weight += weight(degs[v], exponent);
            placed_edges += 1;
        }
    }

    Graph::of_matrix(adj, Constructor::Random(RandomConstructor::ConnectedSemiregular(n, p, exponent)))
}