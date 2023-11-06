use crate::graph::*;

use utilities::vertex_tools::*;

use queues::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum VertexState {
    DOWN,
    UP,
    POST,
}

impl VertexState {
    fn flip(&self) -> VertexState {
        use VertexState::*;
        match self {
            DOWN => UP,
            UP => DOWN,
            POST => POST,
        }
    }

    fn connects_to(&self, other: VertexState) -> bool {
        use VertexState::*;
        match self {
            DOWN => other != UP,
            UP => other != DOWN,
            POST => true,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct Counts {
    down: usize,
    up: usize,
}

impl Counts {
    const ZERO: Counts = Counts { down: 0, up: 0 };

    fn add_inplace(&mut self, state: VertexState) {
        use VertexState::*;
        match state {
            DOWN => self.down += 1,
            UP => self.up += 1,
            POST => {
                self.down += 1; 
                self.up += 1
            }
        }
    }
}

fn process_config(g: &Graph, states: &VertexVec<VertexState>, counts: &mut VertexVec<Counts>) {
    // Check what is reachable.
    let mut found = VertexVec::new(g.n, &false);
    found[Vertex::ZERO] = true;
    let mut q = queue![Vertex::ZERO];
    while let Ok(u) = q.remove() {
        for v in g.adj_list[u].iter() {
            if !found[*v] && states[u].connects_to(states[*v]) {
                found[*v] = true;
                let _ = q.add(*v);
            }
        }
    }

    for v in g.iter_verts() {
        if found[v] {
            counts[v].add_inplace(states[v])
        }
    }
}

pub fn print_counts(g: &Graph) {
    use VertexState::*;
    // could read posts from user input?
    let posts = vec![Vertex::of_usize(1), Vertex::of_usize(4)];
    let mut states = VertexVec::new(g.n, &DOWN);
    for post in posts.iter() {
        states[*post] = POST;
    }

    let mut counts = VertexVec::new(g.n, &Counts::ZERO);

    process_config(g, &states, &mut counts);

    'iter_configs: loop {
        let mut v = Vertex::of_usize(1);
        while v.less_than(g.n) && (states[v] == UP || states[v] == POST) {
            states[v] = states[v].flip();
            v.incr_inplace()
        }
        if v.is_n(g.n) {
            break 'iter_configs;
        } else {
            states[v] = states[v].flip();
        }
        process_config(g, &states, &mut counts);
    }

    println!("Counts:");
    for (v, count) in counts.iter_enum() {
        println!("{}: {:?}", v, count);

    }
}