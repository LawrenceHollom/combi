use crate::entity::{digraph::Digraph, graph::*};

use rand::{thread_rng, Rng};
use utilities::vertex_tools::*;

use queues::*;

/**
 * n.b. the bunkbed site conjecture is FALSE!
 */

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum VertexState {
    DOWN,
    UP,
    POST,
}

impl VertexState {
    fn flip(&self) -> Self {
        use VertexState::*;
        match self {
            DOWN => UP,
            UP => DOWN,
            POST => POST,
        }
    }

    fn connects_to(&self, other: Self) -> bool {
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
    const ZERO: Self = Self { down: 0, up: 0 };

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

    fn is_bad(&self) -> bool {
        self.up > self.down
    }
}

fn process_config(
    g: &Graph,
    states: &VertexVec<VertexState>,
    counts: &mut VertexVec<Counts>,
    unreachable: VertexSet,
) {
    // Check what is reachable.
    let mut found = VertexVec::new(g.n, &false);
    found[Vertex::ZERO] = true;
    let mut q = queue![Vertex::ZERO];
    let mut finds_unreachable = false;

    'flood_fill: while let Ok(u) = q.remove() {
        for v in g.adj_list[u].iter() {
            if !found[*v] && states[u].connects_to(states[*v]) {
                if unreachable.has_vert(*v) {
                    finds_unreachable = true;
                    break 'flood_fill;
                }
                found[*v] = true;
                let _ = q.add(*v);
            }
        }
    }

    if !finds_unreachable {
        for v in g.iter_verts() {
            if found[v] {
                counts[v].add_inplace(states[v])
            }
        }
    }
}

fn get_counts_conditional(g: &Graph, unreachable: VertexSet) -> VertexVec<Counts> {
    use VertexState::*;
    // could read posts from user input?
    let posts = [
        Vertex::of_usize(1),
        Vertex::of_usize(4),
        Vertex::of_usize(7),
    ];
    let mut states = VertexVec::new(g.n, &DOWN);
    for post in posts.iter() {
        if post.less_than(g.n) {
            states[*post] = POST;
        }
    }

    let mut counts = VertexVec::new(g.n, &Counts::ZERO);

    process_config(g, &states, &mut counts, unreachable);

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
        process_config(g, &states, &mut counts, unreachable);
    }

    counts
}

pub fn print_counts(g: &Graph) {
    let counts = get_counts_conditional(g, VertexSet::new(g.n));

    println!("Counts:");
    for (v, count) in counts.iter_enum() {
        println!("{}: {:?}", v, count);
    }
}

/**
 * Returns true if it can find a vertex set s which, conditioned upon s
 * being unreachable, we find that some vertex is in fact bad.
 */
pub fn has_bad_conditioning(g: &Graph) -> bool {
    for s in g.iter_vertex_subsets() {
        if !s.has_vert(Vertex::ZERO) {
            let counts = get_counts_conditional(g, s);

            for count in counts.iter() {
                if count.is_bad() {
                    println!("Found bad count! s = {:?}", s.to_vec());
                    println!("Counts: {:?}", counts);
                    return true;
                }
            }
        }
    }
    false
}

/**
 * This returns the bunkbed signatures of the graph: assuming that
 * zero is the start point, this is the hypercube of which configs
 * lead to a particular vertex being reachable.
 * For the proof of the conjecture, there needs to be a closed,
 * symmetric space of such hypercube subsets which can be inducted
 * with.
 */
pub fn signatures(g: &Graph) -> Vec<String> {
    // we need to store, for each vtx, precisely which configs lead
    // to it being reachable
    let pow = 1_usize << (g.n.to_usize() - 1);
    let mut reachable = VertexVec::new(g.n, &vec![false; pow]);
    let mut is_post = VertexVec::new(g.n, &false);
    let mut rng = thread_rng();
    let b = Digraph::random_semiorientation(g, 0.3333);
    for v in g.iter_verts() {
        if rng.gen_bool(0.3) {
            is_post[v] = true;
        }
    }

    // make a list of relevant permutations in advance
    let mut sigma = VertexVec::new_fn(g.n, |v| v);
    let mut all_permutations = vec![sigma.clone()];
    while sigma[Vertex::ZERO] == Vertex::ZERO {
        let mut v = g.n.to_max_vertex().decr();
        while sigma[v].is_max_less_one(g.n) {
            sigma[v] = Vertex::ZERO.incr();
            v.decr_inplace();
        }
        sigma[v].incr_inplace();
        // is it a genuine permutation?
        let mut is_good_perm = true;
        let mut found = VertexVec::new(g.n, &false);
        'test_perm: for u in sigma.iter() {
            if found[*u] {
                is_good_perm = false;
                break 'test_perm;
            }
            found[*u] = true;
        }
        if is_good_perm {
            all_permutations.push(sigma.clone());
        }
    }

    // Iterate through configs. s is the set of down vertices
    for s in g.iter_vertex_subsets() {
        // Look upon my hacks, ye mighty, and despair! (0 is always down)
        if !s.has_vert(Vertex::of_usize(0)) {
            // We need to check what is reachable in this configuration.
            let mut flood = VertexVec::new(g.n, &false);
            let mut q = queue![Vertex::of_usize(0)];
            flood[Vertex::of_usize(0)] = true;
            while let Ok(v) = q.remove() {
                // flood from v.
                for u in b.out_adj_list[v].iter() {
                    if !flood[*u] && (s.has_vert(*u) == s.has_vert(v) || is_post[*u]) {
                        flood[*u] = true;
                        let _ = q.add(*u);
                    }
                }
            }
            for v in g.iter_verts() {
                // We need to permute stuff around so that v is the last vertex.
                let s_fixed = s.swap(v, g.n.to_max_vertex());
                reachable[v][s_fixed.to_usize() / 2] = flood[v];
            }
        }
    }

    // Now we have the signatures, so it is time to print.
    let mut out = vec![];
    for v in g.iter_verts().skip(1) {
        let mut best_sigma = VertexVec::new_fn(g.n, |v| v);
        let mut best_code = u128::MAX;
        // Now get the lexicographically first permutation of the middle.

        for sigma in all_permutations.iter() {
            let mut new_code: u128 = 0;
            for (s_code, b) in reachable[v].iter().enumerate() {
                if *b {
                    let s = VertexSet::of_int(s_code as u128, g.n).permute(sigma);
                    new_code += 1 << s.to_usize();
                }
            }
            if new_code < best_code {
                best_code = new_code;
                best_sigma = sigma.to_owned();
            }
        }

        let mut line = vec!['0'; reachable[v].len()];
        for (s_code, b) in reachable[v].iter().enumerate() {
            if *b {
                let s = VertexSet::of_int(s_code as u128, g.n).permute(&best_sigma);
                line[s.to_usize()] = '1';
            }
        }
        out.push(line.iter().collect());
    }
    out
}

pub fn contradicts_bb_site_conjecture(g: &Graph) -> bool {
    let counts = get_counts_conditional(g, VertexSet::new(g.n));
    let mut is_bad = false;
    'test_verts: for (v, c) in counts.iter_enum() {
        if c.is_bad() {
            is_bad = true;
            println!("Bad at {} (counts = {:?}", v, c);
            break 'test_verts;
        }
    }
    is_bad
}
