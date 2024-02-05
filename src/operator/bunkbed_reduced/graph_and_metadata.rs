use crate::graph::*;

use super::super::connectedness;

use rand::{rngs::ThreadRng, Rng};

use utilities::{*, vertex_tools::*, edge_tools::{EdgeIndexer, EdgeSetIterator}};

/**
 * g is guaranteed to be spinal. We return the set of all non-spinal vertices.
 */
fn get_spinal_posts(g: &Graph) -> VertexSet {
	let mut posts = VertexSet::new(g.n);
	let mut spinal_end = Vertex::ZERO;
	while g.adj[spinal_end][spinal_end.incr()] {
		spinal_end.incr_inplace();
	}
	let mut post = spinal_end.incr();
	while !post.is_n(g.n) {
		posts.add_vert(post);
		post.incr_inplace();
	}
	posts
}

fn get_spinal_vertices(g: &Graph, k: usize) -> VertexSet {
	let mut vs = VertexSet::of_vert(g.n, Vertex::ZERO);
	let mut spine_end = Vertex::ZERO;
	while g.adj[spine_end][spine_end.incr()] {
		spine_end.incr_inplace();
	}
	// Add the end of the spine, and then random posts.
	for _i in 0..(k-1) {
		vs.add_vert(spine_end); 
		spine_end.incr_inplace()
	}
	vs
}

fn is_spinal(g: &Graph) -> bool {
	use crate::constructor::*;
	match g.constructor {
		Constructor::Random(RandomConstructor::Spinal(_, _, _)) => true,
		_ => false,
	}
}

/**
 * Returns a list of vertices that could be counterexamples to the bunkbed conjecture
 * They must be:
 * - suitably far away from zero
 * - not behind a post-wall.
 * - not itself a post
 */
fn get_target_vertices(g: &Graph, posts: VertexSet, k: usize) -> VertexSet {
	let mut targets = VertexSet::new(g.n);
	let mut post_filter = VertexVec::new(g.n, &true);
	for post in posts.iter() {
		post_filter[post] = false;
	}
	let postless_components = g.filtered_components(Some(&post_filter));
	let base_component = postless_components[Vertex::ZERO];
	let dists = g.flood_fill_dist(Vertex::ZERO);
	let mut max_dist = 0;
	for v in g.iter_verts() {
		if let Some(d) = dists[v] {
			if d > max_dist {
				max_dist = d;
			}
		}
	}
	// Hack so that we only target n-1:
	let mut v = g.n.to_max_vertex();
    while targets.size() < k {
        if !posts.has_vert(v) 
                && dists[v].map_or(false, |d| d >= max_dist - 1) 
                && postless_components[v] == base_component {
            targets.add_vert(v);
        }
        v.decr_inplace()
    }
	targets
}

/**
 * Returns a set of vertices which will be the posts of g
 * n.b. it is assumed that this function is deterministic!
 */
fn get_posts(g: &Graph, max_num_posts: Option<usize>) -> VertexSet {
	let max_num_posts = max_num_posts.unwrap_or(g.n.to_usize() / 4);
	let mut posts = VertexSet::new(g.n);
	let mut num_posts = 0;
	let mut reached = VertexVec::new(g.n, &false);
	reached[Vertex::ZERO] = true;
	let mut x = Vertex::ZERO;
	while num_posts < max_num_posts && !x.incr().is_n(g.n){
		if reached[x] {
			x.incr_inplace()
		} else {
			posts.add_vert(x);
			num_posts += 1;
			reached[x] = true;
			for y in g.adj_list[x].iter() {
				reached[*y] = true;
			}
		}
	}
	posts
}

fn get_max_num_posts(rng: &mut ThreadRng) -> usize {
	let p = rng.gen_range(0.0..1.0);
	if p < 0.01 { 
		0
	} else if p < 0.2 {
		1
	} else if p < 0.95 {
		4
	} else {
		100
	}
}

pub struct GraphAndMetadata {
	g: Graph,
	posts: VertexSet,
	targets: VertexSet,
}

impl GraphAndMetadata {
    pub fn new_deterministic(g: &Graph, k: usize) -> GraphAndMetadata {
        let posts = get_posts(g, None);
	    let targets = get_target_vertices(g, posts, k);
        GraphAndMetadata { g: g.to_owned(), posts, targets }
    }

	pub fn new(h: &Graph, rng: &mut ThreadRng, k: usize) -> GraphAndMetadata {
		let mut g: Graph;
		if h.constructor.is_random() {
			let should_be_connected = rng.gen_bool(0.8);
			'find_g: loop {
				g = h.constructor.new_graph();
				if !should_be_connected || g.is_connected() {
					break 'find_g;
				}
			}
		} else {
			g = h.to_owned();	
		}

		let mut targets = VertexSet::new(g.n);
		if is_spinal(&g) {
			targets = get_spinal_vertices(&g, k);
		} else if rng.gen_bool(0.9) {
			targets.add_vert(Vertex::ZERO);
			for v in g.iter_verts().skip(g.n.to_usize() - k + 1) {
				targets.add_vert(v)
			}
		} else {
			while targets.size() < k {
				targets.add_vert(Vertex::of_usize(rng.gen_range(0..g.n.to_usize())))
			}
		}

		let posts = if is_spinal(&g) {
				get_spinal_posts(&g)	
			} else {
				get_posts(&g, Some(get_max_num_posts(rng)))
			};
		
		GraphAndMetadata{g, posts, targets}
	}

    pub fn min_distance_sum(&self) -> usize {
        self.g.min_distance_sum(self.targets)
    }

	/** 
	 * Test if the graph (etc.) is BORING. Things that make a graph (etc.) boring:
	 * - G is disconnected
	 * - Any non-target, non-post vertex has degree <= 2
	 * - Target vertices are adjacent
	 * - G minus posts is disconnected
	 * - G minus targets is disconnected
	 * - G minus deg-1-targets is not 2-connected
	 * - The target vertices all being too close together.
	 */
	pub fn is_boring(&self) -> bool {
		if !self.g.is_connected() {
			return true;
		}
		for v in self.targets.iter() {
			for u in self.g.adj_list[v].iter() {
				if self.targets.has_vert(*u) {
					return true;
				}
			}
		}
		for v in self.g.iter_verts() {
			if !self.posts.has_vert(v) && !self.targets.has_vert(v) && self.g.deg[v].less_than(3) {
				return true;
			}
		}
		if self.g.num_filtered_components(Some(&self.posts.not().to_vec())) >= 2 {
			return true;
		}
		if self.g.num_filtered_components(Some(&self.targets.not().to_vec())) >= 2 {
			return true;
		}
		let mut deg_1_targets = VertexSet::new(self.g.n);
		for v in self.targets.iter() {
			if self.g.deg[v].equals(1) {
				deg_1_targets.add_vert(v);
			}
		}
		if !connectedness::is_filtered_k_connected(&self.g, 2, Some(&deg_1_targets.not().to_vec())) {
			return true;
		}
		if (self.min_distance_sum() as f64) / (self.targets.size() as f64) < 1.55 {
			return true;
		}
		false
	}

	pub fn get_graph_string(&self) -> String {
		let mut s = self.g.serialise();
		s.push_str("-");
		for v in self.iter_verts() {
			if self.has_post(v) {
				s.push_str("P")
			} else {
				s.push_str("0")
			}
		}
		s.push_str("-");
		for v in self.iter_verts() {
			if self.has_target(v) {
				s.push_str("T")
			} else {
				s.push_str("0")
			}
		}
		s
	}

    pub fn has_post(&self, v: Vertex) -> bool {
        self.posts.has_vert(v)
    }

    pub fn has_target(&self, v: Vertex) -> bool {
        self.targets.has_vert(v)
    }

    pub fn iter_adj_list(&self, v: Vertex) -> impl Iterator<Item = &Vertex> {
        self.g.adj_list[v].iter()
    }

    pub fn get_n(&self) -> Order {
        return self.g.n
    }

    pub fn get_targets(&self) -> VertexSet {
        self.targets
    }

    pub fn num_targets(&self) -> usize {
        self.targets.size()
    }

    pub fn get_edge_indexer(&self) -> EdgeIndexer {
        EdgeIndexer::new(&self.g.adj_list)
    }

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        self.g.iter_verts()
    }

    pub fn iter_targets(&self) -> impl Iterator<Item = Vertex> {
        self.targets.iter()
    }

    pub fn iter_edge_sets(&self) -> EdgeSetIterator {
        self.g.iter_edge_sets()
    }

    pub fn iter_verts_bfs(&self) -> BFSIterator {
        self.g.iter_verts_bfs()
    }

    pub fn get_deg_vec(&self) -> VertexVec<Degree> {
        self.g.deg.to_owned()
    }

    pub fn get_bfs_width(&self) -> usize {
        self.g.get_bfs_width()
    }

    pub fn posts_for_table(&self) -> VertexVec<String> {
        self.posts.to_vec().to_vec_of_strings()
    }

    pub fn targets_for_table(&self) -> VertexVec<String> {
        self.targets.to_vec().to_vec_of_strings()
    }

	pub fn print_table(&self) {
		print_vertex_table(vec![("posts", self.posts_for_table()), ("targets", self.targets_for_table())]);
	}

	pub fn print(&self) {
		self.print_table();
		self.g.print();
	}
}