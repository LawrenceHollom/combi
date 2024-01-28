use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::*;
use std::fmt::*;
use std::io::Write;
use std::time::SystemTime;

use crate::graph::*;

use rand::Rng;
use rand::rngs::ThreadRng;
use rand::thread_rng;
use utilities::*;
use utilities::component_tools::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use queues::*;
use colored::*;

use super::connectedness;

/**
 * This file deals with the version of the conjecture wherein we have
 * conditioned on the positions of the posts, and every horizontal
 * edge is either up or down (and not both)
 */

 /**
  * Tests if two vertices are connected under a given configuration of 
  * present horizontal edges. u_l and v_l store the layers of u and v
  */
fn are_connected(g: &Graph, u: Vertex, u_l: bool, v: Vertex, v_l: bool,
		 posts: VertexSet, config: &EdgeSet, indexer: &EdgeIndexer) -> bool {
    let mut q = queue![];
    let mut visited = VertexVec::new(g.n, &(false, false));
    if u_l {
		visited[u].1 = true;
    } else {
		visited[u].0 = true;
    }
    let _ = q.add((u, u_l));
    let mut found_target = false;
    while q.size() > 0 && !found_target {
		let (x, x_l) = q.remove().unwrap();
		for y in g.adj_list[x].iter() {
			if config.has_edge(Edge::of_pair(x, *y), indexer) {
				if x_l && !visited[*y].1 {
					visited[*y].1 = true;
					if *y == v && x_l == v_l {
						found_target = true;
					}
					let _ = q.add((*y, x_l));
				}
			} else {
				if !x_l && !visited[*y].0 {
					visited[*y].0 = true;
					if *y == v && x_l == v_l {
						found_target = true;
					}
					let _ = q.add((*y, x_l));
				}
			}
		}
		if posts.has_vert(x) && (if x_l { !visited[x].0 } else { !visited[x].1 }) {
			if x_l {
				visited[x].0 = true;
			} else {
				visited[x].1 = true;
			}
			if x == v && x_l == !v_l {
				found_target = true;
			}
			let _ = q.add((x, !x_l));
		}
    }
    found_target
}

fn flat_paths_must_flip_at_post(g: &Graph, u: Vertex, v: Vertex, posts: VertexSet,
				post: Vertex, config: &EdgeSet, indexer: &EdgeIndexer)
				-> bool {
    are_connected(g, u, false, v, false, posts, config, indexer) &&
	!are_connected(g, u, false, v, false, posts.remove_vert_immutable(post),
		       config, indexer)
}

fn cross_paths_must_flip_at_post(g: &Graph, u: Vertex, v: Vertex, posts: VertexSet,
				 post: Vertex, config: &EdgeSet, indexer: &EdgeIndexer)
				 -> bool {
    are_connected(g, u, false, v, true, posts, config, indexer) &&
	!are_connected(g, u, false, v, true, posts.remove_vert_immutable(post),
		       config, indexer)

}

fn post_removal_induction_internal(g: &Graph, verbose: bool) -> bool {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let u = Vertex::ZERO;
    let mut are_all_good = true;
    for v in g.n.iter_verts().skip(1) {
		if verbose {
			println!("TESTING PERCOLATION {} -> {}", u, v);
		}
		let mut posts = VertexSet::new(g.n);
		for post in g.n.iter_verts() {
			posts.add_vert(post);
			if verbose {
				print!("POSTS = ");
				posts.print();
			}
			let mut counter = 0;
			for config in g.iter_edge_sets() {
				if flat_paths_must_flip_at_post(g, u, v, posts, post,
								&config, &indexer) {
					counter -= 1;
				}
				if cross_paths_must_flip_at_post(g, u, v, posts, post,
								&config, &indexer) {
					counter += 1;
				}
			}
			if verbose {
				println!("Removing post {}. Counter: {}", post, counter);
			}
			if counter < 0 {
				are_all_good = false;
				print!("Not all good! v = {}, post = {}, posts = ", v, post);
				posts.print();
			}
		}
    }
    are_all_good
}

pub fn test_post_removal_induction(g: &Graph) {
    let _ = post_removal_induction_internal(g, true);
}

pub fn is_post_removal_induction_always_good(g: &Graph) -> bool {
    post_removal_induction_internal(g, false)
}

/**
 * config has an edge if that edge is down.
 * Returns a VertexVec of items otf [is down-connected to 0, is up-connected to 0]
 */
fn get_connections(g: &Graph, u: Vertex, config: &EdgeSet, indexer: &EdgeIndexer, posts: &VertexSet) -> VertexVec<[bool; 2]> {
    let mut q = queue![(u, 0)];
    let mut visited = VertexVec::new(g.n, &[false, false]);
	visited[u][0] = true;
	while let Ok((v, layer)) = q.remove() {
		if posts.has_vert(v) && !visited[v][1 - layer] {
			visited[v][1 - layer] = true;
			let _ = q.add((v, 1 - layer));
		}
		for u in g.adj_list[v].iter() {
			if !visited[*u][layer] && config.has_edge(Edge::of_pair(v, *u), indexer) == (layer == 0) {
				visited[*u][layer] = true;
				let _ = q.add((*u, layer));
			}
		}
	}
	visited
}

/**
 * Returns a list of vertices that could be counterexamples to the bunkbed conjecture
 * They must be:
 * - suitably far away from zero
 * - not behind a post-wall.
 * - not itself a post
 */
fn get_target_vertices(g: &Graph, posts: VertexSet) -> Vec<Vertex> {
	let mut targets = vec![];
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
	/*for v in g.iter_verts().skip(1) {
		if !posts.has_vert(v) 
				&& dists[v].map_or(false, |d| d >= max_dist - 1) 
				&& postless_components[v] == base_component {
			targets.push(v);
		}
	}*/
	// Hack so that we only target n-1:
	let v = g.n.to_max_vertex();
	if !posts.has_vert(v) 
			&& dists[v].map_or(false, |d| d >= max_dist - 1) 
			&& postless_components[v] == base_component {
		targets.push(v);
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

pub fn contradicts_reduced_conditioned_bunkbed_conjecture(g: &Graph) -> bool {
	// Put in some posts, test if the conj is true conditioned on some vertex being unreachable.
	let mut num_flat_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let mut num_cross_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let posts = get_posts(g, None);
	let targets = get_target_vertices(g, posts);
	let indexer = EdgeIndexer::new(&g.adj_list);
	for config in g.iter_edge_sets() {
		let connections = get_connections(g, Vertex::ZERO, &config, &indexer, &posts);
		for (v, v_conns) in connections.iter_enum() {
			for (u, u_conns) in connections.iter_enum() {
				if u == v || (!u_conns[0] && !u_conns[1]) {
					if v_conns[0] {
						num_flat_avoiding[v][u] += 1;
					}
					if v_conns[1] {
						num_cross_avoiding[v][u] += 1;
					}
				}
			}
		}
	}

	let mut is_contra = false;
	for target in targets {
		for u in g.iter_verts() {
			if num_cross_avoiding[target][u] > num_flat_avoiding[target][u] {
				is_contra = true
			}
		}
	}
	is_contra
}

pub fn contradicts_reduced_bunkbed_conjecture(g: &Graph) -> bool {
	// Put in some posts, test if the conj is true.
	let mut num_flat = VertexVec::new(g.n, &0);
	let mut num_cross = VertexVec::new(g.n, &0);
	let posts = get_posts(g, None);
	let targets = get_target_vertices(g, posts);
	let indexer = EdgeIndexer::new(&g.adj_list);

	println!();
	g.print();

	for config in g.iter_edge_sets() {
		let connections = get_connections(g, Vertex::ZERO, &config, &indexer, &posts);
		for (v, conns) in connections.iter_enum() {
			if conns[0] {
				num_flat[v] += 1;
			}
			if conns[1] {
				num_cross[v] += 1;
			}
		}
	}

	println!("Precise completed!");
	print_vertex_table(vec![("posts", posts.to_vec().to_vec_of_strings()), ("flat", num_flat.to_vec_of_strings()), ("cross", num_cross.to_vec_of_strings())]);
	println!("Targets: {:?}", targets);

	let mut is_contra = false;
	for target in targets {
		if num_cross[target] > num_flat[target] {
			is_contra = true
		}
	}
	is_contra
}

pub fn approx_contradicts_reduced_bunkbed_conjecture(g: &Graph, samples: usize) -> bool {
	let mut num_flat = VertexVec::new(g.n, &0);
	let mut num_cross = VertexVec::new(g.n, &0);
	let posts = get_posts(g, None);
	let targets = get_target_vertices(g, posts);
	let mut rng = thread_rng();
	let indexer = EdgeIndexer::new(&g.adj_list);
	let mut edges = vec![];
	for u in g.iter_verts() {
		for v in g.adj_list[u].iter() {
			if *v > u {
				edges.push(Edge::of_pair(u, *v));
			}
		}
	}
	for _i in 0..samples {
		// Generate a random config.
		let mut config = EdgeSet::new(&indexer);
		for e in edges.iter() {
			if rng.gen_bool(0.5) {
				config.add_edge(*e, &indexer);
			}
		}
		let connections = get_connections(g, Vertex::ZERO, &config, &indexer, &posts);
		for (v, conns) in connections.iter_enum() {
			if conns[0] {
				num_flat[v] += 1;
			}
			if conns[1] {
				num_cross[v] += 1;
			}
		}
	}
	/*println!("Approxes completed! Posts: {:?}", posts.to_vec());
	println!("Num flat: {:?}", num_flat);
	println!("Num cross: {:?}", num_cross);
	println!("Targets: {:?}", targets);*/
	let mut is_contra = false;
	for target in targets {
		if num_cross[target] >= num_flat[target] {
			is_contra = true
		}
	}
	is_contra
}

/**
 * Returns true if g has some subgraph which can be bunkbed-reduced, and so g
 * cannot be a minimal counterexample to the bunkbed conjecture.
 * e.g. has a non-start, non-end, non-post vertex of degree 2
 * We're implicitly kinda assuming that the target vertex is n-1.
 */
pub fn is_bunkbed_reducible(g: &Graph) -> bool {
	let posts = get_posts(g, None);
	let targets = get_target_vertices(g, posts);
	if targets.is_empty() {
		true
	} else if !targets.contains(&g.n.to_max_vertex()) {
		true
	} else { // n-1 is a target.
		// currently the only reduction we have is that min_deg >= 3
		let mut is_reducible = false;
		'test_degrees: for v in g.iter_verts() {
			if g.deg[v].equals(2) {
				is_reducible = true;
				break 'test_degrees;
			}
		}
		is_reducible
	}
}
struct GraphAndMetadata {
	g: Graph,
	posts: VertexSet,
	targets: VertexSet,
}

impl GraphAndMetadata {
	fn new(h: &Graph, rng: &mut ThreadRng, k: usize) -> GraphAndMetadata {
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

	pub fn new_manual(g: &Graph, posts: VertexSet, targets: VertexSet) -> GraphAndMetadata {
		GraphAndMetadata { g: g.to_owned(), posts, targets }
	}

	/** 
	 * Test if the graph (etc.) is BORING. Things that make a graph (etc.) boring:
	 * - G is disconnected
	 * - Any non-target, non-post vertex has degree <= 2
	 * - Target vertices are adjacent
	 * - G minus posts is disconnected
	 * - G minus targets is disconnected
	 * - G minus deg-1-targets is not 2-connected
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
		false
	}

	pub fn print(&self) {
		print_vertex_table(vec![("posts", self.posts.to_vec().to_vec_of_strings()), 
			("targets", self.targets.to_vec().to_vec_of_strings())]);
		self.g.print();
	}
}

#[derive(Clone)]
struct EquivalenceRelation {
	n: Order,
	components: VertexVec<Component>,
	vertices: VertexSet,
}

impl EquivalenceRelation {
	pub fn new(g_etc: &GraphAndMetadata, config: &EdgeSet, indexer: &EdgeIndexer) -> EquivalenceRelation {
		fn pack_vertex(n: Order, x: Vertex, level: bool) -> Vertex {
			if level {
				x.incr_by_order(n)
			} else {
				x
			}
		}
		let n = g_etc.g.n;
		let mut union = UnionFind::new(n.times(2));
		for v in g_etc.targets.iter() {
			let down_conns = get_connections(&g_etc.g, v, config, indexer, &g_etc.posts);
			let up_conns = get_connections(&g_etc.g, v, &config.inverse(indexer), indexer, &g_etc.posts);
			for u in g_etc.targets.iter() {
				if down_conns[u][0] {
					union.merge(pack_vertex(n, u, false), pack_vertex(n, v, false))
				}
				if down_conns[u][1] {
					union.merge(pack_vertex(n, u, true), pack_vertex(n, v, false))
				}
				if up_conns[u][0] {
					union.merge(pack_vertex(n, u, true), pack_vertex(n, v, true))
				}
				if up_conns[u][1] {
					union.merge(pack_vertex(n, u, false), pack_vertex(n, v, true))
				}
			}
		}
		EquivalenceRelation { 
			n,
			components : union.to_canonical_component_vec(),
			vertices: g_etc.targets
		 }
	}
}

impl Debug for EquivalenceRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		let _ = write!(f, "[ ");
		for v in self.vertices.iter() {
			let _ = write!(f, "{} ", self.components[v].to_vertex().to_string());
		}
		let _ = write!(f, "/ ");
		for v in self.vertices.iter() {
			let _ = write!(f, "{} ", self.components[v.incr_by_order(self.n)].to_vertex().to_string());
		}
        write!(f, "]")
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct EquivalenceClass(usize);

impl EquivalenceClass {
	pub fn incr_inplace(&mut self) {
		self.0 += 1
	}

	pub fn to_string(&self) -> String {
		self.0.to_string()
	}

    pub fn to_colored_string(&self) -> ColoredString {
        match self.0 {
            0 => "0".red(),
            1 => "1".blue(),
            2 => "2".green(),
            3 => "3".yellow(),
            4 => "4".magenta(),
            5 => "5".cyan(),
            _ => self.0.to_string().bold(),
        }
    }
}

#[derive(Copy, Clone)]
enum EdgeType {
	Classic,
	FiftyFifty,
	Double,
	ThreeWay,
	Posted,
}

impl EdgeType {
	pub fn of_usize(edge_type: usize) -> EdgeType {
		use EdgeType::*;
		match edge_type {
			0 => Classic,
			1 => FiftyFifty,
			2 => Double,
			3 => ThreeWay,
			4 => Posted,
			_ => FiftyFifty,
		}
	}
	
	pub fn to_rer_vec(&self) -> Vec<ReducedEquivalenceRelation> {
		use EdgeType::*;
		let raw_edges = match self {
			Classic => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]], [[0, 1], [2, 3]], [[0, 0], [0, 0]]],
			FiftyFifty => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]]],
			Double => vec![[[0, 1], [1, 0]], [[0, 0], [1, 1]]],
			ThreeWay => vec![[[0, 0], [1, 1]], [[0, 0], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
			Posted => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
		};
		raw_edges.iter()
			.map(|raw| ReducedEquivalenceRelation::of_raw_vecs(raw)
				.reduce_no_symmetrise(true))
			.collect::<Vec<ReducedEquivalenceRelation>>()
	}
}

#[derive(Clone)]
struct ReducedEquivalenceRelation {
	k: usize,
	down: Vec<EquivalenceClass>,
	up: Vec<EquivalenceClass>,
	next_label: EquivalenceClass,
}

impl PartialEq for ReducedEquivalenceRelation {
    fn eq(&self, other: &Self) -> bool {
		if self.k != other.k {
			return false;
		}
        for (i, c) in self.down.iter().enumerate() {
			if *c != other.down[i] {
				return false;
			}
		}
		for (i, c) in self.up.iter().enumerate() {
			if *c != other.up[i] {
				return false;
			}
		}
		true
    }
}

impl Eq for ReducedEquivalenceRelation {}

impl Ord for ReducedEquivalenceRelation {
    fn cmp(&self, other: &Self) -> Ordering {
		if self.k != other.k {
			return self.k.cmp(&other.k);
		}
        for (i, c) in self.down.iter().enumerate() {
			if c.cmp(&other.down[i]) != Ordering::Equal {
				return c.cmp(&other.down[i])
			}
		}
		for (i, c) in self.up.iter().enumerate() {
			if c.cmp(&other.up[i]) != Ordering::Equal {
				return c.cmp(&other.up[i])
			}
		}
		Ordering::Equal
    }
}

impl PartialOrd for ReducedEquivalenceRelation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for ReducedEquivalenceRelation {
	fn hash<H: Hasher>(&self, state: &mut H) {
        for c in self.down.iter() {
			c.hash(state)
		}
		for c in self.up.iter() {
			c.hash(state)
		}
    }
}

impl Debug for ReducedEquivalenceRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		let _ = write!(f, "[ ");
		for c in self.down.iter() {
			let _ = write!(f, "{} ", c.to_string());
		}
		let _ = write!(f, "/ ");
		for c in self.up.iter() {
			let _ = write!(f, "{} ", c.to_string());
		}
        write!(f, "]")
    }
}

impl ReducedEquivalenceRelation {
	fn of_raw_vecs(raw: &[[usize; 2]; 2]) -> ReducedEquivalenceRelation {
		let down = raw[0].iter().map(|x| EquivalenceClass(*x)).collect::<Vec<EquivalenceClass>>();
		let up = raw[1].iter().map(|x| EquivalenceClass(*x)).collect::<Vec<EquivalenceClass>>();
		let next_label = EquivalenceClass(raw[0][0].max(raw[0][1]).max(raw[1][0]).max(raw[1][1]) + 1);
		ReducedEquivalenceRelation { k: 2, down, up, next_label }
	}

	pub fn of_equiv_rel(rel: EquivalenceRelation) -> ReducedEquivalenceRelation {
		let mut down = vec![];
		let mut up = vec![];
		let mut new_labels = ComponentVec::new(rel.n.times(2), &None);
		let mut next_label = EquivalenceClass(0);
		for v in rel.vertices.iter() {
			let c = rel.components[v];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			down.push(new_labels[c].unwrap());
			let c = rel.components[v.incr_by_order(rel.n)];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			up.push(new_labels[c].unwrap())
		}
		ReducedEquivalenceRelation { k: rel.vertices.size(), down, up, next_label }.reduce_and_symmetrise()
	}

	pub fn empty(k: usize) -> ReducedEquivalenceRelation {
		let down = (0..k).map(|x| EquivalenceClass(x)).collect::<Vec<EquivalenceClass>>();
		let up = (k..(2 *k)).map(|x| EquivalenceClass(x)).collect::<Vec<EquivalenceClass>>();
		ReducedEquivalenceRelation { k, down, up, next_label: EquivalenceClass(2 * k) }.reduce_and_symmetrise()
	}

	fn reduce_no_symmetrise(&self, is_flat: bool) -> ReducedEquivalenceRelation {
		let mut new_labels = vec![None; (self.k + 1) * 2];
		let mut next_label = EquivalenceClass(0);
		fn check_label(labels: &mut Vec<Option<EquivalenceClass>>, next_label: &mut EquivalenceClass, comp: EquivalenceClass) {
			if labels[comp.0].is_none() {
				labels[comp.0] = Some(*next_label);
				next_label.incr_inplace()
			}
		}
		for v in 0..self.k {
			if is_flat {
				check_label(&mut new_labels, &mut next_label, self.down[v]);
				check_label(&mut new_labels, &mut next_label, self.up[v]);
			} else {
				check_label(&mut new_labels, &mut next_label, self.up[v]);
				check_label(&mut new_labels, &mut next_label, self.down[v]);
			}
		}
		let mut new_down = vec![];
		let mut new_up = vec![];
		for v in 0..self.k {
			if is_flat {
				new_down.push(new_labels[self.down[v].0].unwrap());
				new_up.push(new_labels[self.up[v].0].unwrap());
			} else {
				new_down.push(new_labels[self.up[v].0].unwrap());
				new_up.push(new_labels[self.down[v].0].unwrap());
			}
		}
		ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label }
	}

	fn reduce_and_symmetrise(&self) -> ReducedEquivalenceRelation {
		let new_flat = self.reduce_no_symmetrise(true);
		let new_cross = self.reduce_no_symmetrise(false);
		new_flat.min(new_cross)
	}

	pub fn get_all_permutations(&self) -> Vec<ReducedEquivalenceRelation> {
		let mut permutations = vec![];
		// iterate through all the k! permutations, produce, reduce, and add.
		let sigmas = if self.k == 2 {
				vec![vec![0, 1], vec![1, 0]]
			} else if self.k == 3 {
				vec![vec![0, 1, 2], vec![0, 2, 1], vec![1, 0, 2], vec![1, 2, 0], vec![2, 0, 1], vec![2, 1, 0]]	
			} else {
				panic!("Guess what? It's time to fix this awful hack you put in!")
			};
		for sigma in sigmas.iter() {
			let mut new_down = vec![];
			let mut new_up = vec![];
			for i in sigma.iter() {
				new_down.push(self.down[*i]);
				new_up.push(self.up[*i]);
			}
			permutations.push(ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label: self.next_label }.reduce_and_symmetrise())
		}
		permutations
	}

	pub fn add_vertex(&mut self, is_post: bool) {
		self.down.push(self.next_label);
		if !is_post {
			self.next_label.incr_inplace();
		}
		self.up.push(self.next_label);
		self.next_label.incr_inplace();
		self.k += 1;
	}

	/** 
	 * Blindly replace all d with c in the RER
	 * will need reducing at a later date.
	 */
	fn merge_components(&mut self, c: EquivalenceClass, d: EquivalenceClass) {
		for thing in self.down.iter_mut() {
			if *thing == d {
				*thing = c
			}
		}
		for thing in self.up.iter_mut() {
			if *thing == d {
				*thing = c
			}
		}
	}

	pub fn amalgamate_edge(&mut self, new_edge: &ReducedEquivalenceRelation, x: usize, y: usize) {
		// We currently do the silly, easy version, and move to the harder one later.
		// Let's just case-bash it for now.
		if new_edge.down[0] == new_edge.down[1] {
			self.merge_components(self.down[x], self.down[y])
		}
		if new_edge.up[0] == new_edge.up[1] {
			self.merge_components(self.up[x], self.up[y])
		}
		let verts = [x, y];
		for i in 0..2 {
			for j in 0..2 {
				if new_edge.down[i] == new_edge.up[j] {
					self.merge_components(self.down[verts[i]], self.up[verts[j]])
				}
			}
		}
		*self = self.reduce_no_symmetrise(false)
	}

	pub fn remove_vertex(mut self, x: usize) -> ReducedEquivalenceRelation {
		self.down.remove(x);
		self.up.remove(x);
		self.k -= 1;
		self.reduce_no_symmetrise(false)
	}

	pub fn flip(&self) -> ReducedEquivalenceRelation {
		ReducedEquivalenceRelation {
			k: self.k,
			down: self.up.to_owned(),
			up: self.down.to_owned(),
			next_label: self.next_label,
		}
	}

	pub fn is_classically_flat(&self) -> bool {
		self.down[0] == self.down[1]
	}

	pub fn is_classically_cross(&self) -> bool {
		self.down[0] == self.up[1]
	}

	pub fn to_string(&self) -> String {
		format!("{:?}", self)
	}

	fn print_row(row: &Vec<EquivalenceClass>) {
		for c in row.iter() {
			print!(" {}", c.to_colored_string())
		}
	}

	pub fn print_fancy(&self, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!(" ) code = {}\n(", self.to_code());
		Self::print_row(&self.down);
		println!(" ) : [{}]", count);
	}

	pub fn print_fancy_pair(&self, denom: &ReducedEquivalenceRelation, ratio: f64, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!("  /");
		Self::print_row(&denom.up);
		print!(" ) codes = ({}) / ({})\n(", self.to_code(), denom.to_code());
		Self::print_row(&self.down);
		print!(" / ");
		Self::print_row(&denom.down);
		println!(" ) : {:.6} [{}]", ratio, count)
	}

	pub fn to_code(&self) -> u128 {
		let mut code = 0_u128;
		let mut pow = 1_u128;
		for x in self.down.iter() {
			code += (x.0 as u128) * pow;
			pow *= 2 * self.k as u128;
		}
		for x in self.up.iter() {
			code += (x.0 as u128) * pow;
			pow *= 2 * self.k as u128;
		}
		code
	}
}

#[derive(Clone)]
struct EquivalenceCounts {
	counts: HashMap<ReducedEquivalenceRelation, u128>
}

impl EquivalenceCounts {
	pub fn new() -> EquivalenceCounts {
		EquivalenceCounts { counts : HashMap::new() }
	}

	pub fn new_singleton() -> EquivalenceCounts {
		let mut counts = HashMap::new();
		counts.insert(ReducedEquivalenceRelation::empty(1), 1);
		EquivalenceCounts { counts }
	}

	pub fn add(&mut self, g_etc: &GraphAndMetadata, config: &EdgeSet, indexer: &EdgeIndexer) {
		let conn = EquivalenceRelation::new(g_etc, config, indexer);
		let reduced_conn = ReducedEquivalenceRelation::of_equiv_rel(conn);
		self.counts.entry(reduced_conn).and_modify(|x| *x += 1).or_insert(1);
	}

	pub fn add_vertex(&mut self, is_post: bool) {
		let mut new_counts = HashMap::new();
		for (mut rel, count) in self.counts.drain() {
			rel.add_vertex(is_post);
			new_counts.insert(rel, count);
		}
		self.counts = new_counts
	}

	pub fn amalgamate_edge(&mut self, edge: &Vec<ReducedEquivalenceRelation>, x: usize, y: usize) {
		let mut new_counts = HashMap::new();
		for (rel, count) in self.counts.iter() {
			for edge_rel in edge.iter() {
				let mut new_rel = rel.to_owned();
				new_rel.amalgamate_edge(edge_rel, x, y);
				new_counts.entry(new_rel)
						.and_modify(|x| *x += *count)
						.or_insert(*count);
			}
		}
		self.counts = new_counts
	}

	pub fn remove_vertex(&mut self, x: usize) {
		let mut new_counts = HashMap::new();
		for (rel, count) in self.counts.drain() {
			let new_rel = rel.remove_vertex(x);
			new_counts.entry(new_rel)
					.and_modify(|y| *y += count)
					.or_insert(count);
		}
		self.counts = new_counts
	}

	/**
	 * Symmetrise the configs and all that.
	 */
	pub fn reduce(&mut self) {
		let mut new_counts = HashMap::new();
		for (rel, count) in self.counts.drain() {
			let new_rel = rel.reduce_and_symmetrise();
			new_counts.entry(new_rel)
					.and_modify(|v| *v = count.max(*v))
					.or_insert(count);
		}
		self.counts = new_counts
	}

	pub fn num_distinct_rers(&self) -> usize {
		self.counts.keys().len()
	}

	pub fn print(&self) {
		let mut rows = self.counts.iter()
			.map(|(rel, c)| (rel.to_string(), vec![c.to_string(), rel.k.to_string()]))
			.collect::<Vec<(String, Vec<String>)>>();
		rows.sort();
		print_table(vec!["count".to_string(), "k".to_string()], rows)
	}

	pub fn print_summary(&self, g: &Graph) {
		fn get_num(counts: &HashMap<ReducedEquivalenceRelation, u128>, raw: &[[usize; 2]; 2]) -> u128 {
			let rel = ReducedEquivalenceRelation::of_raw_vecs(raw).reduce_and_symmetrise();
			*counts.get(&rel).unwrap_or(&0)
		}
		let num_single_flat = get_num(&self.counts, &[[1, 2], [0, 0]]);
		let num_single_cross = get_num(&self.counts, &[[1, 2], [0, 1]]);
		let num_double_flat = get_num(&self.counts, &[[1, 1], [0, 0]]);
		let num_double_cross = get_num(&self.counts, &[[1, 0], [0, 1]]);
		let single_ratio = (num_single_cross as f64) / (num_single_flat as f64);
		let double_ratio = (num_double_cross as f64) / (num_double_flat as f64);
		println!("1-flat: {}, 1-cross: {}, ratio: {:.5} // 2-flat: {}, 2-cross: {}, ratio: {:.5}", 
		num_single_flat, num_single_cross, single_ratio, num_double_flat, num_double_cross, double_ratio);
		if num_single_flat < num_single_cross || num_double_flat < num_double_cross {
			println!("Counterexample found!");
			g.print();
			panic!("SO IT HAS BEEN DONE!")
		}
	}
}

pub fn print_connection_counts(g: &Graph, k: usize) {
	// We only need to count the first half of the configs
	let posts = get_posts(g, None);
	let indexer = EdgeIndexer::new(&g.adj_list);		
	let mut counts = EquivalenceCounts::new();
	let mut targets = VertexSet::of_vert(g.n, Vertex::ZERO);
	for v in g.iter_verts().skip(g.n.to_usize() - k + 1) {
		targets.add_vert(v)
	}
	let g_etc = GraphAndMetadata::new_manual(g, posts, targets);
	
	for config in g.iter_edge_sets() {
		counts.add(&g_etc, &config, &indexer);
	}

	println!("Counts:");
	counts.print();
}

const BIG_CUTOFF: f64 = 5.999;

struct Data {
	permutations: HashMap<ReducedEquivalenceRelation, Vec<ReducedEquivalenceRelation>>,
	rel_counts: HashMap<ReducedEquivalenceRelation, usize>,
	max_ratios: HashMap<(ReducedEquivalenceRelation, ReducedEquivalenceRelation), (f64, usize)>,
	all_rels: HashSet<ReducedEquivalenceRelation>,
}

impl Data {
	fn new() -> Data {
		Data {
			permutations: HashMap::new(),
			rel_counts: HashMap::new(),
			max_ratios: HashMap::new(),
			all_rels: HashSet::new(),
		}
	}

	fn print(&self) {
		let mut rels_ordered = self.all_rels.iter().map(|x| x.to_owned()).collect::<Vec<ReducedEquivalenceRelation>>();
		rels_ordered.sort();
	
		println!("All equivalence relations");
		for rel in rels_ordered.iter() {
			println!("{:?} : code = {}, count = {}", rel, rel.to_code(), self.rel_counts.get(rel).unwrap())
		}
	
		fn is_approx(x: &f64, y: f64) -> bool {
			*x >= y * 0.99999 && *x <= y * 1.000001
		}
	
		let mut num_unwrapping_fails = 0;
		let mut num_big = 0;
		let mut num_ratio_one_pairs = 0;
		let mut num_ratio_half_pairs = 0;
		let mut num_ratio_two_pairs = 0;
		let mut good_pairs = vec![];
		for rel1 in rels_ordered.iter() {
			for rel2 in rels_ordered.iter() {
				if rel1 != rel2 {
					if let Some((ratio, count)) = self.max_ratios.get(&(rel1.to_owned(), rel2.to_owned())) {
						if *ratio > BIG_CUTOFF {
							num_big += 1;
						} else {
							if is_approx(ratio, 1.0) {
								num_ratio_one_pairs += 1;
							} else if is_approx(ratio, 0.5) {
								num_ratio_half_pairs += 1;
							} else if is_approx(ratio, 2.0) {
								num_ratio_two_pairs += 1;
							}
							good_pairs.push((rel1.to_owned(), rel2.to_owned(), *count, *ratio));
						}
					} else {
						num_unwrapping_fails += 1;
					}
				}
			}
		}
	
		fn cmp_pair(count1: usize, r1: f64, count2: usize, r2: f64) -> Ordering {
			let r1u = (r1 * 100.0) as usize;
			let r2u = (r2 * 100.0) as usize;
			let ratio_compare = r1u.cmp(&r2u);
			if ratio_compare.is_eq() {
				count1.cmp(&count2)
			} else {
				ratio_compare
			}
		}
	
		good_pairs.sort_by(|(_, _, count1, r1), (_, _, count2, r2)| cmp_pair(*count1, *r1, *count2, *r2));
		let mut all_rel_counts = Vec::from_iter(self.rel_counts.iter());
		all_rel_counts.sort_by(|(_, c1), (_, c2)| c1.cmp(c2));
		println!("All ratios less than {}:", BIG_CUTOFF);
		for (rel1, rel2, count, ratio) in good_pairs.iter() {
			rel1.print_fancy_pair(rel2, *ratio, *count);
			println!();
		}
		println!("Num ratios bigger than {}: {}", BIG_CUTOFF, num_big);
		println!("Num ratios resulting in unwrapping errors: {}", num_unwrapping_fails);
		println!("{} pairs have a ratio of precisely 0.500", num_ratio_half_pairs);
		println!("{} pairs have a ratio of precisely 1.000", num_ratio_one_pairs);
		println!("{} pairs have a ratio of precisely 2.000", num_ratio_two_pairs);
		println!("There are {} different relations", self.all_rels.len());
		println!("The three rarest configs are: ");
		for (rel, count) in all_rel_counts.iter().take(3) {
			rel.print_fancy(**count);
			println!();
		}
	}
}

fn is_genuine_counterexample(g_etc: &GraphAndMetadata, data: &Data, counts: &EquivalenceCounts) {
	let mut num_flat = 0;
	let mut num_cross = 0;

	for (rel1, count) in counts.counts.iter() {
		if rel1.k == 2 {
			if rel1.is_classically_flat() {
				num_flat += *count
			}
			if rel1.is_classically_cross() {
				num_cross += *count
			}
			if rel1.flip().is_classically_flat() {
				num_flat += *count
			}
			if rel1.flip().is_classically_cross() {
				num_cross += *count
			}
		}
	}

	if num_cross > num_flat {
		data.print();
		println!("Just g:");
		counts.print();
		g_etc.print();
		println!("num classically flat: {}, num classically cross: {}", num_flat, num_cross);
		panic!("WE'VE GOT A LIVE ONE!")
	}
}

const NOOOTERS: [(u128, u128); 2] = [(148, 144), (20, 80)];

fn consider_noooting(g_etc: &GraphAndMetadata, data: &Data, counts: &EquivalenceCounts) {
	is_genuine_counterexample(g_etc, data, counts);
	for (rel1, count1) in counts.counts.iter() {
		for (rel2, count2) in counts.counts.iter() {
			for (code1, code2) in NOOOTERS.iter() {
				if rel1.to_code() == *code1 && rel2.to_code() == *code2 && count1 > count2 {
					data.print();
					println!("Found a graph with the desired config ratio!");
					rel1.print_fancy_pair(rel2, (*count1 as f64) / (*count2 as f64), 1);
					println!("Counts {} / {}", count1, count2);
					g_etc.print();
					/*counts.print();
					let mut counts_copy = counts.to_owned();
					remove_vertex(2, &mut counts_copy, &mut vert_activity, &mut num_active_verts);
					println!("With highest-index vertex removed:");
					counts_copy.print();*/
					panic!("NOOOT NOOOT")
				}
			}
		}
	}
}

fn add_equivalence_counts(g_etc: &GraphAndMetadata, counts: &EquivalenceCounts, data: &mut Data) {
	for rel in counts.counts.keys() {
		if !data.all_rels.contains(rel) {
			data.all_rels.insert(rel.to_owned());
		}
		if !data.permutations.contains_key(rel) {
			data.permutations.insert(rel.to_owned(), rel.get_all_permutations());
		}
		data.rel_counts.entry(rel.to_owned()).and_modify(|x| *x += 1).or_insert(1);
	}
	for rel1 in data.all_rels.iter() {
		let count1 = *counts.counts.get(rel1).unwrap_or(&0);
		if count1 != 0 {
			for rel2 in data.all_rels.iter() {
				if rel1.k == rel2.k {
					let count2 = *counts.counts.get(rel2).unwrap_or(&0);
					let ratio = if count2 == 0 { f64::INFINITY } else { (count1 as f64) / (count2 as f64) };
					// Now get the lexicographically first permutation of the pair (rel1, rel2).
					let mut first_rel1 = rel1;
					let mut first_rel2 = rel2;
					for (i, permed_rel1) in data.permutations.get(rel1).unwrap().iter().enumerate() {
						let permed_rel2 = &data.permutations.get(rel2).unwrap()[i];
						if permed_rel1 < first_rel1 || (permed_rel1 == first_rel1 && permed_rel2 < first_rel2) {
							first_rel1 = permed_rel1;
							first_rel2 = permed_rel2;
						}
					}

					data.max_ratios.entry((first_rel1.to_owned(), first_rel2.to_owned()))
						.and_modify(|(x, count)| if ratio > *x { *x = ratio; *count = 1 } else if ratio == *x { *count += 1 } )	
						.or_insert((ratio, 1));
				}
			}
		}
	}
	consider_noooting(g_etc, data, counts)
}

fn get_ratios_naive(g_etc: &GraphAndMetadata, data: &mut Data) {
	let indexer = EdgeIndexer::new(&g_etc.g.adj_list);
	let mut counts = EquivalenceCounts::new();

	// This here is the bottleneck.
	for config in g_etc.g.iter_edge_sets() {
		counts.add(g_etc, &config, &indexer);
	}

	add_equivalence_counts(g_etc, &counts, data)
}

const PRINT_DEBUG_LEVEL: u8 = 0;

/**
 * Use dynamic programming to compute the EquivalenceCounts between the given set of vertices.
 */
fn get_ratios_dp(g_etc: &GraphAndMetadata, edge_type: EdgeType, data: &mut Data, print_summary: bool) {
	let mut counts = EquivalenceCounts::new_singleton();
	let n = g_etc.g.n;
	let mut vert_activity = VertexVec::new(n, &None);
	let mut num_active_verts = 1;
	vert_activity[Vertex::ZERO] = Some(0);
	let mut remaining_degree = g_etc.g.deg.to_owned();
	let default_edge = edge_type.to_rer_vec();

	fn remove_vertex(index: usize, counts: &mut EquivalenceCounts, vert_activity: &mut VertexVec<Option<usize>>, num_active_verts: &mut usize) {
		counts.remove_vertex(index);
		//counts.print();
		// Now reduce all later indices to make up for this removal.
		for index2 in vert_activity.iter_mut() {
			if let Some(index2) = index2 {
				if *index2 > index {
					*index2 -= 1;
				}
			}
		}
		*num_active_verts -= 1;
	}

	if PRINT_DEBUG_LEVEL >= 1 {
		println!("BFS width of g = {}", g_etc.g.get_bfs_width());
		print_vertex_table(vec![("posts", g_etc.posts.to_vec().to_vec_of_strings()), ("targets", g_etc.targets.to_vec().to_vec_of_strings())]);
	}

	for v in g_etc.g.iter_verts_bfs().skip(1) {
		// add room for this new vertex, and then add edges and remove unwanted old vertices.
		if PRINT_DEBUG_LEVEL >= 1 {
			print!("Starting on vertex {}; num_active_verts = {}, num RERs = {}; ", v, num_active_verts, counts.num_distinct_rers());
			for w in g_etc.g.iter_verts() {
				if vert_activity[w].is_some() {
					print!("{}({}) ", w, remaining_degree[w].to_usize());
				}
			}
			println!();
		}
		vert_activity[v] = Some(num_active_verts);
		let mut v_index = num_active_verts;
		num_active_verts += 1;
		counts.add_vertex(g_etc.posts.has_vert(v));

		//println!("Added vertex {}", v);
		//counts.print();

		for u in g_etc.g.adj_list[v].iter() {
			// If it needs to be removed; all its edges will have been processed.
			if *u != v && remaining_degree[*u].equals(1) && !g_etc.targets.has_vert(*u) {
				if let Some(index) = vert_activity[*u] {
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Adding killer edge {}-{}", v, u);
					}
					//println!("Activity: {:?}", vert_activity);
					// amalgamate in the edge
					counts.amalgamate_edge(&default_edge, index, v_index);
					remaining_degree[v].decr_inplace();
					remaining_degree[*u].decr_inplace();
					//counts.print();
					// remove the vertex.
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Remove the vertex!");
					}
					remove_vertex(index, &mut counts, &mut vert_activity, &mut num_active_verts);
					if v_index > index {
						v_index -= 1;
					}
				}
				vert_activity[*u] = None;
			}
		}
		for u in g_etc.g.adj_list[v].iter() {
			if *u != v {
				// There's an edge here that we should probably do something about.
				if let Some(index) = vert_activity[*u] {
					// There's an edge we need to amalgamate in.
					//println!("Vertex activity: {:?}", vert_activity);
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Adding standard edge {}-{}", v, *u);
					}
					counts.amalgamate_edge(&default_edge, index, v_index);
					remaining_degree[*u].decr_inplace();
					remaining_degree[v].decr_inplace();
					//counts.print();
				}
			}
		}
		if remaining_degree[v].equals(0) && !g_etc.targets.has_vert(v) {
			// This one needs to be removed too!
			remove_vertex(v_index, &mut counts, &mut vert_activity, &mut num_active_verts);
			vert_activity[v] = None
		}
	}

	if PRINT_DEBUG_LEVEL >= 3 {
		counts.print();
	}

	// Finally, remove vertices not in the prescribed target set.
	for v in g_etc.g.iter_verts() {
		if !g_etc.targets.has_vert(v) {
			if let Some(index) = vert_activity[v] {
				if PRINT_DEBUG_LEVEL >= 2 {
					println!("Removing vertex {} (in final steps)", v);
				}
				// This vertex needs to be removed.
				remove_vertex(index, &mut counts, &mut vert_activity, &mut num_active_verts);
				vert_activity[v] = None;
			}
		}
	}

	if g_etc.targets.size() >= 3 {
		// Remove one vertex and add results to equivalence counts
		for i in 0..g_etc.targets.size() {
			let mut counts_copy = counts.to_owned();
			remove_vertex(i, &mut counts_copy, &mut vert_activity, &mut num_active_verts);
			counts_copy.reduce();
			add_equivalence_counts(g_etc, &counts_copy, data);
		}
	}

	counts.reduce();

	if PRINT_DEBUG_LEVEL >= 1 {
		println!("targets: {:?}", g_etc.targets.to_vec());
		println!("num_active = {}. Activity: {:?}", num_active_verts, vert_activity);
		counts.print();
	}

	if print_summary {
		counts.print_summary(&g_etc.g);
	}

	add_equivalence_counts(g_etc, &counts, data);
}

fn is_spinal(g: &Graph) -> bool {
	use crate::constructor::*;
	match g.constructor {
		Constructor::Random(RandomConstructor::Spinal(_, _, _)) => true,
		_ => false,
	}
}

fn get_spinal_vertices(g: &Graph, k: usize) -> VertexSet {
	let mut spine_end = Vertex::ZERO;
	while g.adj[spine_end][spine_end.incr()] {
		spine_end.incr_inplace();
	}
	let mut vs = VertexSet::of_vert(g.n, Vertex::ZERO);
	for _i in 0..(k-1) {
		vs.add_vert(spine_end);
		spine_end.decr_inplace()
	}
	vs
}

fn simulate_connection_count_ratios(h: &Graph, num_reps: usize, k: usize, edge_type: Option<EdgeType>) {
	println!("Beginning! num_reps: {}", num_reps);
	let mut data = Data::new();
	let mut rng = thread_rng();
	let mut time_of_last_print = SystemTime::now();
	let mut num_boring = 0;
	for rep in 0..num_reps {
		//let rep_start_time = SystemTime::now();
		let mut g_etc;
		if rng.gen_bool(0.8) {
			'find_interesting_g_etc: loop {
				g_etc = GraphAndMetadata::new(h, &mut rng, k);
				if !g_etc.is_boring() {
					break 'find_interesting_g_etc
				} else {
					num_boring += 1;
				}
			}
		} else {
			g_etc = GraphAndMetadata::new(h, &mut rng, k);
		}

		if time_of_last_print.elapsed().unwrap().as_secs() >= 1 {
			print!("{} ", rep);
			std::io::stdout().flush().unwrap();
			time_of_last_print = SystemTime::now();
		}

		if let Some(edge_type) = edge_type {
			get_ratios_dp(&g_etc, edge_type, &mut data, num_reps == 1);
		} else {
			get_ratios_naive(&g_etc, &mut data);
		}
		//println!("rep duration: {}ms, edges: {}, bfs width: {}", rep_start_time.elapsed().unwrap().as_millis(), g_etc.g.size(), g_etc.g.get_bfs_width());
	}
	data.print();
	println!("Constructor: {:?}", h.constructor.to_string());
	println!("Average {} boring graphs per interesting one", num_boring / num_reps);
}

pub fn simulate_connection_count_ratios_naive(h: &Graph, num_reps: usize, k: usize) {
	simulate_connection_count_ratios(h, num_reps, k, None);
}

// It's time to get dynamic.
pub fn bunkbed_connection_counts_dp(h: &Graph, num_reps: usize, k: usize, edge_type: usize) {
	simulate_connection_count_ratios(h, num_reps, k, Some(EdgeType::of_usize(edge_type)));
}

pub fn search_for_counterexample(h: &Graph, edge_type: usize) {
	let edge_type = EdgeType::of_usize(edge_type);
	let mut data = Data::new();
	let mut num_loops = 0;
	let mut time_of_last_print = SystemTime::now();
	let mut rng = thread_rng();

	'search_for_counterexample: loop {
		let g_etc = GraphAndMetadata::new(h, &mut rng, 2);
		print!("DP ");
		std::io::stdout().flush().unwrap();		
		get_ratios_dp(&g_etc, edge_type, &mut data, true);
		num_loops += 1;
		if num_loops > 1000 {
			break 'search_for_counterexample;
		}

		if time_of_last_print.elapsed().unwrap().as_secs() >= 1 {
			print!("{} ", num_loops);
			std::io::stdout().flush().unwrap();
			time_of_last_print = SystemTime::now();
		}
	}
	data.print();
}