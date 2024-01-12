use std::collections::HashMap;
use std::hash::*;
use std::fmt::*;

use crate::graph::*;

use rand::Rng;
use rand::thread_rng;
use utilities::*;
use utilities::component_tools::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use queues::*;
use strum::*;

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
fn get_posts(g: &Graph) -> VertexSet {
	let mut posts = VertexSet::new(g.n);
	let mut num_posts = 0;
	let mut reached = VertexVec::new(g.n, &false);
	reached[Vertex::ZERO] = true;
	let mut x = Vertex::ZERO;
	while num_posts < 4 && !x.incr().is_n(g.n){
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

pub fn contradicts_reduced_conditioned_bunkbed_conjecture(g: &Graph) -> bool {
	// Put in some posts, test if the conj is true conditioned on some vertex being unreachable.
	let mut num_flat_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let mut num_cross_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let posts = get_posts(g);
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
	let posts = get_posts(g);
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
	let posts = get_posts(g);
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
	let posts = get_posts(g);
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

#[derive(Debug, EnumIter, ToString, EnumCount)]
enum ConnectionType {
	Empty,
	Flat,
	Cross,
	DoubleFlat,
	DoubleCross,
	LeftPost,
	RightPost,
	LeftTriangle, // |>
	RightTriangle, // <|
	DoublePost,
	Complete,
	AAAAAAAAAAAAA,
}

impl ConnectionType {
	pub fn get_from_conns(down_conns: &[bool; 2], up_conns: &[bool; 2], left_post: bool, right_post: bool) -> ConnectionType {
		use ConnectionType::*;
		match (down_conns, up_conns, left_post, right_post) {
			([false, false], [false, false], false, false) => Empty,
			([true, false], [false, false], false, false) | ([false, false], [false, true], false, false) => Flat,
			([false, true], [false, false], false, false) | ([false, false], [true, false], false, false) => Cross,
			([true, false], [false, true], false, false) => DoubleFlat,
			([false, true], [true, false], false, false) => DoubleCross,
			([false, false], [false, false], true, false) => LeftPost,
			([false, false], [false, false], false, true) => RightPost,
			([true, false], [true, false], true, false) | ([false, true], [false, true], true, false) => LeftTriangle,
			([true, true], [false, false], false, true) | ([false, false], [true, true], false, true) => RightTriangle,
			([false, false], [false, false], true, true) => DoublePost,
			([true, true], [true, true], true, true) => Complete,
			_ => {
				println!("Bad conns! {:?}", (down_conns, up_conns, left_post, right_post));
				AAAAAAAAAAAAA
			} //panic!("This should be impossible! {:?}", (down_conns, up_conns, left_post, right_post))
		}
	}
}

struct ConnectionCounts {
	counts: [usize; ConnectionType::COUNT],
}

impl ConnectionCounts {
	pub fn new() -> ConnectionCounts {
		ConnectionCounts { counts : [0; ConnectionType::COUNT] }
	}

	pub fn add(&mut self, down_conns: &[bool; 2], up_conns: &[bool; 2], left_post: bool, right_post: bool) {
		let conn = ConnectionType::get_from_conns(down_conns, up_conns, left_post, right_post);
		self.counts[conn as usize] += 1;
	}

	pub fn print(&self) {
		let rows = ConnectionType::iter().enumerate()
			.map(|(i, c)| (c.to_string(), vec![self.counts[i].to_string()]))
			.collect::<Vec<(String, Vec<String>)>>();
		print_table(vec!["count".to_string()], rows)
	}
}

#[derive(Clone)]
struct EquivalenceRelation {
	components: VertexVec<Component>,
}

impl PartialEq for EquivalenceRelation {
    fn eq(&self, other: &Self) -> bool {
        for (i, c) in self.components.iter_enum() {
			if *c != other.components[i] {
				return false;
			}
		}
		true
    }
}

impl Eq for EquivalenceRelation {}

impl Hash for EquivalenceRelation {
	fn hash<H: Hasher>(&self, state: &mut H) {
        for c in self.components.iter() {
			c.hash(state)
		}
    }
}

impl EquivalenceRelation {
	pub fn new(g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer, posts: &VertexSet, vertices: VertexSet) -> EquivalenceRelation {
		fn pack_vertex(n: Order, x: Vertex, level: bool) -> Vertex {
			if level {
				x.incr_by_order(n)
			} else {
				x
			}
		}
		let mut union = UnionFind::new(g.n.times(2));
		for v in vertices.iter() {
			let down_conns = get_connections(g, v, config, indexer, posts);
			let up_conns = get_connections(g, v, &config.inverse(indexer), indexer, posts);
			for u in vertices.iter() {
				if down_conns[u][0] {
					union.merge(pack_vertex(g.n, u, false), pack_vertex(g.n, v, false))
				}
				if down_conns[u][1] {
					union.merge(pack_vertex(g.n, u, true), pack_vertex(g.n, v, false))
				}
				if up_conns[u][0] {
					union.merge(pack_vertex(g.n, u, true), pack_vertex(g.n, v, true))
				}
				if up_conns[u][1] {
					union.merge(pack_vertex(g.n, u, false), pack_vertex(g.n, v, true))
				}
			}
		}
		EquivalenceRelation { components : union.to_canonical_component_vec() }
	}

	pub fn to_string(&self) -> String {
		format!("{:?}", self.components.iter().map(|c| c.to_vertex().to_string()).collect::<Vec<String>>())
	}
}

impl Debug for EquivalenceRelation {
	find a better way to print this thing
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{:?}", self.components.iter().map(|c| c.to_vertex().to_string()).collect::<Vec<String>>())
    }
}

struct EquivalenceCounts {
	counts: HashMap<EquivalenceRelation, usize>
}

impl EquivalenceCounts {
	pub fn new() -> EquivalenceCounts {
		EquivalenceCounts { counts : HashMap::new() }
	}

	pub fn add(&mut self, g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer, posts: &VertexSet, vertices: VertexSet) {
		let conn = EquivalenceRelation::new(g, config, indexer, posts, vertices);
		self.counts.entry(conn).and_modify(|x| *x += 1).or_insert(1);
	}

	pub fn print(&self) {
		let rows = self.counts.iter()
			.map(|(rel, c)| (rel.to_string(), vec![c.to_string()]))
			.collect::<Vec<(String, Vec<String>)>>();
		print_table(vec!["count".to_string()], rows)
	}
}

pub fn print_connection_counts(g: &Graph) {
	// We only need to count the first half of the configs
	let mut counts = ConnectionCounts::new();
	let mut new_counts = EquivalenceCounts::new();
	let posts = get_posts(g);
	let indexer = EdgeIndexer::new(&g.adj_list);
	let v = g.n.to_max_vertex();

	for config in g.iter_edge_sets() {
		let zero_conns = get_connections(g, Vertex::ZERO, &config, &indexer, &posts);
		let down_conns = zero_conns[v];
		let left_post = zero_conns[Vertex::ZERO][1];
		let up_conns = get_connections(g, Vertex::ZERO, &config.inverse(&indexer), &indexer, &posts)[v];
		let up_conns = [up_conns[1], up_conns[0]];
		let right_post = get_connections(g, v, &config, &indexer, &posts)[v][1];
		counts.add(&down_conns, &up_conns, left_post, right_post);

		new_counts.add(g, &config, &indexer, &posts, VertexSet::of_vec(g.n, &vec![Vertex::ZERO, v]));
	}

	println!("Counts:");
	counts.print();
	println!("New counts:");
	new_counts.print();
}

pub fn simulate_connection_count_ratios(g: &Graph, num_reps: usize) {
	let mut max_ratios: HashMap<(EquivalenceRelation, EquivalenceRelation), f64> = HashMap::new();
	for _i in 0..num_reps {
		let h = g.constructor.new_graph();
		let posts = get_posts(&h);
		let indexer = EdgeIndexer::new(&g.adj_list);
		let v = g.n.to_max_vertex();
		let mut counts = EquivalenceCounts::new();

		for config in g.iter_edge_sets() {
			counts.add(g, &config, &indexer, &posts, VertexSet::of_vec(g.n, &vec![Vertex::ZERO, v]));
		}
		for (rel1, count1) in counts.counts.iter() {
			if *count1 != 0 {
				for (rel2, count2) in counts.counts.iter() {
					let ratio = (*count1 as f64) / (*count2 as f64);
					max_ratios.entry((rel1.to_owned(), rel2.to_owned())).and_modify(|x| *x = x.max(ratio)).or_insert(ratio);
				}
			}
		}
	}
	for (i, ((rel1, rel2), ratio)) in max_ratios.iter().enumerate() {
		println!("{}: ({:?}, {:?}) {:.3}", i, rel1, rel2, ratio)
	}
}