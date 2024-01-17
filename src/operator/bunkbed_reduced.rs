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
fn get_posts(g: &Graph, max_num_posts: Option<usize>) -> VertexSet {
	let max_num_posts = max_num_posts.unwrap_or(4);
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
	n: Order,
	components: VertexVec<Component>,
	vertices: VertexSet,
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
		EquivalenceRelation { 
			n: g.n,
			components : union.to_canonical_component_vec(),
			vertices
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

#[derive(Clone)]
struct ReducedEquivalenceRelation {
	k: usize,
	down: Vec<Component>,
	up: Vec<Component>,
}

impl PartialEq for ReducedEquivalenceRelation {
    fn eq(&self, other: &Self) -> bool {
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
			let _ = write!(f, "{} ", c.to_vertex().to_string());
		}
		let _ = write!(f, "/ ");
		for c in self.up.iter() {
			let _ = write!(f, "{} ", c.to_vertex().to_string());
		}
        write!(f, "]")
    }
}

impl ReducedEquivalenceRelation {
	pub fn of_equiv_rel(rel: EquivalenceRelation) -> ReducedEquivalenceRelation {
		let mut down = vec![];
		let mut up = vec![];
		for v in rel.vertices.iter() {
			down.push(rel.components[v]);
			up.push(rel.components[v.incr_by_order(rel.n)])
		}
		ReducedEquivalenceRelation { k: rel.vertices.size(), down, up }.reduce(rel.n)
	}

	fn reduce(&self, n: Order) -> ReducedEquivalenceRelation {
		let mut new_labels_flat = ComponentVec::new(n.times(2), &None);
		let mut new_labels_cross = ComponentVec::new(n.times(2), &None);
		let mut c_flat = Component::ZERO;
		let mut c_cross = Component::ZERO;
		fn check_label(labels: &mut ComponentVec<Option<Component>>, c: &mut Component, comp: Component) {
			if labels[comp].is_none() {
				labels[comp] = Some(*c);
				c.incr_inplace()
			}
		}
		for v in 0..self.k {
			check_label(&mut new_labels_flat, &mut c_flat, self.down[v]);
			check_label(&mut new_labels_flat, &mut c_flat, self.up[v]);
			check_label(&mut new_labels_cross, &mut c_cross, self.up[v]);
			check_label(&mut new_labels_cross, &mut c_cross, self.down[v]);
		}
		let mut down_flat = vec![];
		let mut up_flat = vec![];
		let mut down_cross = vec![];
		let mut up_cross = vec![];
		for v in 0..self.k {
			down_flat.push(new_labels_flat[self.down[v]].unwrap());
			up_flat.push(new_labels_flat[self.up[v]].unwrap());
			down_cross.push(new_labels_cross[self.up[v]].unwrap());
			up_cross.push(new_labels_cross[self.down[v]].unwrap());
		}
		let mut is_flat_before_cross = true;
		'test_ordering: for (i, comp) in down_flat.iter().enumerate() {
			if *comp < down_cross[i] {
				break 'test_ordering;
			} else if *comp > down_cross[i] {
				is_flat_before_cross = false;
				break 'test_ordering;
			}
		}
		if is_flat_before_cross {
			ReducedEquivalenceRelation { k: self.k, down: down_flat, up: up_flat }
		} else {
			ReducedEquivalenceRelation { k: self.k, down: down_cross, up: up_cross }
		}
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
			permutations.push(ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up}.reduce(Order::of_usize(self.k)))
		}
		permutations
	}

	pub fn to_string(&self) -> String {
		format!("{:?}", self)
	}

	fn print_row(row: &Vec<Component>) {
		for c in row.iter() {
			print!(" {}", c.to_colored_string())
		}
	}

	pub fn print_fancy(&self, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!(" )\n(");
		Self::print_row(&self.down);
		println!(" ) : [{}]", count);
	}

	pub fn print_fancy_pair(&self, denom: &ReducedEquivalenceRelation, ratio: f64, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!("  /");
		Self::print_row(&denom.up);
		print!(" )\n(");
		Self::print_row(&self.down);
		print!(" / ");
		Self::print_row(&denom.down);
		println!(" ) : {:.3} [{}]", ratio, count)
	}
}

struct EquivalenceCounts {
	counts: HashMap<ReducedEquivalenceRelation, usize>
}

impl EquivalenceCounts {
	pub fn new() -> EquivalenceCounts {
		EquivalenceCounts { counts : HashMap::new() }
	}

	pub fn add(&mut self, g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer, posts: &VertexSet, vertices: VertexSet) {
		let conn = EquivalenceRelation::new(g, config, indexer, posts, vertices);
		let reduced_conn = ReducedEquivalenceRelation::of_equiv_rel(conn);
		self.counts.entry(reduced_conn).and_modify(|x| *x += 1).or_insert(1);
	}

	pub fn print(&self) {
		let rows = self.counts.iter()
			.map(|(rel, c)| (rel.to_string(), vec![c.to_string()]))
			.collect::<Vec<(String, Vec<String>)>>();
		print_table(vec!["count".to_string()], rows)
	}
}

pub fn print_connection_counts(g: &Graph, k: usize) {
	// We only need to count the first half of the configs
	let posts = get_posts(g, None);
	let indexer = EdgeIndexer::new(&g.adj_list);

	if k == 2 {
		let v = g.n.to_max_vertex();
		let mut counts = ConnectionCounts::new();

		for config in g.iter_edge_sets() {
			let zero_conns = get_connections(g, Vertex::ZERO, &config, &indexer, &posts);
			let down_conns = zero_conns[v];
			let left_post = zero_conns[Vertex::ZERO][1];
			let up_conns = get_connections(g, Vertex::ZERO, &config.inverse(&indexer), &indexer, &posts)[v];
			let up_conns = [up_conns[1], up_conns[0]];
			let right_post = get_connections(g, v, &config, &indexer, &posts)[v][1];
			counts.add(&down_conns, &up_conns, left_post, right_post);
		}
		println!("Old counts:");
		counts.print();
	}
		
	let mut counts = EquivalenceCounts::new();
	let mut vertices = VertexSet::of_vert(g.n, Vertex::ZERO);
	for v in g.iter_verts().skip(g.n.to_usize() - k + 1) {
		vertices.add_vert(v)
	}
	
	for config in g.iter_edge_sets() {
		counts.add(g, &config, &indexer, &posts, vertices);
	}

	println!("Counts:");
	counts.print();
}

const BIG_CUTOFF: f64 = 5.999;

pub fn simulate_connection_count_ratios(h: &Graph, num_reps: usize, k: usize) {
	let mut max_ratios: HashMap<(ReducedEquivalenceRelation, ReducedEquivalenceRelation), (f64, usize)> = HashMap::new();
	println!("Beginning! num_reps: {}", num_reps);
	let mut all_rels : HashSet<ReducedEquivalenceRelation> = HashSet::new();
	let mut permutations = HashMap::new();
	let mut rng = thread_rng();
	let mut rel_counts: HashMap<ReducedEquivalenceRelation, usize> = HashMap::new();
	let mut time_of_last_print = SystemTime::now();
	for rep in 0..num_reps {
		let mut g: Graph;
		let should_be_connected = rng.gen_bool(0.8);
		'find_g: loop {
			g = h.constructor.new_graph();
			if g.size() <= 19 && (!should_be_connected || g.is_connected()) {
				break 'find_g;
			}
		}
		if time_of_last_print.elapsed().unwrap().as_secs() >= 1 {
			print!("{} ", rep);
			std::io::stdout().flush().unwrap();
			time_of_last_print = SystemTime::now();
		}
		let posts = get_posts(&g, Some(get_max_num_posts(&mut rng)));
		let indexer = EdgeIndexer::new(&g.adj_list);
		let mut counts = EquivalenceCounts::new();
		let mut vertices = VertexSet::new(g.n);
		if rng.gen_bool(0.3) {
			vertices.add_vert(Vertex::ZERO);
			for v in g.iter_verts().skip(g.n.to_usize() - k + 1) {
				vertices.add_vert(v)
			}
		} else {
			while vertices.size() < k {
				vertices.add_vert(Vertex::of_usize(rng.gen_range(0..g.n.to_usize())))
			}
		}

		// This here is the bottleneck.
		for config in g.iter_edge_sets() {
			counts.add(&g, &config, &indexer, &posts, vertices);
		}

		for rel in counts.counts.keys() {
			if !all_rels.contains(rel) {
				all_rels.insert(rel.to_owned());
			}
			if !permutations.contains_key(rel) {
				permutations.insert(rel.to_owned(), rel.get_all_permutations());
			}
			rel_counts.entry(rel.to_owned()).and_modify(|x| *x += 1).or_insert(1);
		}
		for rel1 in all_rels.iter() {
			let count1 = *counts.counts.get(rel1).unwrap_or(&0);
			if count1 != 0 {
				for rel2 in all_rels.iter() {
					let count2 = *counts.counts.get(rel2).unwrap_or(&0);
					let ratio = if count2 == 0 { f64::INFINITY } else { (count1 as f64) / (count2 as f64) };
					// Now get the lexicographically first permutation of the pair (rel1, rel2).
					let mut first_rel1 = rel1;
					let mut first_rel2 = rel2;
					for (i, permed_rel1) in permutations.get(rel1).unwrap().iter().enumerate() {
						let permed_rel2 = &permutations.get(rel2).unwrap()[i];
						if permed_rel1 < first_rel1 || (permed_rel1 == first_rel1 && permed_rel2 < first_rel2) {
							first_rel1 = permed_rel1;
							first_rel2 = permed_rel2;
						}
					}

					max_ratios.entry((first_rel1.to_owned(), first_rel2.to_owned()))
						.and_modify(|(x, count)| if ratio > *x { *x = ratio; *count = 1 } else if ratio == *x { *count += 1 } )	
						.or_insert((ratio, 1));
				}
			}
		}
	}
	let mut rels_ordered = all_rels.iter().map(|x| x.to_owned()).collect::<Vec<ReducedEquivalenceRelation>>();
	rels_ordered.sort();

	println!("All equivalence relations");
	for rel in rels_ordered.iter() {
		println!("{:?}", rel)
	}

	fn is_approx(x: &f64, y: f64) -> bool {
		*x >= y * 0.99999 && *x <= y * 1.000001
	}

	let mut i = 0;
	let mut num_unwrapping_fails = 0;
	let mut num_big = 0;
	let mut num_ratio_one_pairs = 0;
	let mut num_ratio_half_pairs = 0;
	let mut num_ratio_two_pairs = 0;
	let mut good_pairs = vec![];
	for rel1 in rels_ordered.iter() {
		for rel2 in rels_ordered.iter() {
			if rel1 != rel2 {
				if let Some((ratio, count)) = max_ratios.get(&(rel1.to_owned(), rel2.to_owned())) {
					if *ratio > BIG_CUTOFF {
						if k == 2 {
							println!("{}: ({:?}, {:?}) big [{}]", i, rel1, rel2, count);
						}
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
					if k == 2 {
						println!("{}: ({:?}, {:?}) fails due to unwrapping error", i, rel1, rel2);
					}
					num_unwrapping_fails += 1;
				}
				i += 1
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
	let mut all_rel_counts = Vec::from_iter(rel_counts.iter());
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
	println!("There are {} different relations", all_rels.len());
	println!("The three rarest configs are: ");
	for (rel, count) in all_rel_counts.iter().take(3) {
		rel.print_fancy(**count);
		println!();
	}
}