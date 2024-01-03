use crate::graph::*;

use rand::Rng;
use rand::thread_rng;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use queues::*;

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
fn get_connections(g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer, posts: &VertexSet) -> VertexVec<Vec<bool>> {
    let mut q = queue![(Vertex::ZERO, 0)];
    let mut visited = VertexVec::new(g.n, &vec![false, false]);
	visited[Vertex::ZERO][0] = true;
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
	for v in g.iter_verts().skip(1) {
		if !posts.has_vert(v) 
				&& dists[v].map_or(false, |d| d >= max_dist - 1) 
				&& postless_components[v] == base_component {
			targets.push(v);
		}
	}
	targets
}

fn get_posts(g: &Graph) -> VertexSet {
	let mut posts = VertexSet::new(g.n);
	let mut num_posts = 0;
	let mut reached = VertexVec::new(g.n, &false);
	reached[Vertex::ZERO] = true;
	let mut x = Vertex::ZERO;
	while num_posts < 3 && !x.is_n(g.n){
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

pub fn contradicts_reduced_bunkbed_conjecture(g: &Graph) -> bool {
	// Put in some posts, test if the conj is true.
	let mut num_flat_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let mut num_cross_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let posts = get_posts(g);
	let targets = get_target_vertices(g, posts);
	let indexer = EdgeIndexer::new(&g.adj_list);
	for config in g.iter_edge_sets() {
		let connections = get_connections(g, &config, &indexer, &posts);
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

const NUM_SAMPLES: usize = 1000;

pub fn approx_contradicts_reduced_bunkbed_conjecture(g: &Graph) -> bool {
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
	for _i in 0..NUM_SAMPLES {
		// Generate a random config.
		let mut config = EdgeSet::new(&indexer);
		for e in edges.iter() {
			if rng.gen_bool(0.5) {
				config.add_edge(*e, &indexer);
			}
		}
		let connections = get_connections(g, &config, &indexer, &posts);
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
	g.print();
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