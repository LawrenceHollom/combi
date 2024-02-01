use std::io::Write;
use std::time::SystemTime;

use crate::graph::*;

use rand::Rng;
use rand::thread_rng;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use queues::*;

mod data;
mod reduced_equivalence_relation;
mod equivalence_counts;
mod graph_and_metadata;
mod equivalence_relation;
mod equivalence_class;

use equivalence_counts::*;
use data::*;
use graph_and_metadata::*;
use reduced_equivalence_relation::EdgeType;

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

pub fn contradicts_reduced_conditioned_bunkbed_conjecture(g: &Graph) -> bool {
	// Put in some posts, test if the conj is true conditioned on some vertex being unreachable.
	let mut num_flat_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let mut num_cross_avoiding = VertexVec::new(g.n, &VertexVec::new(g.n, &0));
	let g_etc = GraphAndMetadata::new_deterministic(g, 2);
	let indexer = EdgeIndexer::new(&g.adj_list);
	for config in g.iter_edge_sets() {
		let connections = equivalence_relation::get_connections(&g_etc, Vertex::ZERO, &config, &indexer);
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
	for target in g_etc.iter_targets() {
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
	let g_etc = GraphAndMetadata::new_deterministic(g, 2);
	let indexer = EdgeIndexer::new(&g.adj_list);

	println!();
	g.print();

	for config in g.iter_edge_sets() {
		let connections = equivalence_relation::get_connections(&g_etc, Vertex::ZERO, &config, &indexer);
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
	print_vertex_table(vec![("posts", g_etc.posts_for_table()), ("flat", num_flat.to_vec_of_strings()), ("cross", num_cross.to_vec_of_strings())]);
	println!("Targets: {:?}", g_etc.targets_for_table());

	let mut is_contra = false;
	for target in g_etc.iter_targets() {
		if num_cross[target] > num_flat[target] {
			is_contra = true
		}
	}
	is_contra
}

pub fn approx_contradicts_reduced_bunkbed_conjecture(g: &Graph, samples: usize) -> bool {
	let mut num_flat = VertexVec::new(g.n, &0);
	let mut num_cross = VertexVec::new(g.n, &0);
	let g_etc = GraphAndMetadata::new_deterministic(g, 2);
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
		let connections = equivalence_relation::get_connections(&g_etc, Vertex::ZERO, &config, &indexer);
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
	for target in g_etc.iter_targets() {
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
	let g_etc = GraphAndMetadata::new_deterministic(g, 2);
	if g_etc.num_targets() == 0 {
		true
	} else if !g_etc.has_target(g.n.to_max_vertex()) {
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

pub fn print_connection_counts(g: &Graph, k: usize) {
	// We only need to count the first half of the configs
	let g_etc = GraphAndMetadata::new_deterministic(g, k);
	let indexer = EdgeIndexer::new(&g.adj_list);		
	let mut counts = EquivalenceCounts::new();
	
	for config in g.iter_edge_sets() {
		counts.add(&g_etc, &config, &indexer);
	}

	println!("Counts:");
	counts.print();
}

fn get_ratios_naive(g_etc: &GraphAndMetadata, data: &mut Data) {
	let indexer = g_etc.get_edge_indexer();
	let mut counts = EquivalenceCounts::new();

	// This here is the bottleneck.
	for config in g_etc.iter_edge_sets() {
		counts.add(g_etc, &config, &indexer);
	}

	data.add_equivalence_counts(g_etc, &counts, true)
}

const PRINT_DEBUG_LEVEL: u8 = 0;

/**
 * Use dynamic programming to compute the EquivalenceCounts between the given set of vertices.
 */
fn get_ratios_dp(g_etc: &GraphAndMetadata, edge_type: EdgeType, data: &mut Data, print_counts: bool) {
	let mut counts = EquivalenceCounts::new_singleton();
	let n = g_etc.get_n();
	let mut vert_activity = VertexVec::new(n, &None);
	let mut num_active_verts = 1;
	vert_activity[Vertex::ZERO] = Some(0);
	let mut remaining_degree = g_etc.get_deg_vec();
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
		println!("BFS width of g = {}", g_etc.get_bfs_width());
		print_vertex_table(vec![("posts", g_etc.posts_for_table()), ("targets", g_etc.targets_for_table())]);
	}

	for v in g_etc.iter_verts_bfs().skip(1) {
		// add room for this new vertex, and then add edges and remove unwanted old vertices.
		if PRINT_DEBUG_LEVEL >= 1 {
			print!("Starting on vertex {}; num_active_verts = {}, num RERs = {}; ", v, num_active_verts, counts.num_distinct_rers());
			for w in g_etc.iter_verts() {
				if vert_activity[w].is_some() {
					print!("{}({}) ", w, remaining_degree[w].to_usize());
				}
			}
			println!();
		}
		vert_activity[v] = Some(num_active_verts);
		let mut v_index = num_active_verts;
		num_active_verts += 1;
		counts.add_vertex(g_etc.has_post(v));

		for u in g_etc.iter_adj_list(v) {
			// If it needs to be removed; all its edges will have been processed.
			if *u != v && remaining_degree[*u].equals(1) && !g_etc.has_target(*u) {
				if let Some(index) = vert_activity[*u] {
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Adding killer edge {}-{}", v, u);
					}
					// amalgamate in the edge
					counts.amalgamate_edge(&default_edge, index, v_index);
					remaining_degree[v].decr_inplace();
					remaining_degree[*u].decr_inplace();
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
		for u in g_etc.iter_adj_list(v) {
			if *u != v {
				// There's an edge here that we should probably do something about.
				if let Some(index) = vert_activity[*u] {
					// There's an edge we need to amalgamate in.
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Adding standard edge {}-{}", v, *u);
					}
					counts.amalgamate_edge(&default_edge, index, v_index);
					remaining_degree[*u].decr_inplace();
					remaining_degree[v].decr_inplace();
				}
			}
		}
		if remaining_degree[v].equals(0) && !g_etc.has_target(v) {
			// This one needs to be removed too!
			remove_vertex(v_index, &mut counts, &mut vert_activity, &mut num_active_verts);
			vert_activity[v] = None
		}
	}

	if PRINT_DEBUG_LEVEL >= 3 {
		counts.print();
	}

	// Finally, remove vertices not in the prescribed target set.
	for v in g_etc.iter_verts() {
		if !g_etc.has_target(v) {
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

	if g_etc.num_targets() >= 3 {
		// Remove one vertex and add results to equivalence counts
		for i in 0..g_etc.num_targets() {
			let mut counts_copy = counts.to_owned();
			remove_vertex(i, &mut counts_copy, &mut vert_activity, &mut num_active_verts);
			counts_copy.reduce();
			data.add_equivalence_counts(g_etc, &counts_copy, !print_counts);
		}
	}

	if PRINT_DEBUG_LEVEL >= 1 || print_counts {
		counts.print_summary(&g_etc);
		println!("targets: {:?}", g_etc.targets_for_table());
		println!("num_active = {}. Activity: {:?}", num_active_verts, vert_activity);
		counts.print_fancy();
	}

	counts.reduce();
	data.add_equivalence_counts(g_etc, &counts, !print_counts);
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
		if rng.gen_bool(0.9) {
			'find_interesting_g_etc: loop {
				g_etc = GraphAndMetadata::new(h, &mut rng, k);
				if !g_etc.is_boring() {
					if PRINT_DEBUG_LEVEL >= 2 {
						println!("Found interesting graph; min_sum = {}", g_etc.min_distance_sum());
						g_etc.print();
					}
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
	if num_reps > 1 {
		data.print();
		data.save_to_file();
		println!("Constructor: {:?}", h.constructor.to_string());
		println!("Average {} boring graphs per interesting one", num_boring / num_reps);
	}
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