use std::{io::{self, Write}, ops::{Index, IndexMut}, u32};

use component_tools::*;
use queues::*;
use rand::{seq::SliceRandom, thread_rng, Rng};
use utilities::{*, edge_tools::*, rational::Rational, vertex_tools::*};

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

    fn other(c: Colour) -> Colour {
        use Colour::*;
        match c {
            Red => Blue,
            Blue => Red,
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
            r: 1000,
            b: 1000,
        }
    }

    fn min(&self) -> u32 {
        self.r.min(self.b)
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

struct Distances {
    from_red: ColourPair,
    from_blue: ColourPair,
}

impl Distances {
    fn of_pairs(blue_dists: ColourPair, red_dists: ColourPair) -> Distances {
        Distances {
            from_red: red_dists,
            from_blue: blue_dists,
        }
    }

    fn min(&self) -> u32 {
        self.from_red.min().min(self.from_blue.min())
    }

    fn max_alternating(&self) -> u32 {
        use Colour::*;
        self.from_red[Blue].max(self.from_blue[Red])
    }
}

/**
 * Run a flood-fill to find the minimal number of flips to the antipode.
 * Can use efficient algorithms because all "distances" are 0 or 1.
 */
fn distance_to_antipode(g: &Graph, v: Vertex, reds: &BigEdgeSet, print_debug: bool) -> Distances {
    use Colour::*;
    let target = cube_antipode(g, v);
    // Blue dists is for paths starting with a blue edge.
    let mut blue_dists = VertexVec::new(g.n, &ColourPair::new());
    // Red dists if for paths starting with a red edge.
    let mut red_dists = VertexVec::new(g.n, &ColourPair::new());
    
    // Set up the distances with the first steps from v.
    for u_sta in g.adj_list[Vertex::ZERO].iter() {
        let u = u_sta.xor(v);
        let e = Edge::of_pair(v, u);
        if reds.has_edge(e) {
            red_dists[u][Red] = 0;
        } else {
            blue_dists[u][Blue] = 0;
        }
    }

    // Just iterate through the vertices in order; all of the predecessors will be processed in time.
    // We need to start at v and move away, so xor the whole thing with v for the required rotation.
    'iter_verts: for u_sta in g.iter_verts().skip(1) {
        let u = u_sta.xor(v);
        if print_debug {
            println!("u_sta = {} (b = {}), u = {} (b = {})", u_sta, u_sta.to_binary_string(), u, u.to_binary_string());
        }
        if g.adj[Vertex::ZERO][u_sta] {
            // We don't go through nbrs of ZERO again, as they've already been done.
            continue 'iter_verts;
        }
        'iter_nbrs: for w in g.adj_list[u].iter() {
            if w.xor(v) >= u_sta {
                // Only consider earlier vertices. (Xoring to return to the standard ordering.)
                continue 'iter_nbrs;
            }
            let e = Edge::of_pair(u, *w);
            let e_col = Colour::of_bool(reds.has_edge(e));
            let other_col = Colour::other(e_col);
            
            let new_red_dist = red_dists[*w][e_col].min(red_dists[*w][other_col] + 1);
            if new_red_dist < red_dists[u][e_col] {
                red_dists[u][e_col] = new_red_dist;
            }
            let new_blue_dist = blue_dists[*w][e_col].min(blue_dists[*w][other_col] + 1);
            if new_blue_dist < blue_dists[u][e_col] {
                blue_dists[u][e_col] = new_blue_dist;
            }
        }
    }

    if print_debug {
        for v in g.iter_verts() {
            println!("\tVertex {} from red to (red = {}, blue = {}) ; from blue to (red = {}, blue = {})", 
                v, red_dists[v][Red], red_dists[v][Blue], blue_dists[v][Red], blue_dists[v][Blue]);
        }
    }

    Distances::of_pairs(blue_dists[target], red_dists[target])
}

/**
 * Is this colouring a representative of its symmetry class?
 * Used to speed up computation, as we only need to check representatives
 */
fn is_colouring_representative(g: &Graph, indexer: &EdgeIndexer, reds: EdgeSet) -> bool {
    let mut is_red = false;
    let mut num_blue = 0;
    for v in g.adj_list[Vertex::ZERO].iter() {
        let e = Edge::of_pair(Vertex::ZERO, *v);
        if is_red {
            // Can't go red and then back again.
            if !reds.has_edge(e, indexer) {
                return false
            }
        } else if reds.has_edge(e, indexer) {
            is_red = true;
        } else {
            num_blue += 1;
        }
    }

    // Check that ZERO is the maximally blue vertex.
    for v in g.iter_verts().skip(1) {
        let mut this_num_blue = 0;
        for w in g.adj_list[v].iter() {
            let e = Edge::of_pair(v, *w);
            if !reds.has_edge(e, indexer) {
                this_num_blue += 1;
            }
        }
        if this_num_blue > num_blue {
            return false
        }
    }
    true
}

/**
 * Is there, for every v in V(g) with antipode u, a path from v to u starting on red
 * and finishing on blue, and also a path starting on blue and finishing on red?
 */
fn is_choosable(g: &Graph, indexer: &EdgeIndexer, reds: EdgeSet) -> bool {
    for v in g.n.div(2).iter_verts() {
        let u = cube_antipode(g, v);
        let mut direction = 1;
        let mut v_reds = VertexSet::new(g.n);
        let mut v_blues = VertexSet::new(g.n);
        let mut u_reds = VertexSet::new(g.n);
        let mut u_blues = VertexSet::new(g.n);
        while g.n.more_than(direction) {
            let xorand = Vertex::of_usize(direction);
            let v_prime = v.xor(xorand);
            let v_e = Edge::of_pair(v, v_prime);
            let u_prime = u.xor(xorand);
            let u_e = Edge::of_pair(u, u_prime);
            
            // Record for which directions we get red or blue
            if reds.has_edge(v_e, indexer) {
                v_reds.add_vert(xorand);
            } else {
                v_blues.add_vert(xorand);
            }
            if reds.has_edge(u_e, indexer) {
                u_reds.add_vert(xorand);
            } else {
                u_blues.add_vert(xorand);
            }

            direction *= 2;
        }

        if v_reds.is_empty() || v_blues.is_empty() || u_reds.is_empty() || u_blues.is_empty() {
            return false
        }
        if v_reds.size() == 1 && u_blues == v_reds {
            return false
        }
        if v_blues.size() == 1 && u_reds == v_blues {
            return false
        }
    }
    true
}

/**
 * Print some information about what percentage of the way there we are.
 */
fn print_update_info(num_colourings_found: u64, num_colourings: u64, last_percentage: &mut u64) {
    let percentage = (num_colourings_found * 100) / num_colourings;
    if percentage > *last_percentage {
        print!("{}% ", percentage);
        let _ = io::stdout().flush();
        *last_percentage = percentage;
    }
}

/**
 * Must be given a hypercube as input.
 * We then iterate over edge colourings, and decide whether they are good or bad.
 * The vertices are adjacent by binary digits (excellent!)
 */
pub fn partition_colourings(g: &Graph) {
    let indexer = EdgeIndexer::new(&g.adj_list);
    // Should probably wrap up all this storage into a struct.
    let mut max_dist = 0;
    let mut max_choosable_dist = 0;
    let mut max_unchoosable_dist = 0;
    let mut num_extremal = 0;
    let mut num_colourings_after_reduction: u64 = 0;
    let mut num_colourings_found = 0;
    let mut num_bad = 0;
    let mut last_percentage = 0;
    let num_colourings = 2_u64.pow(g.size() as u32);
    'iter_colourings: for reds in g.iter_edge_sets() {
        num_colourings_found += 1;
        print_update_info(num_colourings_found, num_colourings, &mut last_percentage);
        if !is_colouring_representative(g, &indexer, reds) {
            continue 'iter_colourings;
        }

        let big_reds = BigEdgeSet::of_edge_set(reds, &indexer);

        let choosable = is_choosable(g, &indexer, reds);

        num_colourings_after_reduction += 1;
        let mut total_antipode_distance = 0;
        let mut total_alternating_distance = 0;
        for v in g.n.div(2).iter_verts() {
            let dists = distance_to_antipode(g, v, &big_reds, false);
            total_antipode_distance += dists.min();
            total_alternating_distance += dists.max_alternating();
        }
        if total_antipode_distance > max_dist {
            num_extremal = 1;
            max_dist = total_antipode_distance;
            println!("New maximum!");
            //print_colouring(g, &reds);
            println!("Total antipode distance = {}\n", total_antipode_distance);

            //distance_to_antipode(g, &indexer, Vertex::ZERO, reds, true);
        } else if total_antipode_distance == max_dist {
            num_extremal += 1;
        }

        if choosable && total_alternating_distance > max_choosable_dist && total_antipode_distance > 4 {
            // We only care about choosable things if they don't have very good antipode stuff to start with.
            max_choosable_dist = total_alternating_distance;

            println!("New choosable maximum!");
            //print_colouring(&indexer, reds);
            println!("Total alternating distance = {} (antipode = {})\n", total_alternating_distance, total_antipode_distance);
        }
        if !choosable && total_antipode_distance > max_unchoosable_dist {
            max_unchoosable_dist = total_antipode_distance;
        }
        let is_bad = total_antipode_distance > 6 && (!choosable || total_alternating_distance > 12);
        if is_bad {
            num_bad += 1;
            println!("Found {} bad ones!", num_bad);
            //print_colouring(&indexer, reds);
        }
    }
    println!("\nNum extremal configurations: {}", num_extremal);
    println!("There are {} colourings in total, of which {} survived reduction", num_colourings, num_colourings_after_reduction);
    println!("Max choosable dist = {}", max_choosable_dist);
    println!("Max unchoosable dist = {}", max_unchoosable_dist);
}

/**
 * Produces a random colouring of the graph G by picking a few seed points and then colouring
 * out from them in shells.
 * Returns the set of red edges.
 */
fn get_random_shell_colouring(g: &Graph) -> BigEdgeSet {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut step = BigVertexSet::new(g.n);
    let mut next_step;
    
    let mut rng = thread_rng();
    let num_starters: usize = rng.gen_range(2..=g.n.to_usize()); // No idea what a sensible number to put here is...
    for _i in 0..num_starters {
        let v = Vertex::of_usize(rng.gen_range(0..g.n.to_usize()));
        step.add_vert(v);
    }

    let mut reds = BigEdgeSet::new(&indexer);
    let mut coloured = BigEdgeSet::new(&indexer);
    let mut is_red_step = true;
    while !step.is_empty() {
        next_step = BigVertexSet::new(g.n);
        for v in step.iter() {
            for w in g.adj_list[v].iter() {
                let e = Edge::of_pair(v, *w);
                if !coloured.has_edge(e) {
                    // We need to colour this edge.
                    coloured.add_edge(e);
                    if is_red_step {
                        reds.add_edge(e);
                    }
                    if !step.has_vert(*w) {
                        next_step.add_vert(*w);
                    }
                }
            }
        }
        step = next_step.to_owned();
        is_red_step = !is_red_step
    }
    reds
}

/**
 * Produces a random colouring by sampling each edge independently with probability 1/2.
 * Returns the set of red edges.
 */
fn get_uniform_random_colouring(g: &Graph) -> BigEdgeSet {
    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut rng = thread_rng();
    let mut reds = BigEdgeSet::new(&indexer);

    for e in g.iter_edges() {
        if rng.gen_bool(0.5) {
            reds.add_edge(e)
        }
    }

    reds
}

/**
 * Produces a colouring where edges are weighted away from matching their neighbours.
 * This is not uniform or particularly easy to model analytically.
 * Needs to shuffle the order of the edges, and so runs in m log m time.
 * Returns the set of red edges.
 */
fn get_unfriendly_random_colouring(g: &Graph) -> BigEdgeSet {
    /**
     * Produce the probability that an edge is red given what the resulting sizes
     * of the red and blue components would be if the edge was red or blue respectively.
     * 
     * Weights towards trying to make lots of small components, but it's not clear how
     * best to do this.
     */
    fn get_raw_red_prob(n: Order, r: usize, b: usize) -> f64 {
        fn make_big_components(r: usize, b: usize) -> f64 {
            (r as f64) / ((r + b) as f64)
        }
        fn make_small_components(r: usize, b: usize) -> f64 {
            // This does it really quickly:
            if r > b {
                // r is bigger, so don't make the edge red.
                return 0.0
            } else {
                return 1.0
            }
        }
        fn magic_cutoff(n: Order) -> usize {
            (n.to_usize() as f64).sqrt() as usize
        }
        if r + b < magic_cutoff(n) {
            // If the components are small, we try to make them bigger
            make_big_components(r, b)
        } else {
            // If the components are big, we try to avoid joining together the big ones.
            make_small_components(r, b)
        }
    }

    /**
     * Go for whichever of the things has the smaller product
     */
    fn get_raw_red_prob_2(r0: usize, b0: usize, r1: usize, b1: usize) -> f64 {
        if r0 * r1 > b0 * b1 {
            0.0
        } else if r0 * r1 < b0 * b1 {
            1.0
        } else { // They are equal.
            0.5
        }
    }

    /**
     * If we have red components of sizes r0 and r1, and blues of b0 and b1 around an edge e,
     * what should the probability be that we make the edge red?
     * This should be symmetric, i.e. get_red_prob(x, y) + get_red_prob(y, x) = 1.
     * 
     * If the connection bools are false, that means that there are genuine different components
     * at both ends. If these variables are true, then the components might be empty.
     * 
     * Currently tries to avoid joining components together
     */
    fn get_red_prob(n: Order, r0: usize, b0: usize, r1: usize, b1: usize, are_red_joined: bool, are_blue_joined: bool) -> f64 {
        match (are_red_joined, are_blue_joined) {
            (false, false) => get_raw_red_prob_2(r0, b0, r1, b1),//get_raw_red_prob(n, r0 + r1 + 1, b0 + b1 + 1),
            (true, false) => get_raw_red_prob_2(r0.max(r1), b0, 1, b1),
            (false, true) => get_raw_red_prob_2(r0, b0.max(b1), r1, 1),
            (true, true) => get_raw_red_prob(n, r0.max(r1) + 1, b0.max(b1) + 1),
        }
    }

    /**
     * Returns an edge at the given vertex in the given class.
     */
    fn get_component_at_vertex(g: &Graph, class: &BigEdgeSet, components: &EdgeUnionFind, v: Vertex) -> Option<EdgeComponent> {
        for u in g.adj_list[v].iter() {
            let e = Edge::of_pair(*u, v);
            if class.has_edge(e) {
                return Some(components.get_component(e))
            }
        }
        None
    }

    /**
     * Returns the size of the component of class-colour at v.
     * Assumes that all these classes have already been nicely unioned.
     */
    fn get_comp_size(components: &EdgeUnionFind, e: Option<EdgeComponent>) -> usize {
        e.map_or(0, |e| components.get_component_size(e))
    }

    /**
     * Returns whether the two components are joined. If either component is None,
     * then we return true; we only return false if both components exist and are different.
     */
    fn are_components_joined(c1: Option<EdgeComponent>, c2: Option<EdgeComponent>) -> bool {
        c1.map_or(true, |c1| c2.map_or(true, |c2| c1 == c2))
    }

    /**
     * Merge all edges in class around e together into the same component.
     * This is called when e is added to this class.
     */
    fn merge_components_at_edge(g: &Graph, class: &BigEdgeSet, components: &mut EdgeUnionFind, e: Edge) {
        fn merge_components_at_vertex(g: &Graph, class: &BigEdgeSet, components: &mut EdgeUnionFind, e: Edge, u: Vertex) {
            for v in g.adj_list[u].iter() {
                let f = Edge::of_pair(u, *v);
                if e != f && class.has_edge(f) {
                    components.merge(e, f)
                }
            }
        }
        
        merge_components_at_vertex(g, class, components, e, e.fst());
        merge_components_at_vertex(g, class, components, e, e.snd());
    }

    let indexer = EdgeIndexer::new(&g.adj_list);
    let mut rng = thread_rng();
    let mut reds = BigEdgeSet::new(&indexer);
    let mut blues = BigEdgeSet::new(&indexer);
    let mut components = EdgeUnionFind::new(&g.adj_list);

    let mut shuffled_edges = g.iter_edges().collect::<Vec<Edge>>();
    shuffled_edges.shuffle(&mut rng);

    for e in shuffled_edges.iter() {
        let fst_red_comp = get_component_at_vertex(g, &reds, &components, e.fst()) ;
        let snd_red_comp = get_component_at_vertex(g, &reds, &components, e.snd());
        let fst_blue_comp = get_component_at_vertex(g, &blues, &components, e.fst());
        let snd_blue_comp = get_component_at_vertex(g, &blues, &components, e.snd());
        let r0 = get_comp_size(&components, fst_red_comp);
        let r1 = get_comp_size(&components, snd_red_comp);
        let b0 = get_comp_size(&components, fst_blue_comp);
        let b1 = get_comp_size(&components, snd_blue_comp);
        let are_red_joined = are_components_joined(fst_red_comp, snd_red_comp);
        let are_blue_joined = are_components_joined(fst_blue_comp, snd_blue_comp);
        let p = get_red_prob(g.n, r0, b0, r1, b1, are_red_joined, are_blue_joined);
        if rng.gen_bool(p) {
            reds.add_edge(*e);
            merge_components_at_edge(g, &reds, &mut components, *e);
        } else {
            blues.add_edge(*e);
            merge_components_at_edge(g, &blues, &mut components, *e);
        }
    }

    // println!("Component sizes: ");
    // let representatives = components.get_representatives();
    // for e in representatives.iter() {
    //     println!("{} : {}", e, components.get_component_size_from_edge(e))
    // }

    reds
}

fn get_colouring_component_graph(g: &Graph, reds: &BigEdgeSet) -> (Graph, VertexVec<usize>) {
    let mut comp_adj = vec![];
    let mut components = EdgeVec::new(&g.adj_list, None);
    let mut component_sizes = vec![];
    let mut next_component = Vertex::ZERO;
    for e in g.iter_edges() {
        if components[e].is_none() {
            components[e] = Some(next_component);
            let mut adj = BigVertexSet::new(g.n);
            let mut size = 1;
            // Starting from this edge, flood fill to find the colour component.
            let mut q = queue![];
            let _ = q.add(e.fst());
            let _ = q.add(e.snd());
            let mut found_verts = BigVertexSet::new(g.n);
            let colour = reds.has_edge(e);

            while let Ok(v) = q.remove() {
                for u in g.adj_list[v].iter() {
                    let f = Edge::of_pair(*u, v);
                    if let Some(other_comp) = components[f] {
                        if other_comp != next_component {
                            // We implicitly know that the edge is the other colour.
                            adj.add_vert(other_comp)
                        }
                    } else if reds.has_edge(f) == colour {
                        // This edge is in the component and hasn't been processed yet.
                        size += 1;
                        components[f] = Some(next_component);
                        if !found_verts.has_vert(*u) {
                            let _ = q.add(*u);
                            found_verts.add_vert(*u);
                        }
                    }
                }
            }
            comp_adj.push(adj.to_vec());
            component_sizes.push(size);
            next_component.incr_inplace()
        }
    }

    let adj = VertexVec::of_vec(comp_adj);
    let h = Graph::of_matrix_to_be_symmetrised(adj, crate::constructor::Constructor::Special);
    let sizes = VertexVec::of_vec(component_sizes);
    (h, sizes)
}

fn get_random_colouring(g: &Graph, colouring_type: usize) -> BigEdgeSet {
    match colouring_type {
        0 => get_uniform_random_colouring(g),
        1 => get_random_shell_colouring(g),
        2 => get_unfriendly_random_colouring(g),
        _ => panic!("Unknown colouring type!"),
    }
}

fn print_colouring(g: &Graph, reds: &BigEdgeSet) {
    for e in g.iter_edges() {
        println!("{} : {}", e, if reds.has_edge(e) { "Red" } else { "Blue" })
    }
}

/**
 * Just runs a direct check on all antipodal pairs and finds the furthers away pair.
 * Here, furthest of course means distance in terms of min colour swaps.
 * 
 * We apply this to a random colouring, which is produced by colouring in shells.
 */
pub fn min_antipode_distance(g: &Graph, colouring_type: usize) -> u32 {
    let mut reds = get_random_colouring(g, colouring_type);

    // let mut i = 0;
    let (mut h, mut component_sizes) = get_colouring_component_graph(g, &reds);
    while h.is_forest() {
        // if is_round_number(i as usize) {
        //     println!("Fail! {}", pretty_format_int(i));
        // }
        reds = get_random_colouring(g, colouring_type);
        // i += 1;
        (h, component_sizes) = get_colouring_component_graph(g, &reds);
    }

    let mut min_dist = u32::MAX;
    'test_verts: for v in g.n.div(2).iter_verts() {
        let dist = distance_to_antipode(g, v, &reds, false).min();
        min_dist = dist.min(min_dist);
        if dist == 0 {
            break 'test_verts
        }
    }

    if min_dist >= 1 {
        println!("Component graph:");
        h.print();
        println!("Component sizes:");
        for (v, size) in component_sizes.iter_enum() {
            println!("{} : {}", v, size)
        }
        print_colouring(g, &reds)
    }
    min_dist
}

pub fn average_antipode_distance(g: &Graph, colouring_type: usize) -> Rational {
    let mut reds = get_random_colouring(g, colouring_type);

    let (mut h, mut _component_sizes) = get_colouring_component_graph(g, &reds);
    while h.is_forest() {
        reds = get_random_colouring(g, colouring_type);
        (h, _component_sizes) = get_colouring_component_graph(g, &reds);
    }

    let mut total_dist = 0;
    for v in g.n.div(2).iter_verts() {
        let dist = distance_to_antipode(g, v, &reds, false).min();
        total_dist += dist
    }
    Rational::new_fraction(total_dist as usize, g.n.to_usize() / 2)
}