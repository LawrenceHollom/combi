use std::{io::{self, Write}, ops::{Index, IndexMut}, u32};

use utilities::{edge_tools::*, vertex_tools::*};

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
fn distance_to_antipode(g: &Graph, indexer: &EdgeIndexer, v: Vertex, reds: EdgeSet, print_debug: bool) -> Distances {
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
        if reds.has_edge(e, indexer) {
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
            let e_col = Colour::of_bool(reds.has_edge(e, indexer));
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
 * Prints the colouring of g in a human readable manner.
 */
fn print_colouring(indexer: &EdgeIndexer, reds: EdgeSet) {
    for e in indexer.iter_edges() {
        let col = Colour::of_bool(reds.has_edge(*e, indexer));
        println!("({}, {}): {:?}", e.fst().to_binary_string(), e.snd().to_binary_string(), col);
    }
}

/**
 * Is this colouring a representative of its symmetry class?
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

        let choosable = is_choosable(g, &indexer, reds);

        num_colourings_after_reduction += 1;
        let mut total_antipode_distance = 0;
        let mut total_alternating_distance = 0;
        for v in g.n.div(2).iter_verts() {
            let dists = distance_to_antipode(g, &indexer, v, reds, false);
            total_antipode_distance += dists.min();
            total_alternating_distance += dists.max_alternating();
        }
        if total_antipode_distance > max_dist {
            num_extremal = 1;
            max_dist = total_antipode_distance;
            println!("New maximum!");
            print_colouring(&indexer, reds);
            println!("Total antipode distance = {}\n", total_antipode_distance);

            //distance_to_antipode(g, &indexer, Vertex::ZERO, reds, true);
        } else if total_antipode_distance == max_dist {
            num_extremal += 1;
        }

        if choosable && total_alternating_distance > max_choosable_dist && total_antipode_distance > 4 {
            // We only care about choosable things if they don't have very good antipode stuff to start with.
            max_choosable_dist = total_alternating_distance;

            println!("New choosable maximum!");
            print_colouring(&indexer, reds);
            println!("Total alternating distance = {} (antipode = {})\n", total_alternating_distance, total_antipode_distance);
        }
        if !choosable && total_antipode_distance > max_unchoosable_dist {
            max_unchoosable_dist = total_antipode_distance;
        }
        let is_bad = total_antipode_distance > 6 && (!choosable || total_alternating_distance > 12);
        if is_bad {
            num_bad += 1;
            println!("Found {} bad ones!", num_bad);
            print_colouring(&indexer, reds);
        }
    }
    println!("\nNum extremal configurations: {}", num_extremal);
    println!("There are {} colourings in total, of which {} survived reduction", num_colourings, num_colourings_after_reduction);
    println!("Max choosable dist = {}", max_choosable_dist);
    println!("Max unchoosable dist = {}", max_unchoosable_dist);
}