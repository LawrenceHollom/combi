use crate::entity::graph::*;
use crate::dossier::percolate::*;

use utilities::*;
use utilities::polynomial::*;
use utilities::vertex_tools::*;
use utilities::edge_tools::*;

use std::fmt;
use std::io::*;
use std::collections::HashMap;
use std::collections::HashSet;
use queues::*;

pub fn print_polynomials(g: &Graph) {
    let bunkbed = g.bunkbed();
    let percolator = Percolator::percolate(&bunkbed, false, true);

    for v in bunkbed.n.iter_verts() {
        println!("{}: {}", v, percolator.polys[v]);
    }

    println!("Differences (home - away):");
    for v in g.n.iter_verts() {
        let poly = percolator.polys[v].sub(&percolator.polys[v.incr_by_order(g.n)]);
        let unimodicity = poly.find_prob_unimode();
        println!("{}: {}", v, poly);
        let q_poly = poly.apply(&Polynomial::of_vec(&vec![1, -1])).with_var_name("q");
        println!(" q: {}", q_poly);
        match unimodicity {
            Modality::Unimodal(mode) => println!("    Unimodal with extremum at {}", mode),
            e => println!("    Not unimodal: {}", e),
        }
    }
}

pub fn are_all_diffs_unimodal(g: &Graph) -> bool {
    let bunkbed = g.bunkbed();
    let percolator = Percolator::percolate(&bunkbed, false, false);
    let mut is_good = true;

    'test_verts: for v in g.n.iter_verts() {
        let poly = percolator.polys[v].sub(&percolator.polys[v.incr_by_order(g.n)]);
        let unimodicity = poly.find_prob_unimode();
        use Modality::*;
        match unimodicity {
            Multimodal => {
                is_good = false;
                break 'test_verts;
            }
            Unimodal(_) | Nonmodal | Constant | Zero => (),
        }
    }

    is_good
}

pub fn print_distance_polynomials(g: &Graph) {
    let bunkbed = g.bunkbed();
    let percolator = Percolator::percolate(&bunkbed, true, true);

    println!("vert|dist|unim|home - away dist poly");
    println!("----|----|----|-----------------------------");
    for v in g.n.iter_verts() {
        for d in 0..(2*g.n.to_usize()) {
            let poly = percolator.dist_polys[v][d].sub(&percolator.dist_polys[v.incr_by_order(g.n)][d]);
            let unimodicity = poly.find_prob_unimode();
            println!(" {}  | {}  |{}| {}", v, d, unimodicity.four_letter_code(), poly);
        }
    }
}

pub fn simulate(g: &Graph) {
    let n = g.n.to_usize();
    let bunkbed = g.bunkbed();
    let samples = 100;
    let reps = 100000;
    let mut probs = vec![vec![0.0; n]; samples];
    for (i, prob) in probs.iter_mut().enumerate() {
        let p = i as f64 / samples as f64;
        for _j in 0..reps {
            let results = Percolator::empirically_percolate_once(&bunkbed, p);
            for v in 0..n {
                prob[v] += if results[v] { 1.0 } else { 0.0 } - if results[v + n] { 1.0 } else { 0.0 };
            }
        }
        print!("{:.3} ", p);
        for prob in prob.iter_mut() {
            *prob /= reps as f64;
            if *prob < 0.0 {
                print!("| {:.4} ", *prob);
            } else {
                print!("|  {:.4} ", *prob);
            }
        }
        println!();
    }
}

fn parallel_count(e: Edge, n: Order, config: &EdgeSet, indexer: &EdgeIndexer) -> usize {
    let neg = config.has_edge(e, indexer);
    let pos = config.has_edge(e.incr_by_order(n), indexer);
    (neg as usize) + (pos as usize)
}

fn flip_pair(e: Edge, n: Order, config: EdgeSet, indexer: &EdgeIndexer) -> EdgeSet {
    let e_plus = e.incr_by_order(n);
    let mut new_config = config.to_owned();
    new_config.flip_edge(e, indexer);
    new_config.flip_edge(e_plus, indexer);
    new_config
}

fn post(v: Vertex, n: Order) -> Edge {
    Edge::of_pair(v, v.incr_by_order(n))
}

#[derive(Eq, Hash, PartialEq, Clone)]
struct ConfSignature {
    posts_index: VertexSet,
    blanks_index: EdgeSet,
    doubles_index: EdgeSet,
}

impl ConfSignature {
    pub fn new(g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer) -> ConfSignature {
        let mut posts_index = VertexSet::new(g.n);
        let mut blanks_index = EdgeSet::new(indexer);
        let mut doubles_index = EdgeSet::new(indexer);
        for u in g.n.iter_verts() {
            let u_plus = u.incr_by_order(g.n);
            if config.has_edge(post(u, g.n), indexer) {
                posts_index.add_vert(u);
            }
            for v in u.incr().iter_from(g.n) {
                if g.adj[u][v] {
                    let e = Edge::of_pair(u, v);
                    let e_plus = Edge::of_pair(u_plus, v.incr_by_order(g.n));
                    if config.has_edge(e, indexer) && config.has_edge(e_plus, indexer) {
                        doubles_index.add_edge(e, indexer);
                    } else if !config.has_edge(e, indexer) && !config.has_edge(e_plus, indexer) {
                        blanks_index.add_edge(e, indexer);
                    }
                }
            }
        }
        ConfSignature { posts_index, blanks_index, doubles_index }
    }
}

impl fmt::Display for ConfSignature {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}, {}, {}]", self.posts_index, self.blanks_index, self.doubles_index)
    }
}

// This doesn't work, but does in some simple cases.
fn flip_postless_component(g: &Graph, config: &EdgeSet, u: Vertex, indexer: &EdgeIndexer) -> EdgeSet {
    let mut postless_component: VertexVec<bool> = VertexVec::new(g.n, &false);
    postless_component[u] = true;
    let mut q: Queue<Vertex> = queue![u];
    while q.size() > 0 {
        let v = q.remove().unwrap();
        for w in g.adj_list[v].iter() {
            let e = Edge::of_pair(v, *w);
            if !postless_component[*w] && *w != Vertex::ZERO && !config.has_edge(post(*w, g.n), indexer)
                    && parallel_count(e, g.n, config, indexer) != 0 {
                postless_component[*w] = true;
                let _ = q.add(*w);
            }
        }
    }
    let mut new_conf = config.to_owned();
    for v in g.n.iter_verts() {
        if postless_component[v] {
            for w in g.adj_list[v].iter() {
                if !postless_component[*w] || *w > v {
                    let e = Edge::of_pair(v, *w);
                    new_conf = flip_pair(e, g.n, new_conf, indexer);
                }
            }
        }
    }
    new_conf
}

fn print_title(g: &Graph) {
    for (u, v) in g.iter_pairs() {
        if g.adj[u][v] {
            print!("({}~{})", u, v);
        }
    }
    println!();
}

fn print_config(g: &Graph, config: &EdgeSet, indexer: &EdgeIndexer) {
    print_title(g);
    for (u, v) in g.iter_pairs() {
        if g.adj[u][v] {
            print!("( {} )", config.has_edge(Edge::of_pair(u, v).incr_by_order(g.n), indexer) as usize);
        }
    }
    println!();
    for (u, v) in g.iter_pairs() {
        if g.adj[u][v] {
            print!("( {} )", config.has_edge(Edge::of_pair(u, v), indexer) as usize);
        }
    }
    println!();
    for u in g.n.iter_verts() {
        print!("({})", u);
    }
    println!();
    for u in g.n.iter_verts() {
        print!("({})", config.has_edge(post(u, g.n), indexer) as usize);
    }
    println!();
}

fn print_problem_configs(g: &Graph, a: &EdgeSet, b: &EdgeSet, indexer: &EdgeIndexer) {
    println!("PROBLEM CONFIGS: ");
    println!("First config: ");
    print_config(g, a, indexer);
    println!("Second config:");
    print_config(g, b, indexer);
}

// Actually implement some given function to turn neg_confs into pos_confs.
fn attempt_injection(g: &Graph, u: Vertex, pos_confs: &[EdgeSet], neg_confs: &[EdgeSet], indexer: &EdgeIndexer,
        inj: &dyn Fn(&Graph, &EdgeSet, Vertex, &EdgeIndexer) -> EdgeSet) {
    println!("Attempting injection!");
    let mut flipped_confs: HashSet<EdgeSet> = HashSet::new();
    let mut pos_confs_set: HashSet<EdgeSet> = HashSet::new();
    for conf in pos_confs.iter() {
        if !pos_confs_set.insert(conf.to_owned()) {
            panic!("This shouldn't go wrong. You should write better code.");
        }
    }
    let mut failed = false;
    'test_confs: for conf in neg_confs.iter() {
        let sig = ConfSignature::new(g, conf, indexer);
        if sig.blanks_index.is_empty() && sig.doubles_index.is_empty() {
            let new_conf = inj(g, conf, u, indexer);
            if !flipped_confs.insert(new_conf.to_owned()) {
                print_problem_configs(g, conf, &new_conf, indexer);
                failed = true;
                println!("FAIL! Set already contained value, this is not an injection!");
                break 'test_confs;
            }
            if !pos_confs_set.contains(&new_conf) {
                print_problem_configs(g, conf, &new_conf, indexer);
                println!("FAIL! Missed pos_confs_set! This is not even a function!");
                failed = true;
                break 'test_confs;
            }
        }
    }
    if !failed {
        println!("SUCCESS! This is an injection.");
    }
}

// Give a domain and range with fixed signature, find possible injections
// and print them if there aren't too many.
// We will flip a set of edges dependent only on sig, which means that the
// function, if valid, is automatically an injection.
fn find_possible_injections(g: &Graph, sig: &ConfSignature, domain: &[EdgeSet], range: &[EdgeSet], indexer: &EdgeIndexer) {
    let mut injections: Vec<EdgeSet> = vec![];
    let mut range_set: HashSet<EdgeSet> = HashSet::new();
    for x in range.iter() {
        range_set.insert(x.to_owned());
    }

    // This is very inefficient for when sig has lots of doubles and blanks.
    for inj_code in g.iter_edge_sets() {
        // Test if inj_code meshes nicely with the doubles and blanks.
        if inj_code.inter(&sig.doubles_index).is_empty() && inj_code.inter(&sig.blanks_index).is_empty() {
            let mut is_inj_good = true;
            // Try flipping everything in domain by inj and see if it makes a subset of range.
            'test_inj: for x in domain.iter() {
                let mut new_conf = x.to_owned();
                for (u, v) in g.iter_pairs() {
                    let e = Edge::of_pair(u, v);
                    if g.adj[u][v] && inj_code.has_edge(e, indexer) {
                        // flip here.
                        new_conf = flip_pair(e, g.n, new_conf, indexer);
                    }
                }
                if !range_set.contains(&new_conf) {
                    is_inj_good = false;
                    break 'test_inj;
                }
            }

            if is_inj_good {
                injections.push(inj_code);
            }
        }
    }

    if injections.is_empty() {
        println!("Found no valid injections!");
        println!("DOMAIN:");
        for config in domain.iter() {
            print_config(g, config, indexer);
        }
        println!("RANGE:");
        for config in range.iter() {
            print_config(g, config, indexer);
        }
        panic!("This means there is no injection of the form we seek!");
    } else if injections.len() < 5 {
        println!("Possible injections: ");
        print_title(g);
        for inj in injections.iter() {
            for (u, v) in g.iter_pairs() {
                if g.adj[u][v] {
                    print!("( {} )", inj.has_edge(Edge::of_pair(u, v), indexer));
                }
            }
            println!();
        }
        println!("Sample config in domain:");
        print_config(g, &domain[0], indexer);
    } else {
        println!("{} injections found; not printing them all.", injections.len());
    }
}

// We have a big list of positive and negative configs we care about.
// This function processes them to try and learn about possible injections.
fn process_bunkbed_configs(g: &Graph, u: Vertex, pos_confs: &[EdgeSet], neg_confs: &[EdgeSet], indexer: &EdgeIndexer) {
    let pos_signatures: Vec<(EdgeSet, ConfSignature)> = 
        pos_confs.iter()
            .map(|conf| (conf.to_owned(), ConfSignature::new(g, conf, indexer)))
            .collect();
    let neg_signatures: Vec<(EdgeSet, ConfSignature)> =
        neg_confs.iter()
            .map(|conf| (conf.to_owned(), ConfSignature::new(g, conf, indexer)))
            .collect();
    let mut counts: HashMap<ConfSignature, (Vec<EdgeSet>, Vec<EdgeSet>)> = HashMap::new();
    
    for (conf, sig) in pos_signatures.iter() {
        match counts.remove(sig) {
            Some((mut poss, negs)) => {
                poss.push(conf.to_owned());
                counts.insert(sig.to_owned(), (poss, negs))
            }
            None => counts.insert(sig.to_owned(), (vec![conf.to_owned()], vec![])),
        };
    }
    
    for (conf, sig) in neg_signatures.iter() {
        match counts.remove(sig) {
            Some((poss, mut negs)) => {
                negs.push(conf.to_owned());
                counts.insert(sig.to_owned(), (poss, negs))
            }
            None => counts.insert(sig.to_owned(), (vec![], vec![conf.to_owned()])),
        };
    }

    let mut mappables: Vec<(EdgeSet, EdgeSet)> = vec![];
    println!("counts:");
    for (sig, (x, y)) in counts.iter() {
        if sig.blanks_index.is_empty() && sig.doubles_index.is_empty() {
            println!("  {}: ({}, {}); {}", sig, x.len(), y.len(), x.len() - y.len());
            // Want to map y into x.
            if x.len() < y.len() {
                panic!("FOUND SOMETHING WHICH CANNOT BE NAIVELY INJECTED!");
            }
            if x.len() == 1 && y.len() == 1 && flip_postless_component(g, &y[0], u, indexer) != x[0] {
                mappables.push((x[0].to_owned(), y[0].to_owned()));
            }
            if !y.is_empty() {
                find_possible_injections(g, sig, y, x, indexer);
            }
        }
    }

    println!("Mappables which flip_postless_component fails on: ");
    for (x, y) in mappables.iter() {
        println!("Pair:");    
        for (u, v) in g.iter_pairs() {
            if g.adj[u][v] {
                let e = Edge::of_pair(u, v);
                if x.has_edge(e, indexer) ^ y.has_edge(e, indexer) {
                    // The edge has been flipped between the configs
                    print!("( F )");
                } else {
                    print!("(   )");
                }
            }
        }
        println!();
        print_config(g, y, indexer);
        println!();
    }
    println!("There were {} directly mappable pairs!", mappables.len());
    attempt_injection(g, u, pos_confs, neg_confs, indexer, &flip_postless_component)
}

// Computes which configurations have u-, u+ with different connectivities
// and then prints about them.
pub fn interesting_configurations(g: &Graph, u: Vertex, print_size: Option<usize>) {
    let g_indexer = EdgeIndexer::new(&g.adj_list);
    let bunkbed = g.bunkbed();
    let bunkbed_indexer = EdgeIndexer::new(&bunkbed.adj_list);
    let m = bunkbed.size();
    if m >= 63 {
        panic!("Too many edges; everything will go to shit!");
    }
    let mut num_interesting = 0;
    let mut num_positive = 0;
    let mut edge_count: Vec<i32> = vec![0; m];
    let mut positive_edge_count: Vec<i32> = vec![0; m];
    let mut negative_edge_count: Vec<i32> = vec![0; m];
    let mut positive_unflippable_count: Vec<i32> = vec![0; m];
    let mut negative_unflippable_count: Vec<i32> = vec![0; m];
    let mut positive_unflippable_configs: Vec<EdgeSet> = vec![];
    let mut negative_unflippable_configs: Vec<EdgeSet> = vec![];
    
    for (i, j) in bunkbed.n.iter_pairs() {
        if bunkbed.adj[i][j] {
            print!("{} ~ {},", i, j);
        }
    }
    println!();
    let iter_max = 2_u128.pow(m as u32);
    let mut percentage = 0;
    let mut index = 0;
    for config in bunkbed.iter_edge_sets() {
        // config encodes which of the edges are/are not present.
        let connected = bunkbed.flood_fill_dist(Vertex::ZERO);
        index += 1;
        if m >= 20 && (100 * index) / iter_max > percentage {
            percentage += 1;
            print!("{}% ", percentage);
            std::io::stdout().flush().unwrap();
        }
        fn flatten (x: Option<u32>) -> i32 {
            if x.is_none() { 0 } else { 1 }
        }
        let sign = flatten(connected[u]) - flatten(connected[u.incr_by_order(g.n)]);
        if sign != 0 {
            num_interesting += 1;
            if sign == 1 {
                num_positive += 1;
            }
            let num_open_edges = config.size();
            edge_count[num_open_edges] += sign;
            if sign == 1 {
                positive_edge_count[num_open_edges] += 1;
            } else {
                negative_edge_count[num_open_edges] += 1;
            }
            // now test whether it is unflippable.
            let mut flood = VertexVec::new(g.n, &None);
            if !config.has_edge(post(Vertex::ZERO, g.n), &bunkbed_indexer) {
                let filter = g.n.iter_verts().map(|v| config.has_edge(post(v, g.n), &bunkbed_indexer)).collect();
                flood = g.flood_fill(Vertex::ZERO, None, Some(&filter));
            }
            if u == Vertex::ZERO || flood[u].is_some() {
                if sign == 1 {
                    positive_unflippable_count[num_open_edges] += 1;
                } else {
                    negative_unflippable_count[num_open_edges] += 1;
                }
                if print_size.map_or(true, |x| x == num_open_edges) {
                    if sign == 1 {
                        positive_unflippable_configs.push(config.to_owned());
                    } else {
                        negative_unflippable_configs.push(config.to_owned());
                    }
                }
                if print_size.map_or(m < 20, |x| x == num_open_edges) {
                    for (i, j) in bunkbed.n.iter_pairs() {
                        if bunkbed.adj[i][j] {
                            print!("{},", if config.has_edge(Edge::of_pair(i, j), &bunkbed_indexer) { 1 } else { 0 });
                        }
                    }
                    println!("{}", sign);
                }
            }
        }
    }
    println!("Bunkbed num interesting: {}", num_interesting);
    println!("num plus: {}, num minus: {}, num total: {}", num_positive, 
        num_interesting - num_positive, iter_max);
    println!("Edge count vector:   {:?}", edge_count);
    println!("Positive edge count: {:?}", positive_edge_count);
    println!("Negative edge count: {:?}", negative_edge_count);
    println!("Positive unflippable count: {:?}", positive_unflippable_count);
    println!("Negative unflippable count: {:?}", negative_unflippable_count);
    process_bunkbed_configs(g, u, &positive_unflippable_configs, &negative_unflippable_configs, &g_indexer);
}

pub fn compute_problem_cuts(g: &Graph, u: Vertex) {
    fn is_one_connected(g: &Graph, one_pick: &VertexVec<bool>, u: Vertex) -> bool {
        // flood fill
        let prev = g.flood_fill(Vertex::ZERO, Some(u), Some(one_pick));
        prev[u].is_some()
    }
    fn cut_size(g: &Graph, picked: &VertexVec<bool>) -> usize {
        let mut size = 0;
        for (u, adj) in g.adj_list.iter_enum() {
            for v in adj.iter() {
                if *v > u {
                    if picked[u] != picked[*v] {
                        size += 1;
                    }
                    if picked[u.incr_by_order(g.n)] != picked[v.incr_by_order(g.n)] {
                        size += 1;
                    }
                }
            }
            if picked[u] != picked[u.incr_by_order(g.n)] {
                size += 1;
            }
        }
        size
    }

    let mut flat_poly = Polynomial::new();
    let mut cross_poly = Polynomial::new();

    for subset in 0..pow(2, 2*g.n.to_usize() as u64) {
        let mut picked = VertexVec::new(g.n.times(2), &false);
        let mut sta = subset;
        let mut cut_order = 0;
        let mut cut = vec![];
        for (v, is_picked) in picked.iter_mut_enum() {
            if sta % 2 == 1 {
                *is_picked = true;
                cut.push(v);
                cut_order += 1;
            }
            sta /= 2;
        }
        let one_pick: VertexVec<bool> = g.n.iter_verts().map(|v| picked[v] != picked[v.incr_by_order(g.n)]).collect();
        // picked is now the set of vertices in the prospective cut
        if g.n.at_least(cut_order) && one_pick[Vertex::ZERO] && one_pick[u] && picked[Vertex::ZERO] {
            if picked[Vertex::ZERO] == picked[u] && is_one_connected(g, &one_pick, u) {
                let size = cut_size(g, &picked);
                flat_poly.add_inplace(&Polynomial::monomial(1, size));
                //println!("FLAT:  {:?}: {}", cut, size);
            }
            if picked[Vertex::ZERO] != picked[u] && is_one_connected(g, &one_pick, u) {
                let size = cut_size(g, &picked);
                cross_poly.add_inplace(&Polynomial::monomial(1, size));
                //println!("CROSS: {:?}: {}", cut, size);
            }
        }
    }

    println!("FLAT:  {}", flat_poly.with_var_name("q"));
    println!("CROSS: {}", cross_poly.with_var_name("q"));
    println!("DIFF:  {}", flat_poly.sub(&cross_poly).with_var_name("q"));
}