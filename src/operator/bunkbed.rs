use crate::graph::*;
use crate::operator::percolate::*;

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
    let n = bunkbed.n.to_usize();
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
    let n = g.n.to_usize();

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

// encode an edge as a single number
fn encode(x: usize, y: usize, n: usize) -> usize {
    if x < y {
        x * n + y
    } else {
        y * n + x
    }
}

fn parallel_count(e: Edge, n: Order, config: EdgeSet) -> usize {
    let neg = config.has_edge(e);
    let pos = config.has_edge(e.incr_by_order(n));
    (neg as usize) + (pos as usize)
}

fn flip_pair(e: Edge, n: Order, config: EdgeSet) -> EdgeSet {
    let neg = config.has_edge(e);
    let pos = config.has_edge(e.incr_by_order(n));
    if neg ^ pos {
        if neg {
            config - (1 << indexer[encode(x, y, n)]) + (1 << indexer[encode(x + n / 2, y + n / 2, n)])
        } else {
            config + (1 << indexer[encode(x, y, n)]) - (1 << indexer[encode(x + n / 2, y + n / 2, n)])
        }
    } else {
        config
    }
}

fn mask_config(x: usize, y: usize, n: usize, indexer: &Vec<usize>, config: u64, is_present: bool) -> u64 {
    (if is_present { config } else { !config }) & (1 << indexer[encode(x, y, n)])
}

fn post(v: Vertex, n: Order) -> Edge {
    Edge::of_pair(v, v.incr_by_order(n))
}

#[derive(Eq, Hash, PartialEq, Clone, Copy)]
struct ConfSignature {
    posts_index: VertexSet,
    blanks_index: EdgeSet,
    doubles_index: EdgeSet,
}

impl ConfSignature {
    pub fn new(g: &Graph, config: EdgeSet) -> ConfSignature {
        let mut posts_index = VertexSet::new();
        let mut blanks_index = EdgeSet::new(&g.adj_list);;
        let mut doubles_index = EdgeSet::new(&g.adj_list);;
        for u in g.n.iter_verts() {
            let u_plus = u.incr_by_order(g.n);
            if config[post(u, g.n)] {
                posts_index.add_vert(u);
            }
            for v in u.incr().iter_from(g.n) {
                if g.adj[u][v] {
                    let e = Edge::of_pair(u, v);
                    let e_plus = Edge::of_pair(u_plus, v.incr_by_order(g.n));
                    let v_plus = v.incr_by_order(g.n);
                    if config[e] && config[e_plus] {
                        doubles_index.add_edge(e);
                    } else if !config[e] && !config[e_plus] {
                        blanks_index.add_edge(e);
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
fn flip_postless_component(g: &Graph, config: EdgeSet, u: Vertex) -> EdgeSet {
    let mut postless_component: VertexVec<bool> = VertexVec::new(g.n, &false);
    postless_component[u] = true;
    let mut q: Queue<Vertex> = queue![u];
    while q.size() > 0 {
        let v = q.remove().unwrap();
        for w in g.adj_list[v].iter() {
            let e = Edge::of_pair(v, *w);
            if !postless_component[*w] && *w != Vertex::ZERO && !config.has_edge(post(*w, g.n))
                    && parallel_count(e, g.n, config) != 0 {
                postless_component[*w] = true;
                let _ = q.add(*w);
            }
        }
    }
    let mut new_conf = config;
    for v in g.n.iter_verts() {
        if postless_component[v] {
            for w in g.adj_list[v].iter() {
                if !postless_component[*w] || *w > v {
                    let e = Edge::of_pair(v, *w);
                    new_conf = flip_pair(e, g.n, new_conf);
                }
            }
        }
    }
    new_conf
}

fn print_title(g: &Graph) {
    for (u, v) in g.n.iter_pairs() {
        if g.adj[u][v] {
            print!("({}~{})", u, v);
        }
    }
    println!();
}

fn print_config(g: &Graph, config: EdgeSet) {
    let g_n = g.n.to_usize();
    let n = 2 * g_n;
    print_title(g);
    for (u, v) in g.n.iter_pairs() {
        if g.adj[u][v] {
            print!("( {} )", config[Edge::of_pair(u, v).incr_by_order(g.n)] as usize);
        }
    }
    println!();
    for (u, v) in g.n.iter_pairs() {
        if g.adj[u][v] {
            print!("( {} )", config[Edge::of_pair(u, v)] as usize);
        }
    }
    println!();
    for u in g.n.iter_verts() {
        print!("({})", u);
    }
    println!();
    for u in g.n.iter_verts() {
        print!("({})", config[post(u, g.n)] as usize);
    }
    println!();
}

fn print_problem_configs(g: &Graph, a: EdgeSet, b: EdgeSet) {
    println!("PROBLEM CONFIGS: ");
    println!("First config: ");
    print_config(g, a);
    println!("Second config:");
    print_config(g, b);
}

// Actually implement some given function to turn neg_confs into pos_confs.
fn attempt_injection(g: &Graph, u: Vertex, pos_confs: &Vec<EdgeSet>, neg_confs: &Vec<EdgeSet>,
        inj: &dyn Fn(&Graph, Vertex, EdgeSet) -> EdgeSet) {
    println!("Attempting injection!");
    let mut flipped_confs: HashSet<EdgeSet> = HashSet::new();
    let mut pos_confs_set: HashSet<EdgeSet> = HashSet::new();
    for conf in pos_confs.iter() {
        if !pos_confs_set.insert(*conf) {
            panic!("This shouldn't go wrong. You should write better code.");
        }
    }
    let mut failed = false;
    'test_confs: for conf in neg_confs.iter() {
        let sig = ConfSignature::new(g, *conf);
        if sig.blanks_index == 0 && sig.doubles_index == 0 {
            let new_conf = inj(g, *conf, u, indexer);
            if !flipped_confs.insert(new_conf) {
                print_problem_configs(g, *conf, new_conf);
                failed = true;
                println!("FAIL! Set already contained value, this is not an injection!");
                break 'test_confs;
            }
            if !pos_confs_set.contains(&new_conf) {
                print_problem_configs(g, *conf, new_conf);
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
fn find_possible_injections(g: &Graph, sig: &ConfSignature, domain: &Vec<EdgeSet>, range: &Vec<EdgeSet>) {
    let mut injections: Vec<u64> = vec![];
    let mut range_set: HashSet<u64> = HashSet::new();
    for x in range.iter() {
        range_set.insert(*x);
    }

    // This is very inefficient for when sig has lots of doubles and blanks.
    // This scores "big yuck" on code quality.
    for inj_code_index in 0..2_u64.pow(g.size() as u32) {
        let mut inj_code = 0;
        let mut edge_index = 0;
        for (u, v) in g.n.iter_pairs() {
            if g.adj[u][v] {
                if (inj_code_index >> edge_index) % 2 == 1 {
                    inj_code += 1 << indexer[encode(u, v, n)];
                }
                edge_index += 1;
            }
        }
        // Test if inj_code meshes nicely with the doubles and blanks.
        if (inj_code & sig.doubles_index == 0) && (inj_code & sig.blanks_index == 0) {
            let mut is_inj_good = true;
            // Try flipping everything in domain by inj and see if it makes a subset of range.
            'test_inj: for x in domain.iter() {
                let mut new_conf = *x;
                for u in 0..(g_n - 1) {
                    for v in (u + 1)..g_n {
                        if g.adj[u][v] && is_present(u, v, n, indexer, inj_code){
                            // flip here.
                            new_conf = flip_pair(u, v, n, indexer, new_conf);
                        }
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

    if injections.len() == 0 {
        println!("Found no valid injections!");
        println!("DOMAIN:");
        for config in domain.iter() {
            print_config(g, *config, indexer);
        }
        println!("RANGE:");
        for config in range.iter() {
            print_config(g, *config, indexer);
        }
        panic!("This means there is no injection of the form we seek!");
    } else if injections.len() < 5 {
        println!("Possible injections: ");
        print_title(g);
        for inj in injections.iter() {
            for u in 0..(g_n - 1) {
                for v in (u + 1)..g_n {
                    if g.adj[u][v] {
                        print!("( {} )", (*inj >> indexer[encode(u, v, n)]) % 2);
                    }
                }
            }
            println!();
        }
        println!("Sample config in domain:");
        print_config(g, domain[0], indexer);
    } else {
        println!("{} injections found; not printing them all.", injections.len());
    }
}

// We have a big list of positive and negative configs we care about.
// This function processes them to try and learn about possible injections.
fn process_bunkbed_configs(g: &Graph, u: usize, pos_confs: &Vec<u64>, neg_confs: &Vec<u64>, indexer: &Vec<usize>) {
    // Good coding practice is my passion.
    let pos_signatures: Vec<(u64, ConfSignature)> = 
        pos_confs.iter()
            .map(|conf| (*conf, ConfSignature::new(g, *conf, indexer)))
            .collect();
    let neg_signatures: Vec<(u64, ConfSignature)> =
        neg_confs.iter()
            .map(|conf| (*conf, ConfSignature::new(g, *conf, indexer)))
            .collect();
    let mut counts: HashMap<ConfSignature, (Vec<u64>, Vec<u64>)> = HashMap::new();
    
    for (conf, sig) in pos_signatures.iter() {
        match counts.remove(sig) {
            Some((mut poss, negs)) => {
                poss.push(*conf);
                counts.insert(*sig, (poss, negs))
            }
            None => counts.insert(*sig, (vec![*conf], vec![])),
        };
    }
    
    for (conf, sig) in neg_signatures.iter() {
        match counts.remove(sig) {
            Some((poss, mut negs)) => {
                negs.push(*conf);
                counts.insert(*sig, (poss, negs))
            }
            None => counts.insert(*sig, (vec![], vec![*conf])),
        };
    }

    let mut mappables: Vec<(u64, u64)> = vec![];
    println!("counts:");
    for (sig, (x, y)) in counts.iter() {
        if sig.blanks_index == 0 && sig.doubles_index == 0 {
            println!("  {}: ({}, {}); {}", sig, x.len(), y.len(), x.len() - y.len());
            // Want to map y into x.
            if x.len() < y.len() {
                panic!("FOUND SOMETHING WHICH CANNOT BE NAIVELY INJECTED!");
            }
            if x.len() == 1 && y.len() == 1 && flip_postless_component(g, y[0], u, indexer) != x[0] {
                mappables.push((x[0], y[0]));
            }
            if y.len() > 0 {
                find_possible_injections(g, sig, y, x, indexer);
            }
        }
    }

    println!("Mappables which flip_postless_component fails on: ");
    let g_n = g.n.to_usize();
    let n = g_n * 2;
    for (x, y) in mappables.iter() {
        println!("Pair:");    
        for u in 0..(g_n - 1) {
            for v in (u + 1)..g_n {
                if g.adj[u][v] {
                    if is_present(u, v, n, indexer, *x) ^ is_present(u, v, n, indexer, *y) {
                        // The edge has been flipped between the configs
                        print!("( F )");
                    } else {
                        print!("(   )");
                    }
                }
            }
        }
        println!();
        print_config(g, *y, indexer);
        println!();
    }
    println!("There were {} directly mappable pairs!", mappables.len());
    attempt_injection(g, u, pos_confs, neg_confs, indexer, &flip_postless_component)
}

// Computes which configurations have u-, u+ with different connectivities
// and then prints about them.
pub fn interesting_configurations(g: &Graph, u: usize, print_size: Option<usize>) {
    let g_n = g.n.to_usize();
    let bunkbed = g.bunkbed();
    let n = bunkbed.n.to_usize();
    let m = bunkbed.size();
    if m >= 63 {
        panic!("Too many edges; everything will go to shit!");
    }
    let mut indexer: Vec<usize> = vec![0; n * n];
    let mut index = 0;
    for (x, adj) in bunkbed.adj_list.iter().enumerate() {
        for y in adj.iter() {
            if *y > x {
                indexer[encode(x, *y, n)] = index;
                index += 1;
            }
        }
    }
    let mut num_interesting = 0;
    let mut num_positive = 0;
    let mut edge_count: Vec<i32> = vec![0; m];
    let mut positive_edge_count: Vec<i32> = vec![0; m];
    let mut negative_edge_count: Vec<i32> = vec![0; m];
    let mut positive_unflippable_count: Vec<i32> = vec![0; m];
    let mut negative_unflippable_count: Vec<i32> = vec![0; m];
    let mut positive_unflippable_configs: Vec<u64> = vec![];
    let mut negative_unflippable_configs: Vec<u64> = vec![];
    
    for i in 0..(n-1) {
        for j in (i+1)..n {
            if bunkbed.adj[i][j] {
                print!("{} ~ {},", i, j);
            }
        }
    }
    println!();
    let iter_max = 2_u64.pow(m as u32);
    let mut percentage = 0;
    for config in 0..iter_max {
        // config encodes which of the edges are/are not present.
        let mut connected = vec![n; n];
        let mut q: Queue<usize> = queue![];
        connected[0] = 0;
        let _ = q.add(0);
        while q.size() > 0 {
            let node = q.remove().unwrap();
            for v in bunkbed.adj_list[node].iter() {
                if is_present(node, *v, n, &indexer, config) && connected[*v] == n {
                    connected[*v] = connected[node] + 1;
                    let _ = q.add(*v);
                }
            }
        }
        if m >= 20 && (100 * config) / iter_max > percentage {
            percentage += 1;
            print!("{}% ", percentage);
            std::io::stdout().flush().unwrap();
        }
        fn flatten (x: usize, n: usize) -> i32 {
            if x == n { 0 } else { 1 }
        }
        let sign = flatten(connected[u], n) - flatten(connected[u + g_n], n);
        if sign != 0 {
            num_interesting += 1;
            if sign == 1 {
                num_positive += 1;
            }
            let mut num_open_edges = 0;
            let mut sta = config;
            while sta > 0 {
                if sta % 2 == 1 {
                    num_open_edges += 1;
                }
                sta /= 2;
            }
            edge_count[num_open_edges] += sign;
            if sign == 1 {
                positive_edge_count[num_open_edges] += 1;
            } else {
                negative_edge_count[num_open_edges] += 1;
            }
            // now test whether it is unflippable.
            q = queue![];
            let mut g_connected = vec![false; g_n];
            g_connected[0] = true;
            if !is_present(0, g_n, n, &indexer, config) {
                let _ = q.add(0);
            }
            while q.size() > 0 {
                let node = q.remove().unwrap();
                for v in g.adj_list[node].iter() {
                    if !is_present(*v, g_n + *v, n, &indexer, config) && !g_connected[*v] {
                        g_connected[*v] = true;
                        let _ = q.add(*v);
                    }
                }
            }
            if g_connected[u] {
                if sign == 1 {
                    positive_unflippable_count[num_open_edges] += 1;
                } else {
                    negative_unflippable_count[num_open_edges] += 1;
                }
                if print_size.map_or(true, |x| x == num_open_edges) {
                    if sign == 1 {
                        positive_unflippable_configs.push(config);
                    } else {
                        negative_unflippable_configs.push(config);
                    }
                }
                if print_size.map_or(m < 20, |x| x == num_open_edges) {
                    for i in 0..(n-1) {
                        for j in (i+1)..n {
                            if bunkbed.adj[i][j] {
                                print!("{},", if is_present(i, j, n, &indexer, config) { 1 } else { 0 });
                            }
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
    process_bunkbed_configs(g, u, &positive_unflippable_configs, &negative_unflippable_configs, &indexer);
}

pub fn compute_problem_cuts(g: &Graph, u: usize) {
    let n = g.n.to_usize();

    fn is_one_connected(g: &Graph, one_pick: &[bool], u: usize, n: usize) -> bool {
        // flood fill
        let mut visited = vec![false; n];
        let mut q = queue![0];
        'flood: loop {
            match q.remove() {
                Ok(node) => {
                    for v in g.adj_list[node].iter() {
                        if !visited[*v] && one_pick[*v] {
                            visited[*v] = true;
                            let _ = q.add(*v);
                            if *v == u {
                                break 'flood;
                            }
                        }
                    }
                }
                Err(_e) => break 'flood
            }
        }
        visited[u]
    }

    fn cut_size(g: &Graph, picked: &[bool], n: usize) -> usize {
        let mut size = 0;
        for (u, adj) in g.adj_list.iter().enumerate() {
            for v in adj.iter() {
                if *v > u {
                    if picked[u] != picked[*v] {
                        size += 1;
                    }
                    if picked[n + u] != picked[n + *v] {
                        size += 1;
                    }
                }
            }
            if picked[u] != picked[u+n] {
                size += 1;
            }
        }
        size
    }

    let mut flat_poly = Polynomial::new();
    let mut cross_poly = Polynomial::new();

    for subset in 0..pow(2, 2*n as u64) {
        let mut picked = vec![false; 2*n];
        let mut sta = subset;
        let mut order = 0;
        let mut cut = vec![];
        for (v, is_picked) in picked.iter_mut().enumerate() {
            if sta % 2 == 1 {
                *is_picked = true;
                cut.push(v);
                order += 1;
            }
            sta /= 2;
        }
        let mut one_pick = vec![false; n];
        for (i, pick) in one_pick.iter_mut().enumerate() {
            *pick = picked[i] != picked[i + n];
        }
        // picked is now the set of vertices in the prospective cut
        if order <= n && one_pick[0] && one_pick[u] && picked[0] {
            if picked[0] == picked[u] && is_one_connected(g, &one_pick, u, n) {
                let size = cut_size(g, &picked, n);
                flat_poly.add_inplace(&Polynomial::monomial(1, size));
                //println!("FLAT:  {:?}: {}", cut, size);
            }
            if picked[0] != picked[u] && is_one_connected(g, &one_pick, u, n) {
                let size = cut_size(g, &picked, n);
                cross_poly.add_inplace(&Polynomial::monomial(1, size));
                //println!("CROSS: {:?}: {}", cut, size);
            }
        }
    }

    println!("FLAT:  {}", flat_poly.with_var_name("q"));
    println!("CROSS: {}", cross_poly.with_var_name("q"));
    println!("DIFF:  {}", flat_poly.sub(&cross_poly).with_var_name("q"));
}