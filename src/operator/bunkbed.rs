use crate::graph::*;
use crate::operator::percolate::*;

use utilities::*;
use utilities::polynomial::*;

use queues::*;

pub fn print_polynomials(g: &Graph) {
    let bunkbed = g.bunkbed();
    let percolator = Percolator::percolate(&bunkbed, false, true);
    let n = bunkbed.n.to_usize();

    for v in 0..n {
        println!("{}: {}", v, percolator.polys[v]);
    }

    println!("Differences (home - away):");
    for v in 0..(n/2) {
        let poly = percolator.polys[v].sub(&percolator.polys[v + (n/2)]);
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

    'test_verts: for v in 0..(n/2) {
        let poly = percolator.polys[v].sub(&percolator.polys[v + (n/2)]);
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
    for v in 0..n {
        for d in 0..(2*n) {
            let poly = percolator.dist_polys[v][d].sub(&percolator.dist_polys[v+n][d]);
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

    /*
    let mut second_diff = Polynomial::new();
    //Dumb second order computation
    for sub1 in 0..pow(2, 2*n as u64) {
        let mut picked1 = vec![false; 2*n];
        let mut sta = sub1;
        let mut cut1 = vec![];
        for v in 0..2*n {
            if sta % 2 == 1 {
                picked1[v] = true;
                cut1.push(v);
            }
            sta /= 2;
        }

        for sub2 in (sub1+1)..pow(2, 2*n as u64) {
            let mut picked2 = vec![false; 2*n];
            let mut sta = sub2;
            let mut cut2 = vec![];
            for v in 0..2*n {
                if sta % 2 == 1 {
                    picked2[v] = true;
                    cut2.push(v);
                }
                sta /= 2;
            }

            let mut size = 0;
            for (w, adj) in g.adj_list.iter().enumerate() {
                for v in adj.iter() {
                    if *v > w {
                        if picked1[w] != picked1[*v] || picked2[w] != picked2[*v] {
                            size += 1;
                        }
                        if picked1[n + w] != picked1[n + *v] || picked2[n + w] != picked2[n + *v] {
                            size += 1;
                        }
                    }
                }
                if picked1[w] != picked1[n + w] || picked2[w] != picked2[n + w] {
                    size += 1;
                }
            }
            if !picked1[0] && !picked2[0] && picked1[u] && picked2[u] { // cross
                second_diff.sub_inplace(&Polynomial::monomial(1, size));
            }
            if !picked1[0] && !picked2[0] && picked1[n + u] && picked2[n + u] { // flat
                second_diff.add_inplace(&Polynomial::monomial(1, size));
            }
        }
    }

    println!("SECOND DIFF: {}", second_diff.with_var_name("q"));
    println!("DIFF - SECOND_DIFF: {}", flat_poly.sub(&cross_poly).sub(&second_diff).with_var_name("q"))
    */
}