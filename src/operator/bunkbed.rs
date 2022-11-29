use crate::graph::*;
use crate::operator::percolate::*;

use utilities::polynomial::*;

pub fn print_polynomials(g: &Graph) {
    let bunkbed = g.bunkbed();
    let percolator = Percolator::percolate(&bunkbed);
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
            Ok(extremum) => println!("    Unimodal with extremum at {}", extremum),
            Err(e) => println!("    Not unimodal: {}", e),
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