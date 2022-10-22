use crate::graph::*;
use crate::operator::percolate::*;

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
        match unimodicity {
            Ok(extremum) => println!("    Unimodal with extremum at {}", extremum),
            Err(e) => println!("    Not unimodal: {}", e),
        }
    }
}