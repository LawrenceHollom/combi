use std::{collections::{HashMap, HashSet}, cmp::Ordering};

use super::reduced_equivalence_relation::*;
use super::graph_and_metadata::*;
use super::equivalence_counts::*;

const BIG_CUTOFF: f64 = 5.999;

pub struct Data {
	permutations: HashMap<ReducedEquivalenceRelation, Vec<ReducedEquivalenceRelation>>,
	rel_counts: HashMap<ReducedEquivalenceRelation, usize>,
	max_ratios: HashMap<(ReducedEquivalenceRelation, ReducedEquivalenceRelation), (f64, usize)>,
	all_rels: HashSet<ReducedEquivalenceRelation>,
}

impl Data {
	pub fn new() -> Data {
		Data {
			permutations: HashMap::new(),
			rel_counts: HashMap::new(),
			max_ratios: HashMap::new(),
			all_rels: HashSet::new(),
		}
	}

	pub fn print(&self) {
		let mut rels_ordered = self.all_rels.iter().map(|x| x.to_owned()).collect::<Vec<ReducedEquivalenceRelation>>();
		rels_ordered.sort();
	
		println!("All equivalence relations");
		for rel in rels_ordered.iter() {
			println!("{:?} : code = {}, count = {}", rel, rel.to_code(), self.rel_counts.get(rel).unwrap())
		}
	
		fn is_approx(x: &f64, y: f64) -> bool {
			*x >= y * 0.99999 && *x <= y * 1.000001
		}
	
		let mut num_unwrapping_fails = 0;
		let mut num_big = 0;
		let mut num_ratio_one_pairs = 0;
		let mut num_ratio_half_pairs = 0;
		let mut num_ratio_two_pairs = 0;
		let mut good_pairs = vec![];
		for rel1 in rels_ordered.iter() {
			for rel2 in rels_ordered.iter() {
				if rel1 != rel2 {
					if let Some((ratio, count)) = self.max_ratios.get(&(rel1.to_owned(), rel2.to_owned())) {
						if *ratio > BIG_CUTOFF {
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
						num_unwrapping_fails += 1;
					}
				}
			}
		}
	
		fn cmp_pair(count1: usize, r1: f64, count2: usize, r2: f64) -> Ordering {
			// This might have done crazy stuff in the past.
			let ratio_compare = r1.partial_cmp(&r2).unwrap_or(Ordering::Equal);
			if ratio_compare.is_eq() {
				count1.cmp(&count2)
			} else {
				ratio_compare
			}
		}
	
		good_pairs.sort_by(|(_, _, count1, r1), (_, _, count2, r2)| cmp_pair(*count1, *r1, *count2, *r2));
		let mut all_rel_counts = Vec::from_iter(self.rel_counts.iter());
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
		println!("There are {} different relations", self.all_rels.len());
		println!("The three rarest configs are: ");
		for (rel, count) in all_rel_counts.iter().take(3) {
			rel.print_fancy(**count);
			println!();
		}
	}

	pub fn insert_relation(&mut self, rel: &ReducedEquivalenceRelation) {
		if !self.all_rels.contains(rel) {
			self.all_rels.insert(rel.to_owned());
		}
		if !self.permutations.contains_key(rel) {
			self.permutations.insert(rel.to_owned(), rel.get_all_permutations());
		}
		self.rel_counts.entry(rel.to_owned()).and_modify(|x| *x += 1).or_insert(1);
	}

	pub fn get_lexicographically_first_permutation(&self, rel1: &ReducedEquivalenceRelation, rel2: &ReducedEquivalenceRelation) -> (ReducedEquivalenceRelation, ReducedEquivalenceRelation) {
		let mut first_rel1 = rel1;
		let mut first_rel2 = rel2;
		for (i, permed_rel1) in self.permutations.get(rel1).unwrap().iter().enumerate() {
			let permed_rel2 = &self.permutations.get(rel2).unwrap()[i];
			if permed_rel1 < first_rel1 || (permed_rel1 == first_rel1 && permed_rel2 < first_rel2) {
				first_rel1 = permed_rel1;
				first_rel2 = permed_rel2;
			}
		}
		(first_rel1.to_owned(), first_rel2.to_owned())
	}

    const NOOOTERS: [(u128, u128); 3] = [(148, 144), (20, 80), (34026, 34020)];

    pub fn consider_noooting(&self, g_etc: &GraphAndMetadata, counts: &EquivalenceCounts) {
        for (rel1, count1) in counts.iter() {
            for (rel2, count2) in counts.iter() {
                let (reduced_rel1, reduced_rel2) = self.get_lexicographically_first_permutation(rel1, rel2);
                for (code1, code2) in Self::NOOOTERS.iter() {
                    if reduced_rel1.to_code() == *code1 && reduced_rel2.to_code() == *code2 && *count1 > *count2 {
                        self.print();
                        println!("Found a graph with the desired config ratio!");
                        rel1.print_fancy_pair(&rel2, (*count1 as f64) / (*count2 as f64), 1);
                        println!("Counts {} / {}", count1, count2);
                        g_etc.print();
                        
                        println!("{} distinct RERs", counts.len());
                        panic!("NOOOT NOOOT")
                    }
                }
            }
        }
    }

    pub fn add_equivalence_counts(&mut self, g_etc: &GraphAndMetadata, counts: &EquivalenceCounts) {
        for rel in counts.keys() {
            self.insert_relation(rel)
        }
        for rel1 in self.all_rels.iter() {
            let count1 = counts[rel1];
            if count1 != 0 {
                for rel2 in self.all_rels.iter() {
                    if rel1.k == rel2.k {
                        let count2 = counts[rel2];
                        let ratio = if count2 == 0 { f64::INFINITY } else { (count1 as f64) / (count2 as f64) };
                        let (rel1, rel2) = self.get_lexicographically_first_permutation(rel1, rel2);

                        self.max_ratios.entry((rel1, rel2))
                            .and_modify(|(x, count)| if ratio > *x { *x = ratio; *count = 1 } else if ratio == *x { *count += 1 } )	
                            .or_insert((ratio, 1));
                    }
                }
            }
        }
        if counts.is_genuine_counterexample() {
            self.print();
            println!("Just g:");
            counts.print();
            g_etc.print();
            panic!("WE'VE GOT A LIVE ONE!")
        }
        self.consider_noooting(g_etc, counts)
    }
}