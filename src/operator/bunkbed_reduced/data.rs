use std::{cmp::Ordering, collections::{HashMap, HashSet}, fmt::Display, fs::{self, File}};
use std::io::Write;

use super::reduced_equivalence_relation::*;
use super::graph_and_metadata::*;
use super::equivalence_counts::*;

const BIG_CUTOFF: f64 = 5.999;

#[derive(PartialEq, Eq, Hash)]
struct RERPair {
    numerator: ReducedEquivalenceRelation,
    denominator: ReducedEquivalenceRelation,
}

impl RERPair {
    fn new(numerator: &ReducedEquivalenceRelation, denominator: &ReducedEquivalenceRelation) -> RERPair {
        RERPair {
            numerator: numerator.to_owned(),
            denominator: denominator.to_owned(),
        }
    }

    fn of_string(text: &str) -> RERPair {
        let (numer, denom) = text.split_once("/").unwrap();
        RERPair {
            numerator: ReducedEquivalenceRelation::of_short_string(numer),
            denominator: ReducedEquivalenceRelation::of_short_string(denom),
        }
    }

    fn to_string(&self) -> String {
        format!("{}/{}", self.numerator.to_short_string(), self.denominator.to_short_string())
    }
}

#[derive(Clone, Copy)]
struct BigRational {
    numer: u128,
    denom: u128,
}

impl BigRational {
    fn new(numer: u128, denom: u128) -> BigRational {
        BigRational { numer, denom }
    }

    fn is_big(&self) -> bool {
        self.numer >= 10 * self.denom
    }

    fn is_one(&self) -> bool {
        self.numer == self.denom
    }

    fn is_two(&self) -> bool {
        self.numer == 2 * self.denom
    }

    fn is_half(&self) -> bool {
        2 * self.numer == self.denom
    }

    fn to_float(&self) -> f64 {
        (self.numer as f64) / (self.denom as f64)
    }
}

impl PartialEq for BigRational {
    fn eq(&self, other: &BigRational) -> bool {
        self.numer * other.denom == other.numer * self.denom
    }
}

impl Eq for BigRational { }

impl PartialOrd for BigRational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BigRational {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.numer * other.denom).cmp(&(other.numer * self.denom))
    }
}

impl Display for BigRational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}/{}", self.numer, self.denom)
    }
}

pub struct Data {
	permutations: HashMap<ReducedEquivalenceRelation, Vec<ReducedEquivalenceRelation>>,
	rel_counts: HashMap<ReducedEquivalenceRelation, usize>,
	max_ratios: HashMap<RERPair, (BigRational, String, usize)>,
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
	
		let mut num_unwrapping_fails = 0;
		let mut num_big = 0;
		let mut num_ratio_one_pairs = 0;
		let mut num_ratio_half_pairs = 0;
		let mut num_ratio_two_pairs = 0;
		let mut good_pairs = vec![];
		for rel1 in rels_ordered.iter() {
			for rel2 in rels_ordered.iter() {
				if rel1 != rel2 {
                    let pair = RERPair::new(rel1, rel2);
					if let Some((ratio, _, count)) = self.max_ratios.get(&pair) {
						if ratio.is_big() {
							num_big += 1;
						} else {
							if ratio.is_one() {
								num_ratio_one_pairs += 1;
							} else if ratio.is_half() {
								num_ratio_half_pairs += 1;
							} else if ratio.is_two() {
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
	
		fn cmp_pair(count1: usize, r1: BigRational, count2: usize, r2: BigRational) -> Ordering {
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
			rel1.print_fancy_pair(rel2, ratio.to_float(), *count);
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

	fn get_lexicographically_first_permutation(&self, pair: &RERPair) -> RERPair {
		let mut first_rel1 = &pair.numerator;
		let mut first_rel2 = &pair.denominator;
		for (i, permed_rel1) in self.permutations.get(&pair.numerator).unwrap().iter().enumerate() {
			let permed_rel2 = &self.permutations.get(&pair.denominator).unwrap()[i];
			if permed_rel1 < first_rel1 || (permed_rel1 == first_rel1 && permed_rel2 < first_rel2) {
				first_rel1 = permed_rel1;
				first_rel2 = permed_rel2;
			}
		}
		RERPair::new(first_rel1, first_rel2)
	}

    const NOOOTERS: [(u128, u128); 3] = [(148, 144), (20, 80), (34026, 34020)];

    pub fn consider_noooting(&self, g_etc: &GraphAndMetadata, counts: &EquivalenceCounts) {
        for (rel1, count1) in counts.iter() {
            for (rel2, count2) in counts.iter() {
                let pair = RERPair::new(rel1, rel2);
                let reduced_pair = self.get_lexicographically_first_permutation(&pair);
                for (code1, code2) in Self::NOOOTERS.iter() {
                    if reduced_pair.numerator.to_code() == *code1 && reduced_pair.denominator.to_code() == *code2 && *count1 > *count2 {
                        self.print();
                        println!("Found a graph with the desired config ratio!");
                        rel1.print_fancy_pair(&rel2, if *count2 == 0 { f64::INFINITY } else { (*count1 as f64) / (*count2 as f64) }, 1);
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
                        let pair = RERPair::new(rel1, rel2);
                        let count2 = counts[rel2];
                        let pair = self.get_lexicographically_first_permutation(&pair);
                        let ratio = BigRational::new(count1, count2);

                        self.max_ratios.entry(pair)
                            .and_modify(|(x, _, count)| 
                                if ratio > *x { 
                                    *x = ratio; 
                                    *count = 1 
                                } else if ratio == *x { 
                                    *count += 1 
                                } )	
                            .or_insert((ratio, g_etc.get_graph_string(), 1));
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

    fn get_ignore_ratios(contents: String) -> HashSet<RERPair> {
        let mut out = HashSet::new();
        for line in contents.lines() {
            out.insert(RERPair::of_string(line));
        }
        out
    }

    fn get_records(contents: String) -> HashMap<RERPair, (BigRational, String)> {
        let mut out = HashMap::new();
        for line in contents.lines() {
            let pars = line.split(":").collect::<Vec<&str>>();
            let pair = RERPair::of_string(pars[0]);
            let (numer, denom) = pars[1].split_once("/").unwrap();
            let ratio = BigRational::new(numer.parse().unwrap(), denom.parse().unwrap());
            out.insert(pair, (ratio, pars[2].to_owned()));
        }
        out
    }

    pub fn save_to_file(&self) {
        // Read in the no-print list and the existing data.
        let mut pathbuf = std::env::current_exe().unwrap();
        pathbuf.pop();
        pathbuf.push("ratios");
        let mut ignore_file = pathbuf.to_owned();
        ignore_file.push("ignore.txt");
        let mut live_file = pathbuf.to_owned();
        live_file.push("live.txt");
        let ignore_ratios = match fs::read_to_string(ignore_file) {
            Ok(contents) => Self::get_ignore_ratios(contents),
            Err(_e) => HashSet::new(),
        };
        let previous_records = match fs::read_to_string(live_file.to_owned()) {
            Ok(contents) => Self::get_records(contents),
            Err(_e) => panic!("Cannot find live file!")
        };
        let mut new_records = HashMap::new();
        for rel1 in self.all_rels.iter() {
            for rel2 in self.all_rels.iter() {
                if rel1.k == rel2.k {
                    let pair = RERPair::new(rel1, rel2);
                    if !ignore_ratios.contains(&pair) {
                        if let Some((new_ratio, new_graph, _count)) = self.max_ratios.get(&pair) {
                            if let Some((old_ratio, old_graph)) = previous_records.get(&pair) {
                                if new_ratio > old_ratio {
                                    new_records.insert(pair, (*new_ratio, new_graph.to_owned()));
                                } else {
                                    new_records.insert(pair, (*old_ratio, old_graph.to_owned()));
                                }
                            } else {
                                new_records.insert(pair, (*new_ratio, new_graph.to_owned()));
                            }
                        } else if let Some((old_ratio, old_graph)) = previous_records.get(&pair) {
                            new_records.insert(pair, (*old_ratio, old_graph.to_owned()));
                        }
                    }
                }
            }
        }
        // Now actually save the new records to the live file.
        let mut writing_file = File::create(live_file).unwrap();

        for (pair, (ratio, graph)) in new_records {
            writeln!(&mut writing_file, "{}:{}:{}", pair.to_string(), ratio, graph).unwrap();
        }
    }
}