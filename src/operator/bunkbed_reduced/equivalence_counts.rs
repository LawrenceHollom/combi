use std::collections::HashMap;
use std::ops::Index;

use utilities::{*, edge_tools::*};

use super::graph_and_metadata::*;
use super::equivalence_relation::*;
use super::reduced_equivalence_relation::*;

#[derive(Clone)]
pub struct EquivalenceCounts {
	counts: HashMap<ReducedEquivalenceRelation, u128>,
	k: usize,
}

impl EquivalenceCounts {
	pub fn new() -> EquivalenceCounts {
		EquivalenceCounts { counts : HashMap::new(), k: 0 }
	}

	pub fn new_singleton() -> EquivalenceCounts {
		let mut counts = HashMap::new();
		counts.insert(ReducedEquivalenceRelation::empty(1), 1);
		EquivalenceCounts { counts, k: 1 }
	}

	pub fn add(&mut self, g_etc: &GraphAndMetadata, config: &EdgeSet, indexer: &EdgeIndexer) {
		let conn = EquivalenceRelation::new(g_etc, config, indexer);
		let reduced_conn = ReducedEquivalenceRelation::of_equiv_rel(conn);
		self.counts.entry(reduced_conn).and_modify(|x| *x += 1).or_insert(1);
	}

	pub fn add_vertex(&mut self, is_post: bool) {
		let mut new_counts = HashMap::new();
		for (mut rel, count) in self.counts.drain() {
			rel.add_vertex(is_post);
			new_counts.insert(rel, count);
		}
		self.k += 1;
		self.counts = new_counts
	}

	pub fn amalgamate_edge(&mut self, edge: &Vec<ReducedEquivalenceRelation>, x: usize, y: usize) {
		let mut new_counts = HashMap::new();
		for (rel, count) in self.counts.iter() {
			for edge_rel in edge.iter() {
				let mut new_rel = rel.to_owned();
				new_rel.amalgamate_edge(edge_rel, x, y);
				new_counts.entry(new_rel)
						.and_modify(|x| *x += *count)
						.or_insert(*count);
			}
		}
		self.counts = new_counts
	}

	pub fn remove_vertex(&mut self, x: usize) {
		let mut new_counts = HashMap::new();
		for (mut rel, count) in self.counts.drain() {
			rel.remove_vertex(x);
			new_counts.entry(rel)
					.and_modify(|y| *y += count)
					.or_insert(count);
		}
		self.k -= 1;
		self.counts = new_counts
	}

	/**
	 * Symmetrise the configs and all that.
	 */
	pub fn reduce(&mut self) {
		let mut new_counts = HashMap::new();
		for (rel, count) in self.counts.drain() {
			let new_rel = rel.reduce_and_symmetrise();
			new_counts.entry(new_rel)
					.and_modify(|v| *v = count.max(*v))
					.or_insert(count);
		}
		self.counts = new_counts
	}
    
    pub fn is_genuine_counterexample(&self) -> bool {
        let mut num_flat = 0;
        let mut num_cross = 0;

        for (rel1, count) in self.iter() {
            if rel1.k == 2 {
                if rel1.is_classically_flat(true) {
                    num_flat += *count
                }
                if rel1.is_classically_cross(true) {
                    num_cross += *count
                }
                if rel1.is_classically_flat(false) {
                    num_flat += *count
                }
                if rel1.is_classically_cross(false) {
                    num_cross += *count
                }
            }
        }

        if num_cross > num_flat {
            println!("num classically flat: {}, num classically cross: {}", num_flat, num_cross);
        }

        num_cross > num_flat
    }

	pub fn num_distinct_rers(&self) -> usize {
		self.counts.keys().len()
	}

    pub fn iter(&self) -> impl Iterator<Item = (&ReducedEquivalenceRelation, &u128)> {
        self.counts.iter()
    }

    pub fn len(&self) -> usize {
        self.counts.len()
    }

    pub fn keys(&self) -> impl Iterator<Item = &ReducedEquivalenceRelation> {
        self.counts.keys()
    }

	pub fn print(&self) {
		let mut rows = self.counts.iter()
			.map(|(rel, c)| (rel.to_string(), vec![c.to_string(), rel.k.to_string()]))
			.collect::<Vec<(String, Vec<String>)>>();
		rows.sort();
		print_table(vec!["count".to_string(), "k".to_string()], rows)
	}

	pub fn print_fancy(&self) {
		let mut counts = self.counts.iter().collect::<Vec<(&ReducedEquivalenceRelation, &u128)>>();
		counts.sort();
		for (rer, count) in counts {
			rer.print_fancy(*count);
			println!();
		}
	}

	pub fn print_summary(&self, g_etc: &GraphAndMetadata) {
		fn get_num(counts: &HashMap<ReducedEquivalenceRelation, u128>, raw: &[[usize; 2]; 2]) -> u128 {
			let rel = ReducedEquivalenceRelation::of_raw_vecs(raw).reduce_and_symmetrise();
			*counts.get(&rel).unwrap_or(&0)
		}
		let num_single_flat = get_num(&self.counts, &[[0, 2], [1, 1]]);
		let num_single_cross = get_num(&self.counts, &[[0, 2], [1, 0]]);
		let num_double_flat = get_num(&self.counts, &[[0, 0], [1, 1]]);
		let num_double_cross = get_num(&self.counts, &[[0, 1], [1, 0]]);
		let single_ratio = (num_single_cross as f64) / (num_single_flat as f64);
		let double_ratio = (num_double_cross as f64) / (num_double_flat as f64);
		println!("1-flat: {}, 1-cross: {}, ratio: {:.5} // 2-flat: {}, 2-cross: {}, ratio: {:.5}", 
		num_single_flat, num_single_cross, single_ratio, num_double_flat, num_double_cross, double_ratio);
		if num_single_flat < num_single_cross || num_double_flat < num_double_cross {
			println!("Counterexample found!");
			g_etc.print();
			self.print_fancy();
			panic!("SO IT HAS BEEN DONE!")
		}
	}
}

impl Index<&ReducedEquivalenceRelation> for EquivalenceCounts {
    type Output = u128;

    fn index(&self, index: &ReducedEquivalenceRelation) -> &Self::Output {
        self.counts.get(index).unwrap_or(&0)
    }
}