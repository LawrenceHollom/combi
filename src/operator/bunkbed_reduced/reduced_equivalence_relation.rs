use std::fmt::*;
use std::cmp::Ordering;
use std::hash::*;

use super::equivalence_relation::*;

use utilities::component_tools::*;

mod fast_vec;

use fast_vec::*;

// At some point we're probably going to opt this to use integers
#[derive(Clone)]
pub struct ReducedEquivalenceRelation {
	pub k: usize,
	down: FastVec,
	up: FastVec,
	next_label: EquivalenceClass,
}

impl PartialEq for ReducedEquivalenceRelation {
    fn eq(&self, other: &Self) -> bool {
		if self.k != other.k {
			return false;
		}
        for (i, c) in self.down.iter(self.k).enumerate() {
			if c != other.down.get(i) {
				return false;
			}
		}
		for (i, c) in self.up.iter(self.k).enumerate() {
			if c != other.up.get(i) {
				return false;
			}
		}
		true
    }
}

impl Eq for ReducedEquivalenceRelation {}

impl Ord for ReducedEquivalenceRelation {
    fn cmp(&self, other: &Self) -> Ordering {
		if self.k != other.k {
			return self.k.cmp(&other.k);
		}
        for (i, c) in self.down.iter(self.k).enumerate() {
			if c.cmp(&other.down.get(i)) != Ordering::Equal {
				return c.cmp(&other.down.get(i))
			}
		}
		for (i, c) in self.up.iter(self.k).enumerate() {
			if c.cmp(&other.up.get(i)) != Ordering::Equal {
				return c.cmp(&other.up.get(i))
			}
		}
		Ordering::Equal
    }
}

impl PartialOrd for ReducedEquivalenceRelation {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for ReducedEquivalenceRelation {
	fn hash<H: Hasher>(&self, state: &mut H) {
        self.down.hash(state);
		self.up.hash(state)
    }
}

impl Debug for ReducedEquivalenceRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		let _ = write!(f, "[ ");
		for c in self.down.iter(self.k) {
			let _ = write!(f, "{} ", c.to_string());
		}
		let _ = write!(f, "/ ");
		for c in self.up.iter(self.k) {
			let _ = write!(f, "{} ", c.to_string());
		}
        write!(f, "]")
    }
}

impl ReducedEquivalenceRelation {
	pub fn of_raw_vecs(raw: &[[usize; 2]; 2]) -> ReducedEquivalenceRelation {
		let down = FastVec::of_array(&raw[0]);
		let up = FastVec::of_array(&raw[1]);
		let next_label = EquivalenceClass::new(raw[0][0].max(raw[0][1]).max(raw[1][0]).max(raw[1][1]) + 1);
		ReducedEquivalenceRelation { k: 2, down, up, next_label }
	}

	pub fn of_equiv_rel(rel: EquivalenceRelation) -> ReducedEquivalenceRelation {
		let mut down = FastVec::new();
		let mut up = FastVec::new();
		let mut new_labels = ComponentVec::new(rel.n.times(2), &None);
		let mut next_label = EquivalenceClass::ZERO;
		for (index, v) in rel.iter_vertices().enumerate() {
			let c = rel[v];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			down.set(index, new_labels[c].unwrap());
			let c = rel[v.incr_by_order(rel.n)];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			up.set(index, new_labels[c].unwrap())
		}
		ReducedEquivalenceRelation { k: rel.get_k(), down, up, next_label }.reduce_and_symmetrise()
	}

    pub fn of_short_string(text: &str) -> ReducedEquivalenceRelation {
        let k = text.len() / 2;
        let bytes = text.as_bytes();
		if bytes.len() != k * 2 {
			panic!("The bytes are too wrong! {}", text)
		}
        let zero = b'0';
        let mut down = FastVec::new();
        let mut up = FastVec::new();
        let mut next_label = EquivalenceClass::ZERO;
        for (i, c) in bytes.iter().take(k).enumerate() {
            let v = EquivalenceClass::new((*c - zero) as usize);
            down.set(i, v);
            if v > next_label {
                next_label = v;
            }
        }
        for (i, c) in bytes.iter().skip(k).enumerate() {
            let v = EquivalenceClass::new((*c - zero) as usize);
            up.set(i, v);
            if v > next_label {
                next_label = v;
            }
        }
        ReducedEquivalenceRelation { k, down, up, next_label }
    }

    pub fn to_short_string(&self) -> String {
        let mut bytes = vec![0_u8; self.k * 2];
        for (i, v) in self.down.iter(self.k).enumerate() {
            bytes[i] = v.to_char();
        }
        for (i, v) in self.up.iter(self.k).enumerate() {
            bytes[i + self.k] = v.to_char();
        }
        String::from_utf8_lossy(&bytes).to_string()
    }

	pub fn empty(k: usize) -> ReducedEquivalenceRelation {
		let down = FastVec::of_range(0, 2, k);
		let up = FastVec::of_range(1, 2, k);
		ReducedEquivalenceRelation { k, down, up, next_label: EquivalenceClass::new(2 * k) }
	}

	// We need to think through what's going on here and if there's a better way to do it.
	pub fn reduce_no_symmetrise_slow(&self, is_flat: bool) -> ReducedEquivalenceRelation {
		let mut new_labels = vec![None; (self.k + 1) * 2];
		let mut next_label = EquivalenceClass::ZERO;
		fn check_label(labels: &mut Vec<Option<EquivalenceClass>>, next_label: &mut EquivalenceClass, comp: EquivalenceClass) {
			if labels[comp._to_usize()].is_none() {
				labels[comp._to_usize()] = Some(*next_label);
				next_label.incr_inplace()
			}
		}
		for v in 0..self.k {
			if is_flat {
				check_label(&mut new_labels, &mut next_label, self.down.get(v));
				check_label(&mut new_labels, &mut next_label, self.up.get(v));
			} else {
				check_label(&mut new_labels, &mut next_label, self.up.get(v));
				check_label(&mut new_labels, &mut next_label, self.down.get(v));
			}
		}
		let mut new_down = FastVec::new();
		let mut new_up = FastVec::new();
		for v in 0..self.k {
			if is_flat {
				new_down.set(v, new_labels[self.down.get(v)._to_usize()].unwrap());
				new_up.set(v, new_labels[self.up.get(v)._to_usize()].unwrap());
			} else {
				new_down.set(v, new_labels[self.up.get(v)._to_usize()].unwrap());
				new_up.set(v, new_labels[self.down.get(v)._to_usize()].unwrap());
			}
		}
		ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label }
	}

	// This is under the assumptiong that self is already reduced (and not necc symmetrised)
	pub fn reduce_and_symmetrise(&self) -> ReducedEquivalenceRelation {
		let mut flipped = self.to_owned();
		let mut going_up = EquivalenceClassSet::new();
		let mut going_down = EquivalenceClassSet::new();
		let mut next_class = EquivalenceClass::ZERO;
		for index in 0..self.k {
			let old_down = self.down.get(index);
			let old_up = self.up.get(index);
			//if they're equal we don't need to do anything.
			if old_down == old_up {
				if old_down == next_class {
					next_class.incr_inplace()
				} else if going_down.has(old_down) {
					flipped.down.set(index, old_down.decr());
					flipped.up.set(index, old_down.decr());
				} else if going_up.has(old_down) {
					flipped.down.set(index, old_down.incr());
					flipped.up.set(index, old_down.incr());
				}
			} else {
				let old_next_class = next_class.to_owned();
				if old_up >= old_next_class {
					if old_up > next_class {
						going_down.add(old_up);
					}
					flipped.down.set(index, next_class);
					next_class.incr_inplace();
				} else if going_down.has(old_up) {
					flipped.down.set(index, old_up.decr())
				} else if going_up.has(old_up) {
					flipped.down.set(index, old_up.incr())
				} else {
					flipped.down.set(index, old_up)
				}
				// Now deal with up.
				if old_down >= old_next_class {
					if old_down < next_class {
						going_up.add(old_down);
					}
					flipped.up.set(index, next_class);
					next_class.incr_inplace()
				} else if going_down.has(old_down) {
					flipped.up.set(index, old_down.decr())
				} else if going_up.has(old_down) {
					flipped.up.set(index, old_down.incr())
				} else {
					flipped.up.set(index, old_down)
				}
			}
		}
		flipped.min(self.to_owned())
	}

	pub fn get_all_permutations(&self) -> Vec<ReducedEquivalenceRelation> {
		let mut permutations = vec![];
		// iterate through all the k! permutations, produce, reduce, and add.
		let sigmas = if self.k == 2 {
				vec![vec![0, 1], vec![1, 0]]
			} else if self.k == 3 {
				vec![vec![0, 1, 2], vec![0, 2, 1], vec![1, 0, 2], vec![1, 2, 0], vec![2, 0, 1], vec![2, 1, 0]]	
			} else {
				panic!("Guess what? It's time to fix this awful hack you put in!")
			};
		for sigma in sigmas.iter() {
			let mut new_down = FastVec::new();
			let mut new_up = FastVec::new();
			for (i, j) in sigma.iter().enumerate() {
				new_down.set(i, self.down.get(*j));
				new_up.set(i, self.up.get(*j));
			}
			let rer = ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label: self.next_label };
			//permutations.push(rer.reduce_no_symmetrise_slow(true).reduce_and_symmetrise())
			permutations.push(rer.reduce_no_symmetrise_slow(true).min(rer.reduce_no_symmetrise_slow(false)))
		}
		permutations
	}

	pub fn add_vertex(&mut self, is_post: bool) {
		self.down.set(self.k, self.next_label);
		if !is_post {
			self.next_label.incr_inplace();
		}
		self.up.set(self.k, self.next_label);
		self.next_label.incr_inplace();
		self.k += 1;
	}

	fn merge_components(&mut self, c: EquivalenceClass, d: EquivalenceClass) {
		if c != d {
			let small = c.min(d);
			let big = c.max(d);
			for index in 0..self.k {
				if self.down.get(index) == big {
					self.down.set(index, small)
				} else if self.down.get(index) > big {
					self.down.decr_inplace(index);
				}
			}
			for index in 0..self.k {
				if self.up.get(index) == big {
					self.up.set(index, small)
				} else if self.up.get(index) > big {
					self.up.decr_inplace(index);
				}
			}
			self.next_label.decr_inplace()
		}
	}

	pub fn amalgamate_edge(&mut self, new_edge: &ReducedEquivalenceRelation, x: usize, y: usize) {
		// We currently do the silly, easy version, and move to the harder one later.
		// Let's just case-bash it for now.
		// - this isn't actually very slow on fifty-fifty or classic edges.
		if new_edge.down.get(0) == new_edge.down.get(1) {
			self.merge_components(self.down.get(x), self.down.get(y))
		}
		if new_edge.up.get(0) == new_edge.up.get(1) {
			self.merge_components(self.up.get(x), self.up.get(y))
		}
		let verts = [x, y];
		for i in 0..2 {
			for j in 0..2 {
				if new_edge.down.get(i) == new_edge.up.get(j) {
					self.merge_components(self.down.get(verts[i]), self.up.get(verts[j]))
				}
			}
		}
	}

	pub fn remove_vertex(&mut self, x: usize) {
		let a = self.down.get(x);
		let b = self.up.get(x);
		self.down.remove(x);
		self.up.remove(x);
		self.k -= 1;
		struct DP {
			new_a: Option<EquivalenceClass>,
			new_b: Option<EquivalenceClass>,
			next_class: EquivalenceClass,
			decr_from_a: EquivalenceClassSet,
			decr_from_b: EquivalenceClassSet,
			found: EquivalenceClassSet,
		}

		let mut dp = DP { 
			new_a: None, 
			new_b: None, 
			next_class: EquivalenceClass::ZERO, 
			decr_from_a: EquivalenceClassSet::new(), 
			decr_from_b: EquivalenceClassSet::new(), 
			found: EquivalenceClassSet::new() 
		};

		fn deal_with_point(index: usize, v: &mut FastVec, a: EquivalenceClass, b: EquivalenceClass, dp: &mut DP) {
			let old_val = v.get(index);
			if old_val == a {
				if let Some(new_a) = dp.new_a {
					v.set(index, new_a)
				} else {
					dp.new_a = Some(dp.next_class);
					v.set(index, dp.next_class);
					dp.next_class.incr_inplace();
				}
			} else if old_val == b {
				if let Some(new_b) = dp.new_b {
					v.set(index, new_b)
				} else {
					dp.new_b = Some(dp.next_class);
					v.set(index, dp.next_class);
					dp.next_class.incr_inplace();
				}
			} else if dp.found.has(old_val) {
				if dp.decr_from_a.has(old_val) {
					v.decr_inplace(index);
				}
				if dp.decr_from_b.has(old_val) {
					v.decr_inplace(index);
				}
			} else {
				if old_val > a && dp.new_a.map_or(true, |new_a| old_val <= new_a) {
					dp.decr_from_a.add(old_val);
					v.decr_inplace(index);
				}
				if a != b && old_val > b && dp.new_b.map_or(true, |new_b| old_val <= new_b) {
					dp.decr_from_b.add(old_val);
					v.decr_inplace(index);
				}
				dp.found.add(old_val);
				dp.next_class = dp.next_class.max(v.get(index).incr())
			}
		}

		for index in 0..self.k {
			deal_with_point(index, &mut self.down, a, b, &mut dp);
			deal_with_point(index, &mut self.up, a, b, &mut dp);
		}
		self.next_label = dp.next_class;
	}

	pub fn is_classically_flat(&self, is_flipped: bool) -> bool {
		if is_flipped {
			self.down.get(0) == self.down.get(1)
		} else {
			self.up.get(0) == self.up.get(1)
		}
	}

	pub fn is_classically_cross(&self, is_flipped: bool) -> bool {
		if is_flipped {
			self.down.get(0) == self.up.get(1)
		} else {
			self.up.get(0) == self.down.get(1)
		}
	}

	pub fn to_string(&self) -> String {
		format!("{:?}", self)
	}

	fn print_row(row: &FastVec, k: usize) {
		for c in row.iter(k) {
			print!(" {}", c.to_colored_string())
		}
	}

	pub fn print_fancy(&self, count: u128) {
		print!("(");
		Self::print_row(&self.up, self.k);
		print!(" ) code = {}\n(", self.to_short_string());
		Self::print_row(&self.down, self.k);
		println!(" ) : [{}]", count);
	}

	pub fn print_fancy_pair(&self, denom: &ReducedEquivalenceRelation, ratio: f64, count: usize) {
		print!("(");
		Self::print_row(&self.up, self.k);
		print!("  /");
		Self::print_row(&denom.up, self.k);
		print!(" ) codes = ({}) / ({})\n(", self.to_short_string(), denom.to_short_string());
		Self::print_row(&self.down, self.k);
		print!(" / ");
		Self::print_row(&denom.down, self.k);
		println!(" ) : {:.6} [{}]", ratio, count)
	}
}

#[derive(Copy, Clone)]
pub enum EdgeType {
	Classic,
	FiftyFifty,
	Double,
	ThreeWay,
	Posted,
}

impl EdgeType {
	pub fn of_usize(edge_type: usize) -> EdgeType {
		use EdgeType::*;
		match edge_type {
			0 => Classic,
			1 => FiftyFifty,
			2 => Double,
			3 => ThreeWay,
			4 => Posted,
			_ => FiftyFifty,
		}
	}
	
	pub fn to_rer_vec(&self) -> Vec<ReducedEquivalenceRelation> {
		use EdgeType::*;
		let raw_edges = match self {
			Classic => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]], [[0, 2], [1, 3]], [[0, 0], [0, 0]]],
			FiftyFifty => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]]],
			Double => vec![[[0, 1], [1, 0]], [[0, 0], [1, 1]]],
			ThreeWay => vec![[[0, 0], [1, 1]], [[0, 0], [1, 1]], [[0, 2], [1, 0]], [[0, 1], [1, 2]]],
			Posted => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]], [[0, 2], [1, 0]], [[0, 1], [1, 2]]],
		};
		raw_edges.iter()
			.map(|raw| ReducedEquivalenceRelation::of_raw_vecs(raw))
			.collect::<Vec<ReducedEquivalenceRelation>>()
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn test_edge_types() {
		for i in 0..10 {
			let edge = EdgeType::of_usize(i);
			let rers = edge.to_rer_vec();
			for rer in rers.iter() {
				assert_eq!(rer.to_owned(), rer.reduce_no_symmetrise_slow(true))
			}
		}
	}

	#[test]
	fn test_empty_rer() {
		for k in 1..5 {
			let rer = ReducedEquivalenceRelation::empty(k);
			assert_eq!(rer, rer.reduce_no_symmetrise_slow(true))
		}
	}

	fn rers(text: &str) -> ReducedEquivalenceRelation {
		ReducedEquivalenceRelation::of_short_string(text)
	}

	#[test]
	fn test_remove_vertex() {
		let mut rer = rers("02141130");
		rer.remove_vertex(1);
		assert_eq!(rer, rers("013120"));

		let mut rer = rers("0120302310");
		rer.remove_vertex(0);
		assert_eq!(rer, rers("01321203"))
	}

	#[test]
	fn test_reduce_and_symmetrise() {
		let rer = rers("0123210143");
		assert_eq!(rer.reduce_and_symmetrise(), rers("0103410242"))
	}

	#[test]
	fn test_reduce_no_symmetrise() {
		let rer = rers("000104");
		assert_eq!(rer.reduce_no_symmetrise_slow(true), rers("000102"));
		assert_eq!(rer.reduce_no_symmetrise_slow(true).reduce_and_symmetrise(), rers("000102"));
	}

	#[test]
	fn test_remove_vertex_zero() {
		let mut rer = rers("020133");
		rer.remove_vertex(0);
		assert_eq!(rer, rers("0211"))
	}
}