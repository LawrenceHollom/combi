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
		let down = FastVec::of_range(0, k);
		let up = FastVec::of_range(k, 2 * k);
		ReducedEquivalenceRelation { k, down, up, next_label: EquivalenceClass::new(2 * k) }.reduce_and_symmetrise()
	}

	// We need to think through what's going on here and if there's a better way to do it.
	fn reduce_no_symmetrise(&self, is_flat: bool) -> ReducedEquivalenceRelation {
		let mut new_labels = vec![None; (self.k + 1) * 2];
		let mut next_label = EquivalenceClass::ZERO;
		fn check_label(labels: &mut Vec<Option<EquivalenceClass>>, next_label: &mut EquivalenceClass, comp: EquivalenceClass) {
			if labels[comp.to_usize()].is_none() {
				labels[comp.to_usize()] = Some(*next_label);
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
				new_down.set(v, new_labels[self.down.get(v).to_usize()].unwrap());
				new_up.set(v, new_labels[self.up.get(v).to_usize()].unwrap());
			} else {
				new_down.set(v, new_labels[self.up.get(v).to_usize()].unwrap());
				new_up.set(v, new_labels[self.down.get(v).to_usize()].unwrap());
			}
		}
		ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label }
	}

	// This is probably going to end up being the expensive one.
	pub fn reduce_and_symmetrise(&self) -> ReducedEquivalenceRelation {
		let new_flat = self.reduce_no_symmetrise(true);
		let new_cross = self.reduce_no_symmetrise(false);
		new_flat.min(new_cross)
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
			for i in sigma.iter() {
				new_down.set(*i, self.down.get(*i));
				new_up.set(*i, self.up.get(*i));
			}
			permutations.push(ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label: self.next_label }.reduce_and_symmetrise())
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
			for index in 0..self.k {
				if self.down.get(index) == d {
					self.down.set(index, c)
				}
			}
			for index in 0..self.k {
				if self.up.get(index) == d {
					self.up.set(index, c)
				}
			}
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
		*self = self.reduce_no_symmetrise(false)
	}

	pub fn remove_vertex(mut self, x: usize) -> ReducedEquivalenceRelation {
		self.down.remove(x);
		self.up.remove(x);
		self.k -= 1;
		self.reduce_no_symmetrise(false)
	}

	pub fn flip(&self) -> ReducedEquivalenceRelation {
		ReducedEquivalenceRelation {
			k: self.k,
			down: self.up.to_owned(),
			up: self.down.to_owned(),
			next_label: self.next_label,
		}
	}

	pub fn is_classically_flat(&self) -> bool {
		self.down.get(0) == self.down.get(1)
	}

	pub fn is_classically_cross(&self) -> bool {
		self.down.get(0) == self.up.get(1)
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
			Classic => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]], [[0, 1], [2, 3]], [[0, 0], [0, 0]]],
			FiftyFifty => vec![[[0, 0], [1, 2]], [[1, 2], [0, 0]]],
			Double => vec![[[0, 1], [1, 0]], [[0, 0], [1, 1]]],
			ThreeWay => vec![[[0, 0], [1, 1]], [[0, 0], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
			Posted => vec![[[0, 0], [1, 2]], [[0, 2], [1, 1]], [[1, 2], [0, 1]], [[1, 0], [0, 2]]],
		};
		raw_edges.iter()
			.map(|raw| ReducedEquivalenceRelation::of_raw_vecs(raw)
				.reduce_no_symmetrise(true))
			.collect::<Vec<ReducedEquivalenceRelation>>()
	}
}