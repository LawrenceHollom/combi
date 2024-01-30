use std::fmt::*;
use std::cmp::Ordering;
use std::hash::*;

use super::equivalence_relation::*;

use utilities::component_tools::*;

use colored::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct EquivalenceClass(usize);

impl EquivalenceClass {
	pub fn incr_inplace(&mut self) {
		self.0 += 1
	}

	pub fn to_string(&self) -> String {
		self.0.to_string()
	}

    pub fn to_colored_string(&self) -> ColoredString {
        match self.0 {
            0 => "0".red(),
            1 => "1".blue(),
            2 => "2".green(),
            3 => "3".yellow(),
            4 => "4".magenta(),
            5 => "5".cyan(),
            _ => self.0.to_string().bold(),
        }
    }
}

// At some point we're probably going to opt this to use integers
#[derive(Clone)]
pub struct ReducedEquivalenceRelation {
	pub k: usize,
	down: Vec<EquivalenceClass>,
	up: Vec<EquivalenceClass>,
	next_label: EquivalenceClass,
}

impl PartialEq for ReducedEquivalenceRelation {
    fn eq(&self, other: &Self) -> bool {
		if self.k != other.k {
			return false;
		}
        for (i, c) in self.down.iter().enumerate() {
			if *c != other.down[i] {
				return false;
			}
		}
		for (i, c) in self.up.iter().enumerate() {
			if *c != other.up[i] {
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
        for (i, c) in self.down.iter().enumerate() {
			if c.cmp(&other.down[i]) != Ordering::Equal {
				return c.cmp(&other.down[i])
			}
		}
		for (i, c) in self.up.iter().enumerate() {
			if c.cmp(&other.up[i]) != Ordering::Equal {
				return c.cmp(&other.up[i])
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
        for c in self.down.iter() {
			c.hash(state)
		}
		for c in self.up.iter() {
			c.hash(state)
		}
    }
}

impl Debug for ReducedEquivalenceRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
		let _ = write!(f, "[ ");
		for c in self.down.iter() {
			let _ = write!(f, "{} ", c.to_string());
		}
		let _ = write!(f, "/ ");
		for c in self.up.iter() {
			let _ = write!(f, "{} ", c.to_string());
		}
        write!(f, "]")
    }
}

impl ReducedEquivalenceRelation {
	pub fn of_raw_vecs(raw: &[[usize; 2]; 2]) -> ReducedEquivalenceRelation {
		let down = raw[0].iter().map(|x| EquivalenceClass(*x)).collect::<Vec<EquivalenceClass>>();
		let up = raw[1].iter().map(|x| EquivalenceClass(*x)).collect::<Vec<EquivalenceClass>>();
		let next_label = EquivalenceClass(raw[0][0].max(raw[0][1]).max(raw[1][0]).max(raw[1][1]) + 1);
		ReducedEquivalenceRelation { k: 2, down, up, next_label }
	}

	pub fn of_equiv_rel(rel: EquivalenceRelation) -> ReducedEquivalenceRelation {
		let mut down = vec![];
		let mut up = vec![];
		let mut new_labels = ComponentVec::new(rel.n.times(2), &None);
		let mut next_label = EquivalenceClass(0);
		for v in rel.iter_vertices() {
			let c = rel[v];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			down.push(new_labels[c].unwrap());
			let c = rel[v.incr_by_order(rel.n)];
			if new_labels[c].is_none() {
				new_labels[c] = Some(next_label);
				next_label.incr_inplace();
			}
			up.push(new_labels[c].unwrap())
		}
		ReducedEquivalenceRelation { k: rel.get_k(), down, up, next_label }.reduce_and_symmetrise()
	}

    pub fn of_short_string(text: &str) -> ReducedEquivalenceRelation {
        let k = text.len() / 2;
        let bytes = text.as_bytes();
        let zero = b'0';
        let mut down = vec![EquivalenceClass(0); k];
        let mut up = vec![EquivalenceClass(0); k];
        let mut next_label = EquivalenceClass(0);
        for (i, c) in bytes.iter().take(k).enumerate() {
            let v = EquivalenceClass((*c - zero) as usize);
            down[i] = v;
            if v > next_label {
                next_label = v;
            }
        }
        for (i, c) in bytes.iter().skip(k).enumerate() {
            let v = EquivalenceClass((*c - zero) as usize);
            up[i] = v;
            if v > next_label {
                next_label = v;
            }
        }
        ReducedEquivalenceRelation { k, down, up, next_label }
    }

    pub fn to_short_string(&self) -> String {
        let mut bytes = vec![0_u8; self.k * 2];
        for (i, v) in self.down.iter().enumerate() {
            bytes[i] = v.0 as u8 + b'0';
        }
        for (i, v) in self.up.iter().enumerate() {
            bytes[i + self.k] = v.0 as u8 + b'0';
        }
        String::from_utf8_lossy(&bytes).to_string()
    }

	pub fn empty(k: usize) -> ReducedEquivalenceRelation {
		let down = (0..k).map(|x| EquivalenceClass(x)).collect::<Vec<EquivalenceClass>>();
		let up = (k..(2 *k)).map(|x| EquivalenceClass(x)).collect::<Vec<EquivalenceClass>>();
		ReducedEquivalenceRelation { k, down, up, next_label: EquivalenceClass(2 * k) }.reduce_and_symmetrise()
	}

	pub fn reduce_no_symmetrise(&self, is_flat: bool) -> ReducedEquivalenceRelation {
		let mut new_labels = vec![None; (self.k + 1) * 2];
		let mut next_label = EquivalenceClass(0);
		fn check_label(labels: &mut Vec<Option<EquivalenceClass>>, next_label: &mut EquivalenceClass, comp: EquivalenceClass) {
			if labels[comp.0].is_none() {
				labels[comp.0] = Some(*next_label);
				next_label.incr_inplace()
			}
		}
		for v in 0..self.k {
			if is_flat {
				check_label(&mut new_labels, &mut next_label, self.down[v]);
				check_label(&mut new_labels, &mut next_label, self.up[v]);
			} else {
				check_label(&mut new_labels, &mut next_label, self.up[v]);
				check_label(&mut new_labels, &mut next_label, self.down[v]);
			}
		}
		let mut new_down = vec![];
		let mut new_up = vec![];
		for v in 0..self.k {
			if is_flat {
				new_down.push(new_labels[self.down[v].0].unwrap());
				new_up.push(new_labels[self.up[v].0].unwrap());
			} else {
				new_down.push(new_labels[self.up[v].0].unwrap());
				new_up.push(new_labels[self.down[v].0].unwrap());
			}
		}
		ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label }
	}

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
			let mut new_down = vec![];
			let mut new_up = vec![];
			for i in sigma.iter() {
				new_down.push(self.down[*i]);
				new_up.push(self.up[*i]);
			}
			permutations.push(ReducedEquivalenceRelation { k: self.k, down: new_down, up: new_up, next_label: self.next_label }.reduce_and_symmetrise())
		}
		permutations
	}

	pub fn add_vertex(&mut self, is_post: bool) {
		self.down.push(self.next_label);
		if !is_post {
			self.next_label.incr_inplace();
		}
		self.up.push(self.next_label);
		self.next_label.incr_inplace();
		self.k += 1;
	}

	/** 
	 * Blindly replace all d with c in the RER
	 * will need reducing at a later date.
	 */
	fn merge_components(&mut self, c: EquivalenceClass, d: EquivalenceClass) {
		for thing in self.down.iter_mut() {
			if *thing == d {
				*thing = c
			}
		}
		for thing in self.up.iter_mut() {
			if *thing == d {
				*thing = c
			}
		}
	}

	pub fn amalgamate_edge(&mut self, new_edge: &ReducedEquivalenceRelation, x: usize, y: usize) {
		// We currently do the silly, easy version, and move to the harder one later.
		// Let's just case-bash it for now.
		if new_edge.down[0] == new_edge.down[1] {
			self.merge_components(self.down[x], self.down[y])
		}
		if new_edge.up[0] == new_edge.up[1] {
			self.merge_components(self.up[x], self.up[y])
		}
		let verts = [x, y];
		for i in 0..2 {
			for j in 0..2 {
				if new_edge.down[i] == new_edge.up[j] {
					self.merge_components(self.down[verts[i]], self.up[verts[j]])
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
		self.down[0] == self.down[1]
	}

	pub fn is_classically_cross(&self) -> bool {
		self.down[0] == self.up[1]
	}

	pub fn to_string(&self) -> String {
		format!("{:?}", self)
	}

	fn print_row(row: &Vec<EquivalenceClass>) {
		for c in row.iter() {
			print!(" {}", c.to_colored_string())
		}
	}

	pub fn print_fancy(&self, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!(" ) code = {}\n(", self.to_code());
		Self::print_row(&self.down);
		println!(" ) : [{}]", count);
	}

	pub fn print_fancy_pair(&self, denom: &ReducedEquivalenceRelation, ratio: f64, count: usize) {
		print!("(");
		Self::print_row(&self.up);
		print!("  /");
		Self::print_row(&denom.up);
		print!(" ) codes = ({}) / ({})\n(", self.to_code(), denom.to_code());
		Self::print_row(&self.down);
		print!(" / ");
		Self::print_row(&denom.down);
		println!(" ) : {:.6} [{}]", ratio, count)
	}

	pub fn to_code(&self) -> u128 {
		let mut code = 0_u128;
		let mut pow = 1_u128;
		for x in self.down.iter() {
			code += (x.0 as u128) * pow;
			pow *= 2 * self.k as u128;
		}
		for x in self.up.iter() {
			code += (x.0 as u128) * pow;
			pow *= 2 * self.k as u128;
		}
		code
	}
}