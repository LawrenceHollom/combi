use colored::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EquivalenceClass(usize);

impl EquivalenceClass {
    pub const ZERO: EquivalenceClass = EquivalenceClass(0);

    pub fn new(c: usize) -> EquivalenceClass {
        EquivalenceClass(c)
    }

	pub fn incr_inplace(&mut self) {
		self.0 += 1
	}

	pub fn to_string(&self) -> String {
		self.0.to_string()
	}

    pub fn to_char(&self) -> u8 {
        self.0 as u8 + b'0'
    }

    // Ideally we would kill this function after the big opt.
    pub fn to_usize(&self) -> usize {
        self.0
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

#[derive(Copy, Clone, Hash)]
pub struct FastVec(u128);

impl FastVec {
    pub fn new() -> FastVec {
        FastVec(0)
    }

    pub fn of_array(arr: &[usize; 2]) -> FastVec {
        FastVec(((arr[1] << 5) + arr[0]) as u128)
    }

    pub fn of_range(start: usize, end: usize) -> FastVec {
        let mut v = Self::new();
        for (index, c) in (start..end).enumerate() {
            v.set(index, EquivalenceClass(c))
        }
        v
    }

    pub fn get(&self, index: usize) -> EquivalenceClass {
        EquivalenceClass(((self.0 >> (index * 5)) % 32) as usize)
    }

    pub fn set(&mut self, index: usize, c: EquivalenceClass) {
        // blank out any old values
        self.0 &= !(31_u128 << (index * 5));
        self.0 += (c.0 as u128) << (index * 5)
    }

    // Remove data at the index, and shift everything above down by 5 bits.
    pub fn remove(&mut self, index: usize) {
        self.0 = (self.0 % (1 << (5 * index))) + ((self.0 >> (5 * (index + 1))) << (5 * index))
    }

    pub fn iter(&self, k: usize) -> impl Iterator<Item = EquivalenceClass> + '_ {
        (0..k).map(|i| EquivalenceClass(((self.0 >> (5 * i)) % 32) as usize))
    }
}