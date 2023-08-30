use std::fmt;
use std::ops;
use std::cmp::*;

fn gcd(x: u64, y: u64) -> u64 {
    if y > x { 
        gcd(y, x)
    } else if y == 0 {
        x
    } else {
        gcd(y, x % y)
    }
}

fn gcd_iu(x: i64, y: u64) -> u64 { gcd(x.abs() as u64, y) }

pub struct Rational {
    numerator: i64,
    denominator: u64
}

impl Rational {
    pub const ONE: Rational = Rational { numerator: 1, denominator: 1 };
    pub const ZERO: Rational = Rational { numerator: 0, denominator: 1 };

    pub fn new (val: i64) -> Rational {
        Rational { numerator: val, denominator: 1 }
    }

    pub fn new_fraction(numer: usize, denom: usize) -> Rational {
        Rational { numerator: numer as i64, denominator: denom as u64 }.normalised()
    }

    fn normalised(&self) -> Rational {
        let gcd = gcd_iu(self.numerator, self.denominator);
        Rational { numerator: self.numerator / (gcd as i64), denominator: self.denominator / gcd }
    }

    pub fn to_f64(self) -> f64 {
        self.numerator as f64 / self.denominator as f64
    }
}

impl ops::Add<Rational> for Rational {
    type Output = Rational;

    fn add(self, rhs: Rational) -> Rational {
        Rational { 
            numerator: self.numerator * rhs.denominator as i64 + rhs.numerator * self.denominator as i64,
            denominator: self.denominator * rhs.denominator 
        }.normalised()
    }
}

impl ops::Sub<Rational> for Rational {
    type Output = Rational;

    fn sub(self, rhs: Rational) -> Rational {
        Rational { 
            numerator: self.numerator * rhs.denominator as i64 - rhs.numerator * self.denominator as i64,
            denominator: self.denominator * rhs.denominator 
        }.normalised()
    }
}

impl ops::Mul<Rational> for Rational {
    type Output = Rational;

    fn mul(self, rhs: Rational) -> Rational {
        Rational {
            numerator: self.numerator * rhs.numerator,
            denominator: self.denominator * rhs.denominator
        }.normalised()
    }
}

impl ops::Div<Rational> for Rational {
    type Output = Rational;

    fn div(self, rhs: Rational) -> Rational {
        Rational {
            numerator: self.numerator * rhs.denominator as i64 * rhs.numerator.clamp(-1, 1),
            denominator: self.denominator * rhs.numerator.abs() as u64
        }.normalised()
    }
}

impl PartialEq for Rational {
    fn eq(&self, other: &Rational) -> bool {
        self.numerator * other.denominator as i64 == other.numerator * self.denominator as i64
    }
}

impl Eq for Rational { }

impl PartialOrd for Rational {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Rational {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.numerator * other.denominator as i64).cmp(&(other.numerator * self.denominator as i64))
    }
}

impl fmt::Display for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} / {}", self.numerator, self.denominator)
    }
}

impl fmt::Debug for Rational {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} / {}", self.numerator, self.denominator)
    }
}