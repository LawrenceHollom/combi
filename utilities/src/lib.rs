use std::fmt;

pub mod polynomial;

#[derive(Copy, Clone)]
pub struct Order(usize);

pub struct Degree(usize);

impl Order {
    pub fn of_string(text: &str) -> Order {
        Order(text.parse().unwrap())
    }

    pub fn of_usize(n: usize) -> Order {
        Order(n)
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for Order {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Degree {
    pub const ZERO: Degree = Degree(0);

    pub fn of_string(text: &str) -> Degree {
        Degree(text.parse().unwrap())
    }

    pub fn of_usize(d: usize) -> Degree {
        Degree(d)
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn incr(&self) -> Degree {
        Degree(self.0 + 1)
    }
}

impl fmt::Display for Degree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn pow(base: u64, exp: u64) -> u64 {
    if exp == 0 {
        1
    } else {
        base * pow(base, exp - 1)
    }
}

pub fn parse_function_like(text: &str) -> (&str, Vec<&str>) {
    match text.split_once('(') {
        Some((func, args_string)) => {
            let mut depth = 0;
            let mut last_index = 0;
            let mut args = vec![];
            for (i, c) in args_string.as_bytes().iter().enumerate() {
                if *c == '(' as u8 {
                    depth += 1;
                } else if *c == ')' as u8 {
                    depth -= 1;
                } else if *c == ',' as u8 && depth == 0 {
                    args.push(&args_string[last_index..i]);
                    last_index = i + 1;
                }
            }
            args.push(&args_string[last_index..args_string.len()].trim_end_matches(')'));
            (func, args)
        }
        None => (text, vec![]),
    }
}