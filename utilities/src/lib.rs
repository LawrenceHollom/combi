use std::fmt;
use std::cmp::*;

pub mod polynomial;
pub mod rational;
pub mod edge_set;

#[derive(Copy, Clone, Debug)]
pub struct Order(usize);

#[derive(Copy, Clone, Debug)]
pub struct Degree(usize);

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct Edge(usize, usize);

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

impl PartialEq for Order {
    fn eq(&self, other: &Order) -> bool {
        self.0 == other.0
    }
}

impl Eq for Order { }

impl PartialOrd for Order {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Order {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
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

    pub fn equals(&self, d: usize) -> bool {
        self.0 == d
    }
}

impl fmt::Display for Degree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq for Degree {
    fn eq(&self, other: &Degree) -> bool {
        self.0 == other.0
    }
}

impl Eq for Degree { }

impl PartialOrd for Degree {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Degree {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.cmp(&other.0)
    }
}

impl Edge {
    pub fn of_string(text: &str) -> Edge {
        let pars: Vec<&str> = text.split("~").map(|x| x.trim()).collect();
        Edge(pars[0].parse().unwrap(), pars[1].parse().unwrap())
    }

    pub fn of_pair(x: usize, y: usize) -> Edge {
        Edge(x, y)
    }

    pub fn to_pair(&self) -> (usize, usize) {
        (self.0, self.1)
    }

    pub fn encode(&self, n: usize) -> usize {
        if self.0 < self.1 {
            self.0 * n + self.1
        } else {
            self.1 * n + self.0
        }
    }

    pub fn fst(&self) -> usize {
        self.0
    }

    pub fn snd(&self) -> usize {
        self.1
    }

    pub fn contains(&self, v: usize) -> bool {
        self.0 == v || self.1 == v
    }

    pub fn equals(&self, d: usize) -> bool {
        self.0 == d
    }    
}

pub fn pow(base: u64, exp: u64) -> u64 {
    if exp == 0 {
        1
    } else {
        base * pow(base, exp - 1)
    }
}

pub fn split_list(text: &str) -> Vec<&str> {
    let mut depth = 0;
    let mut last_index = 0;
    let mut args = vec![];
    for (i, c) in text.as_bytes().iter().enumerate() {
        if *c == '(' as u8 {
            depth += 1;
        } else if *c == ')' as u8 {
            depth -= 1;
        } else if *c == ',' as u8 && depth == 0 {
            args.push(&text[last_index..i]);
            last_index = i + 1;
        }
    }
    args.push(&text[last_index..text.len()].trim_end_matches(')'));
    args
}

pub fn parse_function_like(text: &str) -> (&str, Vec<&str>) {
    match text.split_once('(') {
        Some((func, args_string)) => {
            (func, split_list(args_string))
        }
        None => (text, vec![]),
    }
}

pub fn parse_infix_like(text: &str) -> Option<(&str, &str, &str)> {
    let infix_symbols = vec!['+', '-', '/', '*', '>', '<', '=', '&', '|', '^'];
    let mut operator_start = 0;
    let mut operator_end = 0;
    let bytes = text.as_bytes();
    let left_chop = if bytes[0] == '(' as u8 { 1 } else { 0 };
    let mut depth = 0;
    let mut ever_depth_zero = false;
    if text.len() < 3 {
        // Can't possibly hope to be infix-like
        return None;
    }
    'parser: for (i, c) in bytes.iter().enumerate() {
        if operator_start > 0 && !infix_symbols.contains(&(*c as char)) {
            operator_end = i;
            break 'parser;
        } else if *c == '(' as u8 {
            depth += 1;
        } else if *c == ')' as u8 {
            depth -= 1;
        } else if operator_start == 0 && depth == 0 && infix_symbols.contains(&(*c as char)) {
            operator_start = i;
        }
        if depth == 0 && i < text.len() - 1 {
            ever_depth_zero = true;
        }
    }
    if !ever_depth_zero {
        parse_infix_like(&text[1..text.len()-1])
    } else if operator_start > 0 && operator_end > 0 {
        let op1 = &text[left_chop..(operator_start - left_chop)];
        let op2 = &text[(operator_end + (if bytes[operator_end] == '(' as u8 { 1 } else { 0 }))..text.len()];
        Some((op1, &text[operator_start..operator_end], op2))
    } else {
        None
    }
}