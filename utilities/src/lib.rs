use std::fmt;
use std::cmp::*;
use std::ops;

use vertex_tools::Vertex;
use vertex_tools::VertexPair;

pub mod polynomial;
pub mod rational;
pub mod edge_tools;
pub mod vertex_tools;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Order(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Degree(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Component(Vertex);

impl Order {
    pub fn of_string(text: &str) -> Order {
        Order(text.parse().unwrap())
    }

    pub fn of_usize(n: usize) -> Order {
        Order(n)
    }

    pub fn iter_verts(&self) -> impl Iterator<Item = Vertex> {
        (0..self.0).map(|x| Vertex::of_usize(x))
    }

    pub fn iter_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        VertexPair::new(*self)
    }

    pub fn at_least(&self, d: usize) -> bool {
        self.0 >= d
    }

    pub fn at_most(&self, d: usize) -> bool {
        self.0 <= d
    }

    pub fn less_than(&self, d: usize) -> bool {
        self.0 < d
    }

    pub fn more_than(&self, d: usize) -> bool {
        self.0 > d
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }

    pub fn incr(&self) -> Order {
        Order(self.0 + 1)
    }

    pub fn to_max_vertex(&self) -> Vertex {
        Vertex::of_usize(self.0 - 1)
    }

    pub fn times(&self, x: usize) -> Order {
        Order(self.0 * x)
    }

    pub fn triangle(&self) -> usize {
        (self.0 * (self.0 - 1)) / 2
    }
}

impl fmt::Display for Order {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl ops::Add<Order> for Order {
    type Output = Order;

    fn add(self, rhs: Order) -> Order {
        Order(self.0 + rhs.0)
    }
}

impl Degree {
    pub const ZERO: Degree = Degree(0);
    pub const INF: Degree = Degree(usize::MAX);

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

    pub fn incr_inplace(&mut self) {
        self.0 += 1;
    }

    pub fn decr(&self) -> Degree {
        Degree(self.0 - 1)
    }

    pub fn equals(&self, d: usize) -> bool {
        self.0 == d
    }

    pub fn at_least(&self, d: usize) -> bool {
        self.0 >= d
    }

    pub fn at_most(&self, d: usize) -> bool {
        self.0 <= d
    }

    pub fn less_than(&self, d: usize) -> bool {
        self.0 < d
    }

    pub fn more_than(&self, d: usize) -> bool {
        self.0 > d
    }
}

impl fmt::Display for Degree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl ops::Add<Degree> for Degree {
    type Output = Degree;

    fn add(self, rhs: Degree) -> Degree {
        Degree(self.0 + rhs.0)
    }
}

impl Component {
    pub fn of_vertex(v: Vertex) -> Component {
        Component(v)
    }

    pub fn to_vertex(&self) -> Vertex {
        self.0
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