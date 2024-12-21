use std::fmt;
use std::cmp::*;
use std::ops;

use vertex_tools::*;

pub mod polynomial;
pub mod rational;
pub mod edge_tools;
pub mod vertex_tools;
pub mod component_tools;
pub mod chromatic_tools;
pub mod file_write;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Order(usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Degree(usize);

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

    pub fn iter_verts_rev(&self) -> impl Iterator<Item = Vertex> {
        (0..self.0).rev().map(|x| Vertex::of_usize(x))
    }

    pub fn iter_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        VertexPairIterator::new(*self)
    }

    pub fn iter_directed_pairs(&self) -> impl Iterator<Item = (Vertex, Vertex)> {
        VertexDirectedPairIterator::new(*self)
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

    pub fn to_max_deg(&self) -> Degree {
        Degree(self.0 - 1)
    }

    pub fn incr(&self) -> Order {
        Order(self.0 + 1)
    }

    pub fn incr_by(&self, x: usize) -> Order {
        Order(self.0 + x)
    }

    pub fn to_max_vertex(&self) -> Vertex {
        Vertex::of_usize(self.0 - 1)
    }

    pub fn times(&self, x: usize) -> Order {
        Order(self.0 * x)
    }

    pub fn div(&self, x: usize) -> Order {
        Order(self.0 / x)
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

    pub fn decr_inplace(&mut self) {
        self.0 -= 1;
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

impl ops::Sub<Degree> for Degree {
    type Output = Degree;

    fn sub(self, rhs: Degree) -> Degree {
        Degree(self.0 - rhs.0)
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
            if depth < 0 {
                panic!("The parens depth is < 0!")
            }
        } else if *c == ',' as u8 && depth == 0 {
            args.push(&text[last_index..i]);
            last_index = i + 1;
        }
    }
    args.push(&text[last_index..text.len()]);
    args
}

pub fn parse_function_like(text: &str) -> (&str, Vec<&str>) {
    match text.split_once('(') {
        Some((func, args_string)) => {
            match args_string.rsplit_once(')') {
                Some((args_string, _other)) => (func, split_list(args_string)),
                None => (text, vec![]),
            }
        }
        None => (text, vec![]),
    }
}

pub fn parse_infix_like_restricted(text: &str, infix_symbols: Vec<char>) -> Option<(&str, &str, &str)> {
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
        Some((op1.trim(), &text[operator_start..operator_end], op2.trim()))
    } else {
        None
    }
}

pub fn parse_infix_like(text: &str) -> Option<(&str, &str, &str)> {
    let infix_symbols = vec!['+', '-', '/', '*', '>', '<', '=', '&', '|', '^'];
    parse_infix_like_restricted(text, infix_symbols)
}

/**
 * Returns true if the number is a single-digit multiple of a power of 10
 */
pub fn is_round_number(x: usize) -> bool {
    let log = (x as f64).log10() + 0.000001;
    let pow = 10_usize.pow(log as u32);
    x % pow == 0
}

pub fn pretty_format_int(x: usize) -> String {
    if x < 10_000 {
        format!("{}", x)
    } else if x < 100_000 {
        format!("{}k{}", x / 1_000, (x % 1_000) / 100)
    } else if x < 1_000_000 {
        format!("{}k", x / 1_000)
    } else if x < 100_000_000 {
        format!("{}m{}", x / 1_000_000, (x % 1_000_000) / 100_000)
    } else {
        format!("{}m", x / 1_000_000)
    }
}

pub fn pretty_print_int(x: usize, end: &str) {
    print!("{}{}", pretty_format_int(x), end)
}

pub fn pretty_format_time(nanos: u128) -> String {
    let mics = nanos / 1000;
    let millis = mics / 1000;
    let secs = millis / 1000;
    if nanos < 1000 {
        format!("{} ns", nanos)
    } else if mics < 10 {
        format!("{}.{:0>2} μs", mics, (nanos % 1000) / 10)
    } else if mics < 100 {
        format!("{}.{} μs", mics, (nanos % 1000) / 100)
    } else if mics < 1000 {
        format!("{} μs", mics)
    } else if millis < 10 {
        format!("{}.{:0>2} ms", millis, (mics % 1000) / 10)
    } else if millis < 100 {
        format!("{}.{} ms", millis, (mics % 1000) / 100)
    } else if millis < 1000 {
        format!("{} ms", millis)
    } else {
        format!("{}.{:0>2} s", secs, (millis % 1000) / 10)
    } 
}

pub fn pretty_print_time(nanos: u128, end: &str) {
    print!("{}{}", pretty_format_time(nanos), end)
}

pub fn print_table(titles: Vec<String>, rows: Vec<(String, Vec<String>)>) {
    let n = rows[0].1.len();
    let mut widths = vec![0; n];
    for (i, title) in titles.iter().enumerate() {
        widths[i] = title.len()
    }
    let mut name_width = 6;
    for (name, data) in rows.iter() {
        for (i, datum) in data.iter().enumerate() {
            if datum.len() > widths[i] {
                widths[i] = datum.len()
            }
        }
        if name.len() >= name_width {
            name_width = name.len() + 1;
        }
    }
    print!("{:^width$}", " name ", width = name_width);
    for i in 0..n {
        print!("|{:^width$}", titles[i], width = widths[i]);
    }
    println!();
    print!("{:->width$}", "-", width = name_width);
    for i in 0..n {
        print!("+{:->width$}", "-", width = widths[i]);
    }
    println!();
    for (name, data) in rows.iter() {
        print!("{:>width$}", name, width = name_width);
        for (i, datum) in data.iter().enumerate() {
            print!("|{:^width$}", datum, width = widths[i]);
        }
        println!();
    }
}