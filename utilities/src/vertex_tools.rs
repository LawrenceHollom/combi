use std::cmp::Ordering;
use std::fmt;
use std::fmt::Debug;
use std::ops::*;
use std::slice::*;

use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;

use crate::Order;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Vertex(usize);

#[derive(Clone, Debug)]
pub struct VertexVec<T: Debug + Clone> {
    vec: Vec<T>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VertexSet {
    verts: u128,
    n: Order,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BigVertexSet {
    verts: Vec<u128>,
    n: Order,
}

#[derive(Clone, PartialEq, Eq)]
pub struct VertexSetIterator {
    verts: VertexSet,
    i: Vertex,
}

#[derive(Clone, PartialEq, Eq)]
pub struct ReverseVertexSetIterator {
    verts: VertexSet,
    i: Vertex,
    finished: bool,
}

#[derive(Clone, PartialEq, Eq)]
pub struct BigVertexSetIterator<'a> {
    verts: &'a BigVertexSet,
    i: Vertex,
}

#[derive(Clone, Debug)]
pub struct VertexSetVec<T: Debug + Clone> {
    vec: Vec<T>,
    n: Order
}

pub struct VertexPairIterator {
    curr: (Vertex, Vertex),
    n: Order,
}

pub struct VertexDirectedPairIterator {
    curr: (Vertex, Vertex),
    flipped: bool,
    n: Order,
}

pub struct VertexSubsetIterator {
    curr: u128,
    max: u128,
    n: Order,
}

impl Vertex {
    pub const ZERO: Vertex = Vertex(0);

    pub fn of_string(text: &str) -> Vertex {
        Vertex(text.trim().parse().unwrap())
    }

    pub fn of_usize(x: usize) -> Vertex {
        Vertex(x)
    }

    pub fn incr(&self) -> Vertex {
        Vertex(self.0 + 1)
    }

    pub fn incr_inplace(&mut self) {
        self.0 += 1;
    }

    /**
     * Increase the degree by 1, modulo the given Order
     */
    pub fn incr_wrap(&self, n: Order) -> Vertex {
        Vertex((self.0 + 1) % n.to_usize())
    }

    pub fn incr_wrap_inplace(&mut self, n: Order) {
        self.0 = (self.0 + 1) % n.to_usize();
    }

    pub fn incr_by(&self, x: usize) -> Vertex {
        Vertex(self.0 + x)
    }

    pub fn incr_by_order(&self, n: Order) -> Vertex {
        Vertex(self.0 + n.to_usize())
    }

    pub fn decr(&self) -> Vertex {
        Vertex(self.0 - 1)
    }

    pub fn decr_inplace(&mut self) {
        self.0 -= 1;
    }

    pub fn decr_wrap(&self, n: Order) -> Vertex {
        Vertex((self.0 + n.to_usize() - 1) % n.to_usize())
    }

    pub fn div(&self, denom: usize) -> Vertex {
        Vertex(self.0 / denom)
    }

    pub fn div_inplace(&mut self, denom: usize) {
        self.0 /= denom
    }

    pub fn rem(&self, denom: usize) -> Vertex {
        Vertex(self.0 % denom)
    }

    pub fn xor(&self, other: Vertex) -> Vertex {
        Vertex(self.0 ^ other.0)
    }

    pub fn encode_with(&self, other: Vertex) -> usize {
        fn tri(x: usize) -> usize { (x * (x - 1)) / 2 }
        if self.0 > other.0 {
            tri(self.0) + other.0
        } else {
            tri(other.0) + self.0
        }
    }

    pub fn is_n(&self, n: Order) -> bool {
        n.to_usize() == self.0
    }

    pub fn less_than(&self, n: Order) -> bool {
        self.0 < n.to_usize()
    }

    pub fn as_fraction_of(&self, n: Order) -> f64 {
        (self.0 as f64) / (n.to_usize() as f64)
    }

    pub fn is_zero(&self) -> bool {
        self.0 == 0
    }

    pub fn is_max_less_one(&self, n: Order) -> bool {
        n.to_usize() == self.0 + 1
    }

    pub fn iter_from(&self, n: Order) -> impl Iterator<Item = Vertex> {
        n.iter_verts().skip(self.0)
    }

    pub fn iter_from_to(&self, other: Vertex) -> impl Iterator<Item = Vertex> {
        (self.0..other.0).map(|x| Vertex::of_usize(x))
    }

    pub fn iter_from_to_incl(&self, other: Vertex) -> impl Iterator<Item = Vertex> {
        (self.0..=other.0).map(|x| Vertex::of_usize(x))
    }

    pub fn num_verts_after(&self, n: Order) -> usize {
        n.to_usize() - self.0 - 1
    }

    pub fn to_binary_string(&self) -> String {
        format!("{:04b}", self.0)
    }
}

impl VertexPairIterator {
    pub fn new(n: Order) -> VertexPairIterator {
        VertexPairIterator { curr: (Vertex(0), Vertex(1)), n }
    }
}

impl Iterator for VertexPairIterator {
    type Item = (Vertex, Vertex);

    fn next(&mut self) -> Option<Self::Item> {
        if self.n.at_most(1) {
            return None
        }
        let i = self.curr.0;
        let j = self.curr.1;

        if j > self.n.to_max_vertex() {
            None
        } else if j == self.n.to_max_vertex() {
            let k = i.incr();
            self.curr = (k, k.incr());
            Some((i, j))
        } else {
            self.curr = (i, j.incr());
            Some((i, j))
        }
    }
}

impl VertexDirectedPairIterator {
    pub fn new(n: Order) -> VertexDirectedPairIterator {
        VertexDirectedPairIterator { 
            curr: (Vertex(0), Vertex(1)), 
            flipped: false, 
            n,
        }
    }
}

impl Iterator for VertexDirectedPairIterator {
    type Item = (Vertex, Vertex);

    fn next(&mut self) -> Option<Self::Item> {
        if self.n.at_most(1) {
            return None
        }
        let i = self.curr.0;
        let j = self.curr.1;

        if j > self.n.to_max_vertex() {
            None
        } else if !self.flipped {
            // Give the edge the standard way around first, and then worry
            // about iterating later.
            self.flipped = true;
            Some((i, j))
        } else if j == self.n.to_max_vertex() {
            let k = i.incr();
            self.curr = (k, k.incr());
            self.flipped = false;
            Some((j, i))
        } else {
            self.curr = (i, j.incr());
            self.flipped = false;
            Some((j, i))
        }
    }
}

impl <T: Debug + Clone> VertexVec<T> {
    pub fn new_fn(n: Order, f: impl Fn(Vertex) -> T) -> VertexVec<T> {
        VertexVec { vec: n.iter_verts().map(|x| f(x)).collect() }
    }

    pub fn new(n: Order, t: &T) -> VertexVec<T> {
        VertexVec { vec: vec![t.to_owned(); n.to_usize()] }
    }

    pub fn of_vec(vec: Vec<T>) -> VertexVec<T> {
        VertexVec { vec }
    }

    pub fn len(&self) -> Order {
        Order::of_usize(self.vec.len())
    }

    pub fn set(&mut self, v: Vertex, value: T) {
        self.vec[v.0] = value;
    }

    pub fn get(&self, v: Vertex) -> Option<&T> {
        self.vec.get(v.0)
    }

    pub fn arg_max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<Vertex> {
        let mut best_vert = None;
        let mut max = min;

        for (i, val) in self.vec.iter().enumerate() {
            if cmp(val, &max) == Ordering::Greater {
                max = val;
                best_vert = Some(Vertex(i));
            }
        }

        best_vert
    }

    pub fn max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<&T> {
        self.arg_max(min, cmp).map(|x| &self[x])
    }

    pub fn iter(&self) -> Iter<T> {
        self.vec.iter()
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (Vertex, &T)> {
        self.vec.iter().enumerate().map(|(i, v)| (Vertex::of_usize(i), v))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.vec.iter_mut()
    }

    pub fn iter_mut_enum(&mut self) -> impl Iterator<Item = (Vertex, &mut T)>{
        self.vec.iter_mut().enumerate().map(|(i, t)| (Vertex::of_usize(i), t))
    }

    pub fn shuffle(&mut self, rng: &mut ThreadRng) {
        self.vec.shuffle(rng)
    }

    pub fn sort(&mut self, compare: fn(&T, &T) -> Ordering) {
        self.vec.sort_by(compare);
    }

    pub fn contains(&self, other: &T, eq: fn(&T, &T) -> bool) -> bool {
        for t in self.vec.iter() {
            if eq(t, other) {
                return true;
            }
        }
        false
    }

    pub fn push(&mut self, t: T) {
        self.vec.push(t)
    }

    pub fn drop_vertices(&self) -> &Vec<T> {
        &self.vec
    }

    pub fn println(&self) {
        println!("{:?}", self.vec);
    }

    pub fn to_vec(&self) -> Vec<T> {
        self.vec.to_owned()
    }

    pub fn to_vec_of_strings(&self) -> VertexVec<String> {
        self.iter().map(|x| format!("{:?}", x)).collect::<VertexVec<String>>()
    }
}

pub fn print_vertex_table(rows: Vec<(&str, VertexVec<String>)>) {
    let n = rows[0].1.len();
    let titles = n.iter_verts().map(|v| v.to_string()).collect::<Vec<String>>();
    let new_rows = rows.iter()
        .map(|(name, data)| (name.to_string(), data.to_vec()))
        .collect::<Vec<(String, Vec<String>)>>();
    crate::print_table(titles, new_rows)
}

impl <T: Debug + Clone> VertexSetVec<T> {
    pub fn new(n: Order, t: &T) -> VertexSetVec<T> {
        VertexSetVec { vec: vec![t.to_owned(); 1 << n.to_usize()], n }
    }

    pub fn set(&mut self, v: VertexSet, value: T) {
        self.vec[v.to_usize()] = value;
    }

    pub fn get(&self, v: VertexSet) -> &T {
        &self.vec[v.to_usize()]
    }

    pub fn arg_max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<VertexSet> {
        let mut best_set = None;
        let mut max = min;

        for (i, val) in self.vec.iter().enumerate() {
            if cmp(val, &max) == Ordering::Greater {
                max = val;
                best_set = Some(VertexSet::of_int(i as u128, self.n));
            }
        }

        best_set
    }

    pub fn max(&self, min: &T, cmp: fn(&T, &T) -> Ordering) -> Option<&T> {
        self.arg_max(min, cmp).map(|x| &self[x])
    }

    pub fn iter(&self) -> Iter<T> {
        self.vec.iter()
    }

    pub fn iter_enum(&self) -> impl Iterator<Item = (VertexSet, &T)> {
        self.vec.iter().enumerate().map(|(i, v)| (VertexSet::of_int(i as u128, self.n), v))
    }

    pub fn iter_mut(&mut self) -> IterMut<T> {
        self.vec.iter_mut()
    }

    pub fn iter_mut_enum(&mut self) -> impl Iterator<Item = (VertexSet, &mut T)>{
        self.vec.iter_mut().enumerate().map(|(i, t)| (VertexSet::of_int(i as u128, self.n), t))
    }

    pub fn shuffle(&mut self, rng: &mut ThreadRng) {
        self.vec.shuffle(rng)
    }

    pub fn sort(&mut self, compare: fn(&T, &T) -> Ordering) {
        self.vec.sort_by(compare);
    }

    pub fn contains(&self, other: &T, eq: fn(&T, &T) -> bool) -> bool {
        self.vec.iter().any(|t| eq(t, other))
    }

    pub fn drop_vertices(&self) -> &Vec<T> {
        &self.vec
    }

    pub fn println(&self) {
        println!("{:?}", self.vec);
    }
}

impl <T: Debug + Clone> FromIterator<T> for VertexVec<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut vec = vec![];

        for i in iter {
            vec.push(i);
        }

        VertexVec { vec }
    }
}

impl VertexSet {
    pub fn new(n: Order) -> VertexSet {
        VertexSet{ verts: 0, n }
    }

    pub fn everything(n: Order) -> VertexSet {
        VertexSet{ verts: (1 << n.to_usize()) - 1, n }
    }

    pub fn of_int(set: u128, n: Order) -> VertexSet {
        VertexSet{ verts: set, n }
    }

    pub fn of_vert(n: Order, v: Vertex) -> VertexSet {
        VertexSet{ verts: 1 << v.0, n }
    }

    pub fn of_vec(n: Order, vs: &Vec<Vertex>) -> VertexSet {
        let mut set = VertexSet::new(n);
        for v in vs.iter() {
            set.add_vert(*v)
        }
        set
    }

    pub fn to_int(&self) -> u128 {
        self.verts
    }

    pub fn to_usize(&self) -> usize {
        self.verts as usize
    }

    pub fn to_vec(&self) -> VertexVec<bool> {
        let mut out = VertexVec::new(self.n, &false);
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                out[v] = true
            }
        }
        out
    }

    pub fn add_vert(&mut self, v: Vertex) {
        self.verts |= 1 << v.0;
    }

    pub fn add_vert_immutable(&self, v: Vertex) -> VertexSet {
        VertexSet{ verts: self.verts | 1 << v.0, n: self.n }
    }

    pub fn remove_vert(&mut self, v: Vertex) {
        self.verts &= !(1 << v.0)
    }

    pub fn remove_vert_immutable(&self, v: Vertex) -> VertexSet {
        VertexSet{
            verts: self.verts & !(1 << v.0),
            n: self.n
        }
    }

    pub fn add_all(&mut self, vs: VertexSet) {
        self.verts |= vs.verts;
    }

    pub fn remove_all(&mut self, vs: VertexSet) {
        self.verts &= !vs.verts;
    }

    /**
     * Could also be called remove_all_immutable
     */
    pub fn setminus(&self, vs: VertexSet) -> VertexSet {
        VertexSet{ verts: self.verts & !vs.verts, n: self.n }
    }

    pub fn union(&self, other: &VertexSet) -> VertexSet {
        VertexSet{ verts: self.verts | other.verts, n: self.n }
    }

    /**
     * Return a VertexSet consisting of the intersection of self with other.
     */
    pub fn inter(&self, other: &VertexSet) -> VertexSet {
        VertexSet{ verts: self.verts & other.verts, n: self.n }
    }

    /**
     * Returns the set of vertices present in exactly one of self and other.
     */
    pub fn xor(&self, other: &VertexSet) -> VertexSet {
        VertexSet { verts: self.verts ^ other.verts, n: self.n }
    }

    /**
     * Inverts the vertices in the set (up to n).
     */
    pub fn not(&self) -> VertexSet {
        let everything = (1 << self.n.to_usize()) - 1;
        VertexSet { verts: everything & !self.verts, n: self.n }
    }

    pub fn is_empty(&self) -> bool {
        self.verts == 0
    }

    pub fn is_nonempty(&self) -> bool {
        let everything = (1 << self.n.to_usize()) - 1;
        self.verts & everything != 0
    }

    /**
     * For use when manually iterating. Will return false when this VertexSet has
     * been incremented to 1 << n or beyond.
     */
    pub fn is_in_range(&self) -> bool {
        self.verts < (1 << self.n.to_usize())
    }

    /**
     * Is every vertex from 0 to n-1 in the set?
     */
    pub fn is_everything(&self) -> bool {
        let everything = (1 << self.n.to_usize()) - 1;
        self.verts & everything == everything
    }

    pub fn has_vert(&self, v: Vertex) -> bool {
        (self.verts >> v.0) % 2 == 1
    }

    /** 
     * Swap whether vertices u and v are in the set.
     */
    pub fn swap(&self, u: Vertex, v: Vertex) -> VertexSet {
        if self.has_vert(u) == self.has_vert(v) {
            *self
        } else if self.has_vert(u) {
            self.remove_vert_immutable(u).add_vert_immutable(v)
        } else {
            self.remove_vert_immutable(v).add_vert_immutable(u)
        }
    }

    /**
     * Apply a permutation of the vertices.
     */
    pub fn permute(&self, sigma: &VertexVec<Vertex>) -> VertexSet {
        let mut out = VertexSet::new(self.n);
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                out.add_vert(sigma[v])
            }
        }
        out
    }

    pub fn size(&self) -> usize {
        let mut sta = self.verts;
        let mut size = 0;
        while sta > 0 {
            size += sta % 2;
            sta /= 2;
        }
        size as usize
    }

    /**
     * Returns the Vertex in self with the smallest index, or None if self is empty.
     */
    pub fn get_first_element(&self) -> Option<Vertex> {
        let mut sta = self.verts;
        if sta == 0 {
            None
        } else {
            let mut vert = Vertex::ZERO;
            while sta % 2 == 0 {
                vert.incr_inplace();
                sta >>= 1;
            }
            Some(vert)
        }
    }

    /**
     * Returns the vertex in self with the largest index, or None if self is empty.
     * This is very fast due to ilog2
     */
    pub fn get_biggest_element(&self) -> Option<Vertex> {
        if self.verts == 0 {
            None
        } else {
            Some(Vertex::of_usize(self.verts.ilog2() as usize))
        }
    }

    /**
     * Only use this if you're really sure that you want to (e.g. you're manually iterating.)
     * Increments to the next vertex set in the binary ordering.
     */
    pub fn incr_inplace(&mut self) {
        self.verts += 1;
    }

    /**
     * Changes self to the minimal VertexSet (in the binary ordering) after self which does
     * not contain the vertex v. This is done by blanking out everything strictly after v,
     * and then adding v to the raw vertex set.
     * If v is not in the set, then nothing is done. (Beware of infinite loops if calling this!)
     */
    pub fn incr_inplace_to_remove_vertex(&mut self, v: Vertex) {
        if self.has_vert(v) {
            let mask = (1 << self.n.to_usize()) - (1 << v.0);
            self.verts &= mask;
            self.verts += 1 << v.0;
        }
    }

    pub fn iter(&self) -> VertexSetIterator {
        VertexSetIterator::new(*self)
    }

    /**
     * Manually iterates in the reverse order. Just as fast as iter. Probably.
     */
    pub fn iter_rev(&self) -> ReverseVertexSetIterator {
        ReverseVertexSetIterator::new(*self)
    }

    pub fn print(&self) {
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                print!("1");
            } else {
                print!("0");
            }
        }
        println!();
    }

    pub fn print_hum(&self) {
        print!("{{ ");
        let mut is_first = true;
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                if is_first {
                    print!("{}", v);
                    is_first = false
                } else {
                    print!(", {}", v);
                }
            }
        }
        println!(" }}");

    }
}

impl BigVertexSet {
    const BITS: usize = 128;
    
    pub fn new(n: Order) -> BigVertexSet {
        let num_pars: usize = (n.to_usize() + Self::BITS - 1) / Self::BITS;
        BigVertexSet{ verts: vec![0; num_pars], n }
    }

    pub fn of_vec(n: Order, vs: &Vec<Vertex>) -> BigVertexSet {
        let mut set = BigVertexSet::new(n);
        for v in vs.iter() {
            set.add_vert(*v)
        }
        set
    }

    pub fn of_filter(filter: &VertexVec<bool>) -> BigVertexSet { 
        let mut set = BigVertexSet::new(filter.len());
        for (v, is_in_set) in filter.iter_enum() {
            if *is_in_set {
                set.add_vert(v);
            }
        }
        set
    }

    pub fn of_vertex_set(vs: VertexSet) -> BigVertexSet {
        BigVertexSet { 
            verts: vec![vs.verts], 
            n: vs.n 
        }
    }

    pub fn to_vec(&self) -> VertexVec<bool> {
        let mut out = VertexVec::new(self.n, &false);
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                out[v] = true
            }
        }
        out
    }

    pub fn add_vert(&mut self, v: Vertex) {
        let part = v.0 / Self::BITS;
        self.verts[part] |= 1 << (v.0 % Self::BITS);
    }

    pub fn remove_vert(&mut self, v: Vertex) {
        let part = v.0 / Self::BITS;
        self.verts[part] &= !(1 << (v.0 % Self::BITS))
    }

    pub fn add_all(&mut self, vs: BigVertexSet) {
        for (i, part) in self.verts.iter_mut().enumerate() {
            *part |= vs.verts[i];
        }
    }

    pub fn remove_all(&mut self, vs: BigVertexSet) {
        for (i, part) in self.verts.iter_mut().enumerate() {
            *part &= !vs.verts[i];
        }
    }

    pub fn has_vert(&self, v: Vertex) -> bool {
        let part = v.0 / Self::BITS;
        (self.verts[part] >> (v.0 % Self::BITS)) % 2 == 1
    }

    pub fn is_empty(&self) -> bool {
        for part in self.verts.iter() {
            if *part != 0 {
                return false
            }
        }
        return true
    }

    pub fn iter(&self) -> BigVertexSetIterator {
        BigVertexSetIterator::new(self)
    }
    
    pub fn print(&self) {
        for v in self.n.iter_verts() {
            if self.has_vert(v) {
                print!("1");
            } else {
                print!("0");
            }
        }
        println!();
    }
}

impl VertexSetIterator {
    pub fn new(verts: VertexSet) -> VertexSetIterator {
        VertexSetIterator { 
            verts, 
            i: Vertex::ZERO 
        }
    }
}

impl ReverseVertexSetIterator {
    pub fn new(verts: VertexSet) -> ReverseVertexSetIterator {
        ReverseVertexSetIterator { 
            verts, 
            i: verts.n.to_max_vertex(),
            finished: false 
        }
    }
}

impl BigVertexSetIterator<'_> {
    pub fn new(verts: &BigVertexSet) -> BigVertexSetIterator {
        BigVertexSetIterator { 
            verts, 
            i: Vertex::ZERO 
        }
    }
}

impl fmt::Display for Vertex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Debug + Clone> Index<Vertex> for VertexVec<T> {
    type Output = T;

    fn index(&self, index: Vertex) -> &Self::Output {
        &self.vec[index.0]
    }
}

impl<T: Debug + Clone> IndexMut<Vertex> for VertexVec<T> {
    fn index_mut(&mut self, index: Vertex) -> &mut Self::Output {
        &mut self.vec[index.0]
    }
}

impl<T: Debug + Clone> Index<VertexSet> for VertexSetVec<T> {
    type Output = T;

    fn index(&self, index: VertexSet) -> &Self::Output {
        &self.vec[index.to_usize()]
    }
}

impl<T: Debug + Clone> IndexMut<VertexSet> for VertexSetVec<T> {
    fn index_mut(&mut self, index: VertexSet) -> &mut Self::Output {
        &mut self.vec[index.to_usize()]
    }
}

impl fmt::Display for VertexSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.verts)
    }
}

impl Iterator for ReverseVertexSetIterator {
    type Item = Vertex;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }
        while !self.i.is_zero() && !self.verts.has_vert(self.i) {
            self.i.decr_inplace();
        }
        if self.i.is_zero() {
            self.finished = true;
            if self.verts.has_vert(self.i) {
                Some(self.i)
            } else {
                None
            }
        } else {
            let out = self.i;
            self.i.decr_inplace();
            Some(out)
        }
    }
}

impl Iterator for VertexSetIterator {
    type Item = Vertex;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.i.is_n(self.verts.n) && !self.verts.has_vert(self.i) {
            self.i.incr_inplace();
        }
        if self.i.is_n(self.verts.n) {
            None
        } else {
            let out = self.i;
            self.i.incr_inplace();
            Some(out)
        }
    }
}

impl Iterator for BigVertexSetIterator<'_> {
    type Item = Vertex;

    fn next(&mut self) -> Option<Self::Item> {
        while !self.i.is_n(self.verts.n) && !self.verts.has_vert(self.i) {
            self.i.incr_inplace();
        }
        if self.i.is_n(self.verts.n) {
            None
        } else {
            let out = self.i;
            self.i.incr_inplace();
            Some(out)
        }
    }
}

impl VertexSubsetIterator {
    pub fn new(n: Order) -> VertexSubsetIterator {
        VertexSubsetIterator { curr: 0, max: 1_u128 << n.to_usize(), n }
    }
}

impl Iterator for VertexSubsetIterator {
    type Item = VertexSet;

    fn next(&mut self) -> Option<VertexSet> {
        if self.curr < self.max {
            let out = VertexSet::of_int(self.curr, self.n);
            self.curr += 1;
            Some(out)
        } else {
            None
        }
    }
}