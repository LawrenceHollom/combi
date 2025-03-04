use rand::{rngs::ThreadRng, thread_rng, Rng};
use utilities::vertex_tools::VertexVec;
use core::fmt;
use std::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};

use crate::entity::graph::*;

#[derive(Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    pub const ZERO: Point = Point { x: 0.0, y: 0.0 };

    pub fn new_random(rng: &mut ThreadRng) -> Point {
        Point {
            x: rng.gen_range(-3.0..3.0),
            y: rng.gen_range(0.01..2.0),
        }
    }

    pub fn euc_length(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }

    pub fn euc_dist_to(&self, other: Point) -> f64 {
        ((self.x - other.x) * (self.x - other.x) + (self.y - other.y) * (self.y - other.y)).sqrt()
    }

    pub fn hyp_dist_to(&self, other: Point) -> f64 {
        2.0 * (self.euc_dist_to(other) / (2.0 * (self.y * other.y).sqrt())).asinh()
    }

    pub fn rotate90(&self) -> Point {
        Point {
            x: -self.y,
            y: self.x,
        }
    }

    /**
     * Returns the centre of the Euclidean circle which represents the
     * hyperbolic line between self and other.
     */
    fn get_centre_with(&self, other: Point) -> Point {
        let mid = (*self + other) / 2.0;
        let dir = (other - *self).rotate90();
        let t = - mid.y / dir.y;
        Point{ x: mid.x + t * dir.x, y: 0.0 }

    }

    /**
     * Returns the "normalised" vector in the direction of other
     */
    pub fn hyp_direction_to(&self, other: Point) -> Point {
        let centre = self.get_centre_with(other);
        let mut dir = (*self - centre).rotate90();
        if dir.x * (other.x - self.x) < 0.0 {
            dir = dir * (-1.0);
        }
        dir * (2.0 * self.y * (0.5_f64).sinh() / dir.euc_length())
    }
}

const DELTA: f64 = 0.1;

/**
 * Move each point by a little bit depending on what the edges want.
 */
fn wiggle_points(g: &Graph, d: f64, points: &mut VertexVec<Point>) -> f64 {
    /*for point in points.iter() {
        print!("{:?} \t", point);
    }
    println!();*/
    let mut new_points = VertexVec::new(g.n, &Point::ZERO);
    let mut max_force = 0.0;
    for (v, point) in points.iter_enum() {
        new_points[v] = *point;
        let mut total_force = Point::ZERO;
        for u in g.adj_list[v].iter() {
            let force = points[v].hyp_dist_to(points[*u]) - d;
            total_force += points[v].hyp_direction_to(points[*u]) * (force);
        }
        new_points[v] += total_force * DELTA;
        let abs_force = total_force.euc_length();
        if abs_force >= max_force {
            max_force = abs_force
        }
    }
    for (v, point) in points.iter_mut_enum() {
        *point = new_points[v]
    }
    max_force
}

fn is_d_distance_embedding(g: &Graph, d: f64, points: &VertexVec<Point>) -> bool {
    for (u, v) in g.iter_pairs() {
        if g.adj[u][v] && (points[u].hyp_dist_to(points[v]) - d).abs() > 0.001 {
            // This is an edge but the points are not d apart
            // println!("Edge fail");
            return false
        }
        if !g.adj[u][v] && points[u].hyp_dist_to(points[v]) < 0.005 {
            // This is a non-edge but the points are very close
            // println!("Close fail");
            return false
        }
    }
    true
}

/**
 * Attempt to embed g as a d-distance graph in the hyperbolic plane from a random start.
 * Uses the upper-half plane model.
 */
fn attempt_embed_d_distance(g: &Graph, rng: &mut ThreadRng, d: f64) -> bool {
    let mut points = VertexVec::new(g.n, &Point::ZERO);
    for p in points.iter_mut() {
        *p = Point::new_random(rng);
    }

    while wiggle_points(g, d, &mut points) > 0.0005 { };

    is_d_distance_embedding(g, d, &points)
}

/**
 * Tests whether g embeds as a d-distance graph in the hyperbolic plane.
 */
fn does_embed_d_distance(g: &Graph, rng: &mut ThreadRng, d: f64) -> bool {
    for i in 0..100 {
        if attempt_embed_d_distance(g, rng, d) {
            println!("Found hyperbolic embedding on attempt {}", i);
            return true
        }
    }
    false
}

pub fn does_embed_generically(g: &Graph) -> bool {
    let mut rng = thread_rng();
    for d in [1.0, 1.5, 2.0, 2.5] {
        if !does_embed_d_distance(g, &mut rng, d) {
            return false;
        }
    }
    true
}

/**
 * Test all 2^n induced subgraphs for if they have at least 2n - 2 edges.
 */
pub fn has_hereditarily_few_edges(g: &Graph) -> bool {
    for vs in g.iter_vertex_subsets() {
        let mut count = 0;
        for v in vs.iter() {
            for u in g.adj_list[v].iter() {
                if vs.has_vert(*u) {
                    count += 1;
                }
            }
        }
        if vs.size() >= 2 && vs.size() < g.n.to_usize() && count >= 4 * (vs.size() - 1) {
            // There are too many edges.
            return false
        }
    }
    true
}

impl Sub<Point> for Point {
    type Output = Point;

    fn sub(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl AddAssign<Point> for Point {
    fn add_assign(&mut self, rhs: Point) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl SubAssign<Point> for Point {
    fn sub_assign(&mut self, rhs: Point) {
        self.x -= rhs.x;
        self.y -= rhs.y;
    }
}

impl Add<Point> for Point {
    type Output = Point;

    fn add(self, rhs: Point) -> Self::Output {
        Point {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl Div<f64> for Point {
    type Output = Point;

    fn div(self, rhs: f64) -> Self::Output {
        Point {
            x: self.x / rhs,
            y: self.y / rhs,
        }
    }
}

impl Mul<f64> for Point {
    type Output = Point;

    fn mul(self, rhs: f64) -> Self::Output {
        Point {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl fmt::Debug for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({:.5}, {:.5})", self.x, self.y)
    }
}