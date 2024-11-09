use std::ops::{Add, AddAssign, Div, Mul, Sub, SubAssign};

use image::{ImageBuffer, ImageReader, Rgba};
use rand::{rngs::ThreadRng, thread_rng, Rng};

use crate::entity::graph::*;
use utilities::vertex_tools::*;

#[derive(Debug, Clone, Copy)]
struct Point {
    x: f64,
    y: f64,
}

#[derive(Clone)]
struct Embedding {
    positions: VertexVec<Point>,
    delta: f64,
    repulsion_scale: f64,
}

struct Printer {
    node_bg: ImageBuffer<Rgba<u8>, Vec<u8>>,
    numbers: ImageBuffer<Rgba<u8>, Vec<u8>>,
    width: u32,
    height: u32,
}

impl Point {
    pub const ZERO: Point = Point { x: 0.0, y: 0.0 };

    pub fn new_random(rng: &mut ThreadRng) -> Point {
        Point {
            x: rng.gen_range(0.0..1.0),
            y: rng.gen_range(0.0..1.0),
        }
    }

    pub fn to_pixel_coords(&self, width: u32, height: u32) -> Point {
        let x = (width as f64) * (self.x + 0.15) / 1.3;
        let y = (height as f64) * (self.y + 0.15) / 1.3;
        Point { x, y }
    }

    pub fn to_int_coords(&self, width: u32, height: u32) -> (u32, u32) {
        let px = self.to_pixel_coords(width, height);
        (px.x.round() as u32, px.y.round() as u32)
    }

    pub fn length(&self) -> f64 {
        ((self.x * self.x) + (self.y * self.y)).sqrt()
    }

    /**
     * Scales the point so that the given rectangle is mapped onto [0, 1]^2
     */
    pub fn scale_to(&self, min_x: f64, max_x: f64, min_y: f64, max_y: f64) -> Point {
        Point {
            x: (self.x - min_x) / (max_x - min_x),
            y: (self.y - min_y) / (max_y - min_y),
        }
    }
}

impl Embedding {
    pub fn new(g: &Graph, rng: &mut ThreadRng) -> Embedding {
        let mut positions = VertexVec::new(g.n, &Point::ZERO);
        for pos in positions.iter_mut() {
            *pos = Point::new_random(rng);
        }
        Embedding {
            positions,
            delta: 0.05,
            repulsion_scale: 0.5 / (g.n.to_usize() as f64),
        }
    }

    fn get_attraction(&self, u: Vertex, v: Vertex, is_edge: bool) -> f64 {
        let dist = (self.get(u) - self.get(v)).length();
        let repulsion = self.repulsion_scale / (dist * dist);
        let attraction = if is_edge { dist } else { 0.0 };
        attraction - repulsion
    }

    fn get_energy_of_pair(&self, u: Vertex, v: Vertex, is_edge: bool) -> f64 {
        let dist = (self.get(u) - self.get(v)).length();
        let repulsion_e = self.repulsion_scale / dist;
        let attraction_e = if is_edge { dist * dist } else { 0.0 };
        attraction_e + repulsion_e
    }

    /** 
     * Shuffle stuff around to try and find the perfect embedding. 
     * The key idea is that edges should attract, and non-edges repel, and there should
     * always be stuff on each boundary.
     **/
    pub fn optimise(&mut self, g: &Graph) {
        for _rep in 0..200 {
            // Edges attract, nodes repel.
            let mut new_positions = VertexVec::new(g.n, &Point::ZERO);
            for (v, pos) in self.positions.iter_enum() {
                new_positions[v] = *pos;
            }

            for (u, v) in g.iter_pairs() {
                // positive force => move points together.
                let force = self.get_attraction(u, v, g.adj[u][v]);
                let offset = self.get(u) - self.get(v);
                new_positions[u] -= offset * (force * self.delta);
                new_positions[v] += offset * (force * self.delta);
            }

            // Scale to the full size.
            let mut min_x = 1.0;
            let mut max_x = 0.0;
            let mut min_y = 1.0;
            let mut max_y = 0.0;

            for pos in new_positions.iter() {
                if pos.x < min_x {
                    min_x = pos.x;
                }
                if pos.x > max_x {
                    max_x = pos.x;
                }
                if pos.y < min_y {
                    min_y = pos.y;
                }
                if pos.y > max_y {
                    max_y = pos.y;
                }
            }

            for (v, pos) in new_positions.iter_enum() {
                self.positions[v] = pos.scale_to(min_x, max_x, min_y, max_y);
            }
        }
    }

    pub fn get_energy(&self, g: &Graph) -> f64 {
        let mut energy = 0.0;
        for (u, v) in g.iter_pairs() {
            energy += self.get_energy_of_pair(u, v, g.adj[u][v])
        }
        energy
    }

    pub fn get(&self, v: Vertex) -> Point {
        self.positions[v]
    }
}

impl Printer {
    const WHITE: Rgba<u8> = Rgba([255, 255, 255, 255]);
    const BLACK: Rgba<u8> = Rgba([0, 0, 0, 255]);

    pub fn new() -> Printer {
        Printer {
            node_bg: read_image( "base"),
            numbers: read_image("numbers"),
            width: 1000,
            height: 1000,
        }
    }

    fn draw_line(&self, imgbuf: &mut ImageBuffer<Rgba<u8>, Vec<u8>>, start_point: Point, end_point: Point) {
        let start = start_point.to_pixel_coords(self.width, self.height);
        let end = end_point.to_pixel_coords(self.width, self.height);
        let length = (end - start).length();
        let step = (end - start) / (length * 5.0);
        let num_steps = (length * 5.0) as usize;
        let mut pos = start;
        for _i in 0..num_steps {
            pos = pos + step;
            let x = pos.x.round() as u32;
            let y = pos.y.round() as u32;
            *imgbuf.get_pixel_mut(x, y) = Self::BLACK;
        }
    }

    fn draw_img_at_point(&self, imgbuf: &mut ImageBuffer<Rgba<u8>, Vec<u8>>, img: &ImageBuffer<Rgba<u8>, Vec<u8>>, x: u32, y: u32, atlas_x: u32, atlas_size: u32) {
        let sx = x - img.width() / (2 * atlas_size);
        let sy = y - img.height() / 2;
        let width = img.width();
        let min_x = (width / atlas_size) * atlas_x;
        let max_x = min_x + (width / atlas_size);
        for (x, y, px) in img.enumerate_pixels() {
            if x >= min_x && x < max_x && px.0[3] >= 1 {
                imgbuf.put_pixel(sx + x - min_x, sy + y, *px);
            }
        }
    }

    fn draw_node(&self, imgbuf: &mut ImageBuffer<Rgba<u8>, Vec<u8>>, name: String, point: Point) {
        let (x, y) = point.to_int_coords(self.width, self.height);
        self.draw_img_at_point(imgbuf, &self.node_bg, x, y, 0, 1);
        let name_len = name.len() as u32;
        for (i, c) in name.chars().enumerate() {
            let digit_x = x + (10 * i as u32) - ((name_len - 1) * 5);
            let digit = (c as u8 - '0' as u8) as u32;
            self.draw_img_at_point(imgbuf, &self.numbers, digit_x, y, digit, 10);
        }
    }

    pub fn print_graph(&self, g: &Graph, embedding: &Embedding, filename: &str) {
        let mut imgbuf: ImageBuffer<Rgba<u8>, Vec<u8>> = image::ImageBuffer::new(self.width, self.height);

        for px in imgbuf.pixels_mut() {
            *px = Self::WHITE;
        }

        // Draw all the edges.
        for e in g.iter_edges() {
            self.draw_line(&mut imgbuf, embedding.get(e.fst()), embedding.get(e.snd()))
        }

        // Draw all the node blobs and numbers
        for v in g.iter_verts() {
            self.draw_node(&mut imgbuf, v.to_string(), embedding.get(v))
        }

        save_buffer(&imgbuf, filename);
    }
}

pub fn print_graph(g: &Graph) {
    let printer = Printer::new();
    let mut rng = thread_rng();

    let mut optimal_embedding = Embedding::new(g, &mut rng);
    let mut min_energy = f64::MAX;

    // Attempt 100 embeddings, and pick the one with the lowest energy
    for _i in 0..100 {
        let mut embedding = Embedding::new(g, &mut rng);
        embedding.optimise(g);
        let energy = embedding.get_energy(g);
        if energy < min_energy {
            min_energy = energy;
            optimal_embedding = embedding.to_owned();
        }
    }
    printer.print_graph(g, &optimal_embedding, "output");
}

pub fn read_image(filename: &str) -> ImageBuffer<Rgba<u8>, Vec<u8>> {
    let filename = format!("images/{}.png", filename);
    let img = ImageReader::open(filename).unwrap().decode().unwrap();
    img.as_rgba8().unwrap().clone()
}

pub fn save_buffer(imgbuf: &ImageBuffer<image::Rgba<u8>, Vec<u8>>, filename: &str) {
    let filename = format!("images/{}.png", filename);
    imgbuf.save(filename).unwrap()
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